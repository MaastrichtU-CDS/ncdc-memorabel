RPC_models <- function(df, config, model = "memory", exclude=c()) {
  vtg::log$info("Starting: Models")
  result = tryCatch({
    con <- RPostgres::dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv("PGDATABASE"),
      host = Sys.getenv("PGHOST"),
      port = Sys.getenv("PGPORT"),
      password = Sys.getenv("PGPASSWORD"),
      user = Sys.getenv("PGUSER"),
    )
    # Sub-cohort if selected
    cohort <- c()
    if ("subcohort" %in% names(config)) {
      cohort <- config[["subcohort"]]
    }

    query <- 'SELECT * FROM ncdc'
    # To select a specific cohort in the organization
    if (!(is.na(cohort)) && length(cohort) > 0) {
      cohorts_parsed <- paste(cohort, collapse=",")
      vtg::log$info(cohorts_parsed)
      query <- paste(
        "SELECT ROW_NUMBER() OVER() AS row, n.* FROM ncdc AS n LEFT JOIN PERSON AS p ON n.id = p.person_id WHERE p.care_site_id IN ('",
        cohorts_parsed,
        "')"
      )
    }
    df <- RPostgres::dbGetQuery(con, query)
    pre_summary <- summary_stats(df)
    # The dataframe will contain all the data harmonized for the cohort. The
    # variable names will be the same in all cohorts.
    # In any case, it's a best practice to validate that all columns are available
    check_names <- c("age", "sex", "education_category_3","visit_years", "p_tau", "amyloid_b_ratio_42_40", "gfap", "nfl", "priority_memory_dr_ravlt")
    missing_variables <- c()
    for (name in check_names) {
      if (!name %in% colnames(df)) {
        missing_variables <- c(name, missing_variables)
      }
    }

    if (length(missing_variables) > 0) {
      return(list(
        "error_message" = paste("Missing the following variables: ", paste(missing_variables, collapse=", "))
      ))
    }

    # Identifying the participants that need to be excluded
    # Participants will be excluded if date of birth or sex is missing.
    # Participants are also excluded if there are no duplicates of ID number (i.e., there has not been a follow_up)
    excluded <- unique(
      # df$id[is.na(df$birth_year) | is.na(df$sex) | !anyDuplicated(df$id, incomparable = FALSE, fromLast = FALSE)]
      df$id[is.na(df$birth_year) | is.na(df$sex) | !(
        duplicated(df$id, incomparable = FALSE, fromLast = FALSE) | duplicated(df$id, incomparable = FALSE, fromLast = TRUE)
      )]
    )
    
    # Selected participants
    included <- df$id[! df$id %in% excluded]
    df <- df[df$id %in% included,]

    # Pre-processing the data
    df <- preprocessing(df, model, config)

    #Follow-ups (in years)
    mutate(across(c(date, plasma), as.Date, format = "%m/%d?%Y"))
    df$difference_time <- time_length(interval(as.Date(df$date), as.Date(df$plasma)), unit = "years")
    #check that this logic checks out
    ## 1. not multiple baselines for the same ID
    ## 2. that the interval from 0 to the next FU is acceptable
    df$fu_years <- ifelse(df$difference_time >= -1 | df$difference_time <= 1, 0, df$difference_time)
    
    # Age of participant:
    # current_year <- format(Sys.Date(), "%Y")
    # Year of birth will always be available (mandatory in OMOP), age is not guarantee
    df$age_rec <- transform(df, age_rec=ifelse(is.na(age), as.numeric(format(df$date, "%Y")) - df$birth_year, age))

    #age^2
    df$age2 <- df$age_rec ^ 2
    
    # Centering age:
    df$age_cent <- df$age_rec - 50
    df$age_cent2 <- df$age_cent^2

    # Sex
    # df$sex <- recode_factor(df$sex, "male" = "1", "female" = "0")
    df$sex <- as.factor(df$sex, levels = c("0", "1"), labels = c("male", "female"))

    # Education levels 
    df$education <- factor(df$education_category_3, levels = c(0, 1, 2), labels = c("low", "medium", "high"))
    # dummy variables:
    low_edu <- ifelse(df$education == '0', 1, 0)
    high_edu <- ifelse(df$education == '2', 1, 0)

    #Memory delayed recall z-transformations
    priority_memory_dr_test <- NULL
    if (sum(!is.na(df$priority_memory_dr_ravlt)) > 0) {
      df$priority_memory_dr <- df$priority_memory_dr_ravlt
      df$priority_memory_dr_z <- (
        df$priority_memory_dr_ravlt - (10.924 + (df$age_cent * -0.073) + \ 
        (df$age_cent2 * -0.0009) + (df$sex_num * -1.197) + (df$education_low * -0.844) \
         + (df$education_high * 0.424))) / sd(df$priority_memory_dr, na.rm = TRUE)
    } else if (sum(!is.na(df$priority_memory_dr_lm)) > 0)) {
      df$priority_memory_dr_z <-  scale(df$priority_memory_dr_lm)
      df$priority_memory_dr <- df$priority_memory_dr_lm
    } else {
      return(list(
        "error_message" = paste("Delayed recall test not found"))
      ))
    }

    summary_post <- summary_stats(
      df,
      c(
        "p_tau", "amyloid_b_ratio_42_40", "gfap", "nfl", "priority_memory_dr",
        "priority_memory_dr_z", "age_rec", "age_cent"
      )
    )

    if (nrow(df) == 0) {
      return(list(
        "error_message" = "Empty dataset: no participants selected"
      ))
    }

    # Model testing (add model for every biomarker x cognitive measure)
    RIRS_memory_dr <- lme(priority_memory_dr_z ~ visit_years + age_rec + sex + low_edu + high_edu + p_tau + p_tau * visit_years, 
                          data = df, 
                          random = ~ visit_years | id,
                          weights = varIdent(form= ~1 | visit_years),
                          correlation = 'corSymm',
                          method = "ML", 
                          na.action = na.exclude, 
                          control = list(opt="optim")) #may need to change this if model doesn't converge
    
    # Unstructured Marginal Modal Memory delayed recall
    marginal_memory_dr <- gls(priority_memory_dr_z ~ visit_years + age2 + sex + low_edu + high_edu + p_tau + p_tau * visit_years, 
                          data = df,
                          weights = varIdent(form= ~1 | visit_years),
                          correlation = corSymm(form = ~1 | id),
                          method = "ML", 
                          na.action = na.exclude, 
                          control = list(opt="optim")) #may need to change this if model doesn't converge

    results <- list(
      "model_memory_dr" = RIRS_memory_dr,
      "model_marginal_memory_dr" = marginal_memory_dr,
      "n" = nrow(df),
      "summary" = summary_post,
      "pre_summary" = pre_summary,
      "db" = Sys.getenv("PGDATABASE")
      # warnings <- data["warn"]
    )
    return(results)
  }, error = function(e) {
    msg <- "Error while running linear models"
    vtg::log$info(msg)
    vtg::log$info(e)
    return(list(
      "error_message" = paste(msg, e, sep=" ")
    ))
  })
return(result)
}
