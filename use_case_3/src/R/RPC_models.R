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
    check_names <- c("age", "sex", "education_category_3", "p_tau", "amyloid_b_ratio_42_40", "gfap", "nfl", "priority_memory_dr_ravlt")
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
    memory_dr_test_name <- NULL
    if (sum(!is.na(df$priority_memory_dr_ravlt)) > 0) {
      memory_dr_test_name <- "priority_memory_dr_ravlt"
    } else if (sum(!is.na(df$priority_memory_dr_lm)) > 0) {
      memory_dr_test_name <- "priority_memory_dr_lm"
    } else {
      return(list(
        "error_message" = paste("Delayed recall test not found")
      ))
    }
    vtg::log$info("Cognitive test available: '{memory_dr_test_name}'")

    df_plasma <- df[!is.na(df$p_tau),]
    df_baseline <- df[!is.na(df$education_category_3),]
    df_cogn_test <- df[!is.na(df[[memory_dr_test_name]]) & df$id %in% df_plasma$id & df$id %in% df_baseline$id,] %>%
      dplyr::group_by(id, date) %>%
      dplyr::filter(abs(difftime(date, df_plasma[df_plasma$id == id,]$date_plasma)) == min(abs(difftime(date, df_plasma[df_plasma$id == id,]$date_plasma))))

    df_grouped <- merge(
      x = df_baseline[c("id", "age", "sex", "birth_year", "education_category_3")],
      y = df_plasma[c("id", "date_plasma", "p_tau")],
      by = "id"
    )
    df <- merge(
      x = df_grouped,
      y = df_cogn_test[c("id", "date", memory_dr_test_name)],
      by = "id",
      all.x = T
    )

    excluded <- unique(df$id[is.na(df$birth_year) | is.na(df$sex)])
      # df$id[is.na(df$birth_year) | is.na(df$sex) | !anyDuplicated(df$id, incomparable = FALSE, fromLast = FALSE)]

    # Selected participants
    included <- df$id[! df$id %in% excluded]
    vtg::log$info("Number of rows in the dataset: '{nrow(df)}'")
    vtg::log$info("Excluded '{length(excluded)}' participants")
    vtg::log$info("'{length(included)}' participants included in the analysis")
    print(sum(!is.na(df$priority_memory_dr_ravlt)))
    df <- df[df$id %in% included,]
    vtg::log$info("Number of rows in the dataset after exclusion: '{nrow(df)}'")

    # Pre-processing the data
    # df <- preprocessing(df, model, config)

    df %>%
      mutate(across(c(date, date_plasma), as.Date, format = "%d/%m/%Y"))
    df$difference_time <- time_length(interval(as.Date(df$date), as.Date(df$date_plasma)), unit = "years")
    print(df$difference_time)
    df$baseline <- ifelse(df$difference_time >= -1 | df$difference_time <= 1, 0) 
    baseline_df <- df %>%
      filter(baseline == 0) %>%
      select(id, date) %>%
      rename(date_baseline = date)
    df <- df %>%
      left_join(baseline_df, by = "id") %>%
      dplyr::mutate(days_since_baseline = as.numeric(difftime(date, date_baseline, units = "days")))
  
    # Age of participant:
    # current_year <- format(Sys.Date(), "%Y")
    # Year of birth will always be available (mandatory in OMOP), age is not guarantee
    df$age_rec <- ifelse(is.na(df$age), as.numeric(format(df$date, "%Y")) - df$birth_year, df$age)
    
    #Age squared:
    df$age2 <- df$age_rec^2

    # Centering age:
    df$age_cent <- df$age_rec - 50
    df$age_cent2 <- df$age_cent^2

    # Sex
    df$sex_num <- as.numeric(df$sex) + 1
    df$sex <- factor(df$sex, levels = c(0, 1), labels = c("male", "female"))

    # Education levels
    df$education <- dplyr::recode_factor(df$education_years, "1" = "0", "2" = "0",
                                   "3" = "0", "4" = "0", "5" = "1",
                                   "6" = "1", "7" = "2", "8" = "2", "9" = "2")
    df$education <- factor(df$education, levels = c(0, 1, 2), labels = c("low", "medium", "high"))
    summary(df$education)
    # dummy variables:
    df$low_edu <- ifelse(df$education == 'low', 1, 0)
    df$high_edu <- ifelse(df$education == 'high', 1, 0)

    #Descriptive statistics
    #Count of participants
    n_distinct(df$id)
    
    #Count of women and men (0 = women, 1 = men)
    df %>%
      group_by(sex) %>%
      summarise(
        count_sex = n_distinct(id)
      )
    
    #Average follow-up time with standard deviations and the median. 
    df %>%
      group_by(id) %>%
      slice(which.max(days_since_baseline)) %>%
      ungroup %>%
      summarise(mean_FU_days = mean(days_since_baseline, na.rm = TRUE),
                sd_FU_days = sd(days_since_baseline, na.rm = TRUE),
                median_FU_days = median(days_since_baseline, na.rm = TRUE)
                )
    
    #This makes a table with means and standard deviations for the following variables per days since baseline 
    ##(this should become years (I think...))
    ##Here we are missing all the NPA results  
    df %>%
      group_by(days_since_baseline) %>%
      summarise(
        nr_participants = n_distinct(id),
        mean_p_tau = mean(p_tau, na.rm = TRUE),
        sd_p_tau = sd(p_tau, na.rm = TRUE),
        mean_amyloid_b_ratio = mean(amyloid_b_ratio_42_40, na.rm = TRUE),
        sd_amyloid_b_ratio = sd(amyloid_b_ratio_42_40, na.rm = TRUE),
        mean_gfap = mean(gfap, na.rm = TRUE),
        sd_gfap = sd(gfap, na.rm = TRUE),
        mean_nfl = mean(nfl, na.rm = TRUE),
        sd_nfl = sd(nfl, na.rm = TRUE),
        mean_edu_years = mean(education_years, na.rm = TRUE),
        sd_edu_years = sd(education_years, na.rm = TRUE),
        mean_age = mean(age_rec, na.rm = TRUE),
        sd_age = sd(age_rec, na.rm = TRUE),
        )
    
    #same as above but here the table sorted by sex
    df %>%
      group_by(sex) %>%
      summarise(
        nr_participants = n_distinct(id),
        mean_p_tau = mean(p_tau, na.rm = TRUE),
        sd_p_tau = sd(p_tau, na.rm = TRUE),
        mean_amyloid_b_ratio = mean(amyloid_b_ratio_42_40, na.rm = TRUE),
        sd_amyloid_b_ratio = sd(amyloid_b_ratio_42_40, na.rm = TRUE),
        mean_gfap = mean(gfap, na.rm = TRUE),
        sd_gfap = sd(gfap, na.rm = TRUE),
        mean_nfl = mean(nfl, na.rm = TRUE),
        sd_nfl = sd(nfl, na.rm = TRUE),
        mean_edu_years = mean(education_years, na.rm = TRUE),
        sd_edu_years = sd(education_years, na.rm = TRUE),
        mean_age = mean(age_rec, na.rm = TRUE),
        sd_age = sd(age_rec, na.rm = TRUE),
      )
    
    #same as above but here the table sorted by days since baseline and sex
    df %>%
      group_by(days_since_baseline, sex) %>%
      summarise(
        nr_participants = n_distinct(id),
        mean_p_tau = mean(p_tau, na.rm = TRUE),
        sd_p_tau = sd(p_tau, na.rm = TRUE),
        mean_amyloid_b_ratio = mean(amyloid_b_ratio_42_40, na.rm = TRUE),
        sd_amyloid_b_ratio = sd(amyloid_b_ratio_42_40, na.rm = TRUE),
        mean_gfap = mean(gfap, na.rm = TRUE),
        sd_gfap = sd(gfap, na.rm = TRUE),
        mean_nfl = mean(nfl, na.rm = TRUE),
        sd_nfl = sd(nfl, na.rm = TRUE),
        mean_edu_years = mean(education_years, na.rm = TRUE),
        sd_edu_years = sd(education_years, na.rm = TRUE),
        mean_age = mean(age_rec, na.rm = TRUE),
        sd_age = sd(age_rec, na.rm = TRUE),
      )

    #Z-score transformations
    #Memory delayed recall z-transformations
    # if (sum(!is.na(df$priority_memory_dr_ravlt)) > 0) {
    if (memory_dr_test_name == "priority_memory_dr_ravlt") {
      df$priority_memory_dr <- df$priority_memory_dr_ravlt
      df$priority_memory_dr_z <- (
        df$priority_memory_dr_ravlt - (10.924 + (df$age_rec * -0.073) +
          (df$age_cent2 * -0.0009) + (df$sex_num * -1.197) + (df$low_edu * -0.844)
         + (df$high_edu * 0.424))) / sd(df$priority_memory_dr_ravlt, na.rm = TRUE)
    # } else if (sum(!is.na(df$priority_memory_dr_lm)) > 0) {
    } else if (memory_dr_test_name == "priority_memory_dr_lm") {
      df$priority_memory_dr_z <-  scale(df$priority_memory_dr_lm)
      df$priority_memory_dr <- df$priority_memory_dr_lm
    } else {
      return(list(
        "error_message" = paste("Delayed recall test not found")
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
    # RIRS_memory_dr <- nlme::lme(priority_memory_dr_z ~ fu_years + age_rec + sex + low_edu + high_edu + p_tau + p_tau * fu_years,
    #                       data = df,
    #                       random = ~ fu_years | id,
    #                       weights = nlme::varIdent(form= ~1 | fu_years),
    #                       correlation = "corSymm",
    #                       method = "ML",
    #                       na.action = na.exclude,
    #                       # Error: unused argument (REML = TRUE)
    #                       # REML = TRUE,
    #                       control = list(opt="optim")) #may need to change this if model doesn't converge

    # Unstructured Marginal Modal Memory delayed recall
    marginal_memory_dr <- nlme::gls(priority_memory_dr_z ~ fu_years + age2 + sex + low_edu + high_edu + p_tau + p_tau * fu_years,
                          data = df,
                          weights = nlme::varIdent(form= ~1 | fu_years),
                          correlation = nlme::corSymm(form = ~1 | id),
                          method = "ML",
                          na.action = na.exclude,
                          # Error: unused argument (REML = TRUE)
                          # REML = TRUE,
                          control = list(opt="optim")) #may need to change this if model doesn't converge

    results <- list(
      # "model_memory_dr" = RIRS_memory_dr,
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
