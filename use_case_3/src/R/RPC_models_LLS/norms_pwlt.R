RPC_models_lls_norm <- function(df, config, model = "memory", exclude=c()) {
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

    #Create the dataset needed
    # cognitive variables
    # The variables available at LLS: priority_memory_im_pwlt, priority_memory_de_pwlt
    df$priority_memory_dr_lm <- df$priority_memory_im_pwlt
    df$priority_memory_im_lm <- df$priority_memory_de_pwlt
    cog_vars <- c(
      "priority_memory_im_pwlt",
      "priority_memory_de_pwlt"
    )

    df_baseline <- df[!is.na(df$age) | !is.na(df$education_category_3) | !is.na(df$education_category_verhage),]
    df_baseline <- df_baseline[! duplicated(df_baseline$id),]

    df_baseline_education <- df[!is.na(df$education_category_3),]
    df_baseline_education <- df_baseline_education[! duplicated(df_baseline_education$id),]

    df_grouped <- df_baseline %>%
      dplyr::select(id, age, sex, birth_year) %>%
      dplyr::left_join(
        df_baseline_education %>% dplyr::select(id, education_category_3),
        by = "id"
      ) %>%
      dplyr::distinct(id, .keep_all = TRUE)

    # Cognitive test filter
    df_cogn_test <- df[rowSums(!is.na(df[cog_vars])) > 0, ]

    df_final <- df_cogn_test %>%
      dplyr::select(
        id, date,
        priority_memory_im_pwlt, priority_memory_de_pwlt,
        priority_executive_stroop_3_time, mmse_total,
        attention_test_stroop_1_time, attention_test_stroop_2_time,
        attention_test_sdst_60_correct, attention_test_sdst_90_correct
      ) %>%
      dplyr::left_join(df_grouped, by = "id")

    excluded <- df_final %>%
      dplyr::filter(is.na(birth_year) | is.na(sex)) %>%
      dplyr::pull(id) %>%
      unique()

    # Age of participant:
    # current_year <- format(Sys.Date(), "%Y")
    # Year of birth will always be available (mandatory in OMOP), age is not guarantee
    df$age_rec <- ifelse(is.na(df$age), as.numeric(format(df$date, "%Y")) - df$birth_year, df$age)

    #Age squared:
    df$age2 <- df$age_rec^2

    # Centering age:
    df$age_cent <- df$age_rec - 50
    df$age_cent2 <- df$age_cent^2

    # Sex - in the database 0 identifies men and 1 women
    # For the models: women should be identified with 0 and men with 1
    # This dataframe only contains patients with birth year and sex info
    df$sex_num <- ifelse(df$sex == 0, 1, 0)
    df$sex <- factor(df$sex, levels = c(0, 1), labels = c("male", "female"))

    # Education levels
    df$education <- factor(df$education_category_3, levels = c(0, 1, 2), labels = c("low", "medium", "high"))

    # dummy variables:
    df$education_low <- ifelse(df$education == 'low', 1, 0)
    df$education_high <- ifelse(df$education == 'high', 1, 0)

    df$id <- as.factor(as.character(df$id))

    # Descriptive statistics --------------------------------------------------
    #Count of participants
    dplyr::n_distinct(df$id)

    #Count of women and men (0 = women, 1 = men)
    count_men_and_women_table <- df %>%
      dplyr::group_by(sex) %>%
      dplyr::summarise(
        count_sex = dplyr::n_distinct(id)
      )

    ##Descriptives delayed recall logical memory task, immediate recall logical memory task.
    descriptives_by_sex_table <- df %>%
      dplyr::group_by(sex) %>%
      dplyr::summarise(
        nr_participants = dplyr::n_distinct(id),
        mean_age = mean(age, na.rm = TRUE),
        sd_age = sd(age, na.rm = TRUE),
        mean_dr_lm = mean(priority_memory_dr_lm, na.rm = TRUE),
        sd_dr_lm = sd(priority_memory_dr_lm, na.rm = TRUE),
        mean_im_lm = mean(priority_memory_im_lm, na.rm = TRUE),
        sd_im_lm = sd(priority_memory_im_lm, na.rm = TRUE)
      )

    descriptives_by_edu_table <- df %>%
      dplyr::group_by(education_category_3) %>%
      dplyr::summarise(
        nr_participants = dplyr::n_distinct(id),
        mean_age = mean(age, na.rm = TRUE),
        sd_age = sd(age, na.rm = TRUE),
        mean_dr_lm = mean(priority_memory_dr_lm, na.rm = TRUE),
        sd_dr_lm = sd(priority_memory_dr_lm, na.rm = TRUE),
        mean_im_lm = mean(priority_memory_im_lm, na.rm = TRUE),
        sd_im_lm = sd(priority_memory_im_lm, na.rm = TRUE)
      )

    descriptives_by_sex_and_edu_table <- df %>%
      dplyr::group_by(sex, education_category_3) %>%
      dplyr::summarise(nr_participants = dplyr::n_distinct(id),
                mean_age = mean(age, na.rm = TRUE),
                sd_age = sd(age, na.rm = TRUE),
                mean_dr_lm = mean(priority_memory_dr_lm, na.rm = TRUE),
                sd_dr_lm = sd(priority_memory_dr_lm, na.rm = TRUE),
                mean_im_lm = mean(priority_memory_im_lm, na.rm = TRUE),
                sd_im_lm = sd(priority_memory_im_lm, na.rm = TRUE)
      )

    # descriptives_im_lm <- describe(df$priority_memory_im_lm)
    descriptives_im_lm_stats <- list(
      mean = mean(df$priority_memory_im_lm, na.rm=TRUE),
      sd = sd(df$priority_memory_im_lm, na.rm=TRUE),
      min = min(df$priority_memory_im_lm, na.rm=TRUE),
      max = max(df$priority_memory_im_lm, na.rm=TRUE)
    )

    # descriptives_dr_lm <- describe(df$priority_memory_dr_lm)
    descriptives_dr_lm_stats <- list(
      mean = mean(df$priority_memory_dr_lm, na.rm=TRUE),
      sd = sd(df$priority_memory_dr_lm, na.rm=TRUE),
      min = min(df$priority_memory_dr_lm, na.rm=TRUE),
      max = max(df$priority_memory_dr_lm, na.rm=TRUE)
    )

    df_clean <- df %>%
      dplyr::select(priority_memory_im_lm, priority_memory_dr_lm, age_cent, sex, education_low, education_high) %>%
      tidyr::drop_na()
    # Regression models -------------------------------------------------------
    #Need some new library's for this part
    library(lmtest)

    #Syntax regression model including age, sex, and education for immediate recall of logical memory
    model_im_lm <- lm(priority_memory_im_lm ~ age_cent + sex + education_low + education_high, data = df)
    summary_model_im_lm <- sjPlot::tab_model(model_im_lm)

    #heteroscedasticity test for LM_IM. Significant = homoscedasticity violated.
    bptest_im_lm <- lmtest::bptest(model_im_lm, varformula = NULL, studentize = TRUE, data = list(), weights = NULL)

    #Since heteroscedascity is not the case, we need the following models:
    rse <- sigma(model_im_lm)

 model_im_lm_residual_errors <- sjPlot::tab_model(
  model_im_lm,
  show.r2 = TRUE,
  show.adj.r2 = TRUE,
  show.aic = TRUE,
  notes = paste0("Residual Standard Error: ", round(rse, 3),
                 " (df = ", df.residual(model_im_lm), ")")
)

        rse2 <- sigma(model_dr_lm)

model_dr_lm_residual_errors <- sjPlot::tab_model(
  model_dr_lm,
  show.r2 = TRUE,
  show.adj.r2 = TRUE,
  show.aic = TRUE,
  notes = paste0("Residual Standard Error: ", round(rse2, 3),
                 " (df = ", df.residual(model_im_lm), ")")
)
    #Calculate the residuals for the model
    res_im <- residuals(model_im_lm)

    #Create a polynomial function to model the squared residuals
    im_lm_var <- lm(I(res_im^2) ~ age_cent + sex + education_low + education_high, data = model.frame(model_im_lm))
    summary_model_var_im_lm <- sjPlot::tab_model(im_lm_var)

    #Create a polynomial function to model the cubic residuals
    im_lm_var_cube <- lm(I(res_im^3) ~ age_cent + sex + education_low + education_high, data = model.frame(model_im_lm))
    summary_model_var_im_lm_cube <- sjPlot::tab_model(im_lm_var)

    #Predict the residuals (this is a proof of concept at the moment)
    #pred_var <- predict(lm_var, df)

    #Take the square root of the (polynomial) residuals to get the variance for the z-scores (this is a proof of concept at the moment)
    #sigma <- sqrt(pred_var)

    #Predict the logical memory score (this is a proof of concept at the moment)
    #mu <- predict(model_im_lm, df)

    #calculate the z-scores for dataset (this is a proof of concept at the moment)
    #im_lm_z_score <- (df$priority_memory_im_lm - mu) / sigma

    ##Syntax regression model including age, sex, and education for delayed recall of logical memory
    model_dr_lm <- lm(priority_memory_dr_lm ~ age_cent + sex + education_low + education_high, data = df)
    summary_model_dr_lm <- sjPlot::tab_model(model_dr_lm)
    res_dr <- residuals(model_dr_lm)

    #heteroscedasticity test for LM_DR. Significant = homoscedasticity violated.
    bptest_dr_lm <- lmtest::bptest(model_dr_lm, varformula = NULL, studentize = TRUE, data = list(), weights = NULL)

    #Create a polynomial function to model the squared residuals
    dr_lm_var <- lm(I(res_dr^2) ~ age_cent + sex + education_low + education_high, data = model.frame(model_dr_lm))
    summary_model_var_dr_lm <- sjPlot::tab_model(dr_lm_var)

    #Create a polynomial function to model the cubic residuals
    dr_lm_var_cube <- lm(I(res_dr^3) ~ age_cent + sex + education_low + education_high, data = model.frame(model_dr_lm))
    summary_model_var_dr_lm_cube <- sjPlot::tab_model(dr_lm_var)

    #Predict the residuals (this is a proof of concept at the moment)
    #pred_var <- predict(lm_var, df)

    #Take the square root of the (polynomial) residuals to get the variance for the z-scores (this is a proof of concept at the moment)
    #sigma <- sqrt(pred_var)

    #Predict the logical memory score (this is a proof of concept at the moment)
    #mu <- predict(model_im_lm, df)

    #calculate the z-scores for dataset (this is a proof of concept at the moment)
    #im_lm_z_score <- (df$priority_memory_dr_lm - mu) / sigma

    # The output --------------------------------------------------------------
    results <- list(
      "descriptives_by_sex" = descriptives_by_sex_table,
      "descriptives_by_edu" = descriptives_by_edu_table,
      "descriptives_by_sex_and_edu" = descriptives_by_sex_and_edu_table,
      "summary_model_im_lm" = summary_model_im_lm,
      "summary_model_dr_lm" = summary_model_dr_lm,
      "bptest_im_lm" =  bptest_im_lm,
      "bptest_dr_lm" =  bptest_dr_lm,
      "summary_model_var_im_lm" = summary_model_var_im_lm,
      "summary_model_var_dr_lm" = summary_model_var_dr_lm,
      "summary_model_var_im_lm_cube" = summary_model_var_im_lm_cube,
      "summary_model_var_dr_lm_cube" = summary_model_var_dr_lm_cube,
      "model_im_lm_residual_errors" = model_dr_lm_residual_errors,
      "model_dr_lm_residual_errors" = model_dr_lm_residual_errors,
      "n" = nrow(df),
      "n_cog" = nrow(df_cogn_test),
      "n_clean" = nrow(df_clean),
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
