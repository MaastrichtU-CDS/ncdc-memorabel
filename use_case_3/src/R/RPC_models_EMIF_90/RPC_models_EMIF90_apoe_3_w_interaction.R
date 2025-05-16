RPC_models_EMIF_90 <- function(df, config, model = "memory", exclude=c()) {
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
    check_names <- c("age", "sex", "education_category_verhage", "p_tau", "amyloid_b_ratio_42_40", "gfap", "nfl", "priority_memory_dr_ravlt", "apoe_carrier")
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
    memory_dr_test_name <- "priority_memory_dr_cerad"
    df_plasma <- df[!is.na(df$p_tau),]
    df_baseline <- df[!is.na(df$age) & !is.na(df$sex),]
    df_baseline_education <- df[!is.na(df$education_category_verhage),]
    df_apoe <- df[!is.na(df$apoe_carrier),]
    df_baseline_education <- df_baseline_education[! duplicated(df_baseline_education$id),]
    df_grouped <- merge(
      x = df_baseline[c("id", "age", "sex", "birth_year")],
      y = df_baseline_education[c("id", "education_category_verhage", "education_years")],
      by = "id"
    )
    df_grouped <- df_grouped[! duplicated(df_grouped$id),]
    df_grouped <- merge(
      x = df_grouped[c("id", "age", "sex", "birth_year", "education_category_verhage", "education_years")],
      y = df_plasma[c("id", "date_plasma", "p_tau", "gfap", "nfl", "amyloid_b_42", "amyloid_b_40", "amyloid_b_ratio_42_40")],
      by = "id"
    )
    df_grouped <- df_grouped[! duplicated(df_grouped$id),]
    df_apoe <- df_apoe[! duplicated(df_apoe$id),]
    df_grouped <- merge(
      x = df_grouped,
      y = df_apoe[c("id", "apoe_carrier")],
      by = "id"
      # all.x = T
    )
    df_cogn_test <- df[!is.na(df[[memory_dr_test_name]]) | !is.na(df[["attention_test_tmt_a_time"]]) | !is.na(df[["attention_test_sdst_90_ts"]])
      | !is.na(df[["priority_memory_im_cerad"]]) | !is.na(df[["dexterity_clock_drawing"]]) | !is.na(df[["priority_language_animal_fluency_60_correct"]])
      | !is.na(df[["priority_memory_dr_cerad"]]) | !is.na(df[["mmse_total"]]),]
    df <- merge(
      x = df_cogn_test[c("id", "date", "attention_test_tmt_a_time", "attention_test_tmt_a_errors",
        "attention_test_sdst_90_ts", "date_memory", "priority_memory_im_cerad", "priority_memory_im_vat_a",
        "priority_memory_im_vat_b", "priority_memory_im_rrf", "priority_memory_dr_cerad", "priority_memory_dr_rrf_short",
        "date_executive", "priority_executive_tmt_b_time", "priority_executive_tmt_b_errors", "priority_executive_wais_3_b",
        "priority_executive_wais_3_b_span", "priority_executive_wais_3_f", "priority_executive_wais_3_f_span", "date_language",
        "priority_language_animal_fluency_60_correct", "priority_language_animal_fluency_60_errors", "priority_language_animal_fluency_60_ts",
        "priority_language_animal_fluency_120_correct", "priority_language_animal_fluency_120_errors", "priority_language_animal_fluency_120_ts",
        "priority_language_letter_fluency_60", "dexterity_clock_drawing", "mmse_total")],
      y = df_grouped,
      by = "id"
      # all.x = T
    )
    # attention_test_sdst_60_correct should be attention_test_sdst_60_ts but there was an error in the DB
    df$attention_test_sdst_60_ts <- df$attention_test_sdst_60_correct
    excluded <- unique(df$id[is.na(df$birth_year) | is.na(df$sex)])

    # Missing data
    df[df==-95.0] <- NA
    df[df==-96.0] <- NA
    df[df==-97.0] <- NA
    df[df==-98.0] <- NA
    df[df==-99.0] <- NA

    # Selected participants
    included <- unique(df$id[! df$id %in% excluded])
    vtg::log$info("Number of rows in the dataset: '{nrow(df)}'")
    vtg::log$info("Excluded '{length(excluded)}' participants")
    vtg::log$info("'{length(included)}' participants included in the analysis")
    df <- df[df$id %in% included,]
    vtg::log$info("Number of rows in the dataset after exclusion: '{nrow(df)}'")

    df %>%
      dplyr::mutate(dplyr::across(c(date, date_plasma), as.Date, format = "%d/%m/%Y"))
    df$difference_time <- lubridate::time_length(lubridate::interval(as.Date(df$date), as.Date(df$date_plasma)), unit = "years")

    # Should it be the minimum difference or is it necessary to be within 1 year?
 df_baseline <- df %>%
      dplyr::group_by(id) %>%
      dplyr::slice(which.min(abs(difference_time)))
    df_baseline$baseline <- df_baseline$difference_time >= -1 & df_baseline$difference_time <= 1

    baseline_df <- df_baseline %>%
      dplyr::filter(baseline == TRUE) %>%
      dplyr::select(id, date) %>%
      dplyr::rename(date_baseline = date)

    df <- df %>%
      dplyr::left_join(baseline_df[c("id", "date_baseline")], by = "id") %>%
      dplyr::mutate(days_since_baseline = as.numeric(difftime(date, date_baseline, units = "days"))) %>%
      dplyr::filter(days_since_baseline >= 0)

    df$years_since_baseline <- as.integer(df$days_since_baseline/365.25, 0)

    df <- subset(df, years_since_baseline >= 0)
