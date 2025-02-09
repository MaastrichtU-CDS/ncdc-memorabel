
RPC_models_Smart_MR <- function(df, config, model = "memory", exclude=c()) {
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
    check_names <- c("age", "sex", "education_category_3", "p_tau", "amyloid_b_ratio_42_40", "gfap", "nfl", "priority_memory_dr_15_word_list_correct", "apoe_carrier")
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
    if (sum(!is.na(df$priority_memory_dr_15_word_list_correct)) > 0) {
      memory_dr_test_name <- "priority_memory_dr_15_word_list_correct"
    } else if (sum(!is.na(df$priority_memory_dr_lm)) > 0) {
      memory_dr_test_name <- "priority_memory_dr_lm"
    } else {
      return(list(
        "error_message" = paste("Delayed recall test not found")
      ))
    }
    vtg::log$info("Cognitive test available: '{memory_dr_test_name}'")

    df_plasma <- df[!is.na(df$p_tau),]
    df_baseline <- df[!is.na(df$education_category_3) | !is.na(df$education_category_verhage),]
    df_cogn_test <- df[!is.na(df[[memory_dr_test_name]]) & df$id %in% df_plasma$id & df$id %in% df_baseline$id,]
    df_mmse <- df[!is.na(df[["mmse_total"]]) & df$id %in% df_plasma$id & df$id %in% df_baseline$id,]
      # dplyr::group_by(id, date) %>%
      # dplyr::filter(abs(difftime(date, df_plasma[df_plasma$id == id,]$date_plasma)) == min(abs(difftime(date, df_plasma[df_plasma$id == id,]$date_plasma))))
    # df_amyloid <- df[!is.na(df$amyloid_b_ratio_42_40),]

    # education_years - not available in most cohort (included here for now
    # to be available for the summarise function)
    df_grouped <- merge(
      x = df_baseline[c("id", "age", "sex", "birth_year", "education_category_3", "education_years", "education_category_verhage")],
      y = df_plasma[c("id", "date_plasma", "p_tau", "gfap", "nfl", "amyloid_b_42", "amyloid_b_40", "amyloid_b_ratio_42_40", "apoe_carrier")],
      by = "id"
    )
    df_grouped <- df_grouped[! duplicated(df_grouped$id),]
    df <- merge(
      x = df_cogn_test[c("id", "date", "priority_memory_dr_15_word_list_correct	",
                         "priority_memory_im_15_word_list_correct", "priority_language_animal_fluency_120_correct", "attention_test_sdst_120_correct")],
      y = df_grouped,
      by = "id",
      all.x = T
    )

    # df <- merge(
    #   x = df_grouped,
    #   y = df_amyloid[c("id", "amyloid_b_ratio_42_40")],
    #   by = "id",
    #   all.x = T
    # )

    excluded <- unique(df$id[is.na(df$birth_year) | is.na(df$sex)])
      # df$id[is.na(df$birth_year) | is.na(df$sex) | !anyDuplicated(df$id, incomparable = FALSE, fromLast = FALSE)]

    # Selected participants
    included <- unique(df$id[! df$id %in% excluded])
    vtg::log$info("Number of rows in the dataset: '{nrow(df)}'")
    vtg::log$info("Excluded '{length(excluded)}' participants")
    vtg::log$info("'{length(included)}' participants included in the analysis")
    df <- df[df$id %in% included,]
    vtg::log$info("Number of rows in the dataset after exclusion: '{nrow(df)}'")

    # Pre-processing the data
    # df <- preprocessing(df, model, config)

    df %>%
      dplyr::mutate(dplyr::across(c(date, date_plasma), as.Date, format = "%d/%m/%Y"))
    df$difference_time <- lubridate::time_length(lubridate::interval(as.Date(df$date), as.Date(df$date_plasma)), unit = "years")

    # Should it be the minimum difference or is it necessary to be within 1 year?
    df$baseline <- df$difference_time >= -1 & df$difference_time <= 1
    baseline_df <- df %>%
      dplyr::filter(baseline == TRUE) %>%
      dplyr::select(id, date) %>%
      dplyr::rename(date_baseline = date)

    df <- df %>%
      dplyr::left_join(baseline_df[c("id", "date_baseline")], by = "id") %>%
      dplyr::mutate(days_since_baseline = as.numeric(difftime(date, date_baseline, units = "days")))

    df$years_since_baseline <- as.integer(df$days_since_baseline/365.25, 0)

    df <- subset(df, years_since_baseline >= 0)

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
    # available, no need to consider NAs
    df$sex_num <- ifelse(df$sex == 0, 1, 0)
    # df$sex_num <- factor(df$sex_num, levels = c(0, 1), labels = c("female", "male"))
    df$sex <- factor(df$sex, levels = c(0, 1), labels = c("male", "female"))

    # Apoe
    df$apoe_carrier <- factor(df$apoe_carrier, levels = c(0, 1), labels = c("no","yes"))

    # dummy variables:
    df$education_low <- ifelse(df$education == 'low', 1, 0)
    df$education_high <- ifelse(df$education == 'high', 1, 0)

    # In the original dataset, this variable may not
    # be associated with the plasma data but only with the visit date
    # May be necessary to first check if amyloid_b_42 and amyloid_b_40 are
    # available. If not available, use amyloid_b_ratio_42_40 directly from
    # the database.
    df$amyloid_b_ratio_42_40 <- ifelse(
      !(is.na(df$amyloid_b_42) & is.na(df$amyloid_b_40) & df$amyloid_b_40 != 0),
      df$amyloid_b_42 / df$amyloid_b_40,
      df$amyloid_b_ratio_42_40
    )

    df$id <- as.factor(as.character(df$id))
    # df %>% dplyr::mutate_if(is.character, as.factor)

    #animal fluency 120 sec to 60 sec
    df$priority_language_animal_fluency_60_correct  <- df$priority_language_animal_fluency_120_correct/2

    #Descriptive statistics
    #Count of participants
    dplyr::n_distinct(df$id)

    #Count of women and men (0 = women, 1 = men)
    count_men_and_women_table <- df %>%
      dplyr::group_by(sex) %>%
      dplyr::summarise(
        count_sex = dplyr::n_distinct(id)
      )

    #Average follow-up time with standard deviations and the median.
    average_FU_time_table <- df %>%
      dplyr::group_by(id) %>%
      dplyr::slice(which.max(years_since_baseline)) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(
        mean_FU_years = mean(years_since_baseline, na.rm = TRUE),
        sd_FU_years = sd(years_since_baseline, na.rm = TRUE),
        median_FU_years = median(years_since_baseline, na.rm = TRUE)
        )

    #descriptives of education
    descriptives_education_table <- df %>%
    dplyr::group_by(years_since_baseline, sex, education_category_3) %>%
    dplyr::filter(dplyr::n_distinct(id) > 2) %>%
    dplyr::summarise(count = dplyr::n())

    #This makes a table with means and standard deviations for the following variables per days since baseline
    ##(this should become years (I think...))
    ##Here we are missing all the NPA results
    descriptives_per_year_table <- df %>%
      dplyr::group_by(years_since_baseline) %>%
      dplyr::filter(dplyr::n_distinct(id) > 2) %>%
      dplyr::summarise(
        nr_participants = dplyr::n_distinct(id),
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
        mean_apoe = mean(apoe_carrier, na.rm = TRUE),
        sd_apoe = sd(apoe_carrier, na.rm = TRUE)
    )

    #same as above but here the table sorted by sex
    descriptives_by_sex_table <- df %>%
      dplyr::group_by(sex) %>%
      dplyr::filter(dplyr::n_distinct(id) > 2) %>%
      dplyr::summarise(
        nr_participants = dplyr::n_distinct(id),
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
        years_since_baseline = mean(years_since_baseline, na.rm = TRUE),
        sd_years_since_baseline = sd(years_since_baseline, na.rm = TRUE),
        mean_apoe = mean(apoe_carrier, na.rm = TRUE),
        sd_apoe = sd(apoe_carrier, na.rm = TRUE)
    )

    #same as above but here the table sorted by years since baseline and sex
   descriptives_by_sex_and_FU_table <- df %>%
     dplyr::group_by(years_since_baseline, sex) %>%
     dplyr::filter(dplyr::n_distinct(id) > 2) %>%
     dplyr::summarise(
      nr_participants = dplyr::n_distinct(id),
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
      mean_apoe = mean(apoe_carrier, na.rm = TRUE),
      sd_apoe = sd(apoe_carrier, na.rm = TRUE)
    )

    #Z-score transformations
    #Z-score: Memory immediate recall
    #used van der Elst for RAVLT
    #used norm scores from ADC for logical memory
    if (c("priority_memory_im_15_word_list_correct") %in% colnames(df)) {
      df$priority_memory_im_z <-
        ((df$priority_memory_im_15_word_list_correct - (49.672+ (df$age_cent * -0.247) + (df$age_cent2 * -0.0033) + (df$sex_num * -4.227) + (df$education_low * -3.055) + (df$education_high * 2.496))) / 7.826)
    } else {
    return(list(
        "error_message" = paste("immediate recall test not found, no z-score transformation possible")
      ))
    }

    #Memory delayed recall z-transformations
    #used van der Elst for RAVLT
    #used norm scores from ADC for logical memory
    if (memory_dr_test_name == "priority_memory_dr_15_word_list_correct") {
      df$priority_memory_dr <- df$priority_memory_dr_15_word_list_correct
      df$priority_memory_dr_z <- (
        df$priority_memory_dr_15_word_list_correct - (10.924 + (df$age_cent * -0.073) +
          (df$age_cent2 * -0.0009) + (df$sex_num * -1.197) + (df$education_low * -0.844)
         + (df$education_high * 0.424))) / 2.496
    } else {
      return(list(
        "error_message" = paste("Delayed recall test not found")
      ))
    }

    #Z-score: language
   print("Animal Fluency")
   print(sum(is.na(df["priority_language_animal_fluency_120_correct	"])))
    #Van der Elst, et al. norms for animal fluency
     if (c("priority_language_animal_fluency_120_correct	") %in% colnames(df)) {
       df$priority_language_z <-
      (df$priority_language_animal_fluency_60_correct - (24.777 +(df$age_cent * -0.097) + (df$education_low * -2.790) + (df$education_high * 1.586)) / 5.797)
    } else {
      return(list(
        "error_message" = paste("language test not found, no z-score transformation possible")
      ))
    }

    #Z-score: processing speed
    #SDST WAIS-4 norms
    if (c("attention_test_sdst_120_correct") %in% colnames(df)){
      df <- df %>%
        mutate(sdst_scaled = case_when(
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 0 & attention_test_sdst_120_correct <= 34  ~ 1,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 35 & attention_test_sdst_120_correct <= 40 ~ 2,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 41 & attention_test_sdst_120_correct <= 45 ~ 3,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 46 & attention_test_sdst_120_correct <= 51 ~ 4,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 52 & attention_test_sdst_120_correct <= 56 ~ 5,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 57 & attention_test_sdst_120_correct <= 62 ~ 6,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 63 & attention_test_sdst_120_correct <= 67 ~ 7,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 68 & attention_test_sdst_120_correct <= 72 ~ 8,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 73 & attention_test_sdst_120_correct <= 77 ~ 9,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 78 & attention_test_sdst_120_correct <= 82 ~ 10,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 83 & attention_test_sdst_120_correct <= 87 ~ 11,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 88 & attention_test_sdst_120_correct <= 91 ~ 12,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 92 & attention_test_sdst_120_correct <= 96 ~ 13,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 97 & attention_test_sdst_120_correct <= 100 ~ 14,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 101 & attention_test_sdst_120_correct <= 104 ~ 15,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 105 & attention_test_sdst_120_correct <= 108 ~ 16,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 109 & attention_test_sdst_120_correct <= 112 ~ 17,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 113 & attention_test_sdst_120_correct <= 115 ~ 18,
          age_rec >= 25.0 & age_rec <= 29.92 & attention_test_sdst_120_correct >= 116 & attention_test_sdst_120_correct <= 135 ~ 19,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 0 & attention_test_sdst_120_correct <= 34  ~ 1,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 35 & attention_test_sdst_120_correct <= 40 ~ 2,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 41 & attention_test_sdst_120_correct <= 45 ~ 3,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 46 & attention_test_sdst_120_correct <= 51 ~ 4,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 52 & attention_test_sdst_120_correct <= 56 ~ 5,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 57 & attention_test_sdst_120_correct <= 61 ~ 6,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 62 & attention_test_sdst_120_correct <= 67 ~ 7,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 68 & attention_test_sdst_120_correct <= 72 ~ 8,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 73 & attention_test_sdst_120_correct <= 77 ~ 9,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 78 & attention_test_sdst_120_correct <= 81 ~ 10,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 82 & attention_test_sdst_120_correct <= 86 ~ 11,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 87 & attention_test_sdst_120_correct <= 91 ~ 12,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 92 & attention_test_sdst_120_correct <= 96 ~ 13,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 97 & attention_test_sdst_120_correct <= 100 ~ 14,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 101 & attention_test_sdst_120_correct <= 104 ~ 15,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 105 & attention_test_sdst_120_correct <= 108 ~ 16,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 109 & attention_test_sdst_120_correct <= 112 ~ 17,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 113 & attention_test_sdst_120_correct <= 115 ~ 18,
          age_rec > 29.92 & age_rec <= 34.92 & attention_test_sdst_120_correct >= 116 & attention_test_sdst_120_correct <= 135 ~ 19,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 0 & attention_test_sdst_120_correct <= 33  ~ 1,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 34 & attention_test_sdst_120_correct <= 39 ~ 2,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 40 & attention_test_sdst_120_correct <= 44 ~ 3,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 45 & attention_test_sdst_120_correct <= 49 ~ 4,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 50 & attention_test_sdst_120_correct <= 54 ~ 5,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 55 & attention_test_sdst_120_correct <= 59 ~ 6,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 60 & attention_test_sdst_120_correct <= 64 ~ 7,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 65 & attention_test_sdst_120_correct <= 69 ~ 8,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 70 & attention_test_sdst_120_correct <= 74 ~ 9,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 75 & attention_test_sdst_120_correct <= 79 ~ 10,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 80 & attention_test_sdst_120_correct <= 83 ~ 11,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 84 & attention_test_sdst_120_correct <= 88 ~ 12,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 89 & attention_test_sdst_120_correct <= 93 ~ 13,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 94 & attention_test_sdst_120_correct <= 98 ~ 14,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 99 & attention_test_sdst_120_correct <= 102 ~ 15,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 103 & attention_test_sdst_120_correct <= 107 ~ 16,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 108 & attention_test_sdst_120_correct <= 111 ~ 17,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 112 & attention_test_sdst_120_correct <= 115 ~ 18,
          age_rec > 34.92 & age_rec <= 44.92 & attention_test_sdst_120_correct >= 116 & attention_test_sdst_120_correct <= 135 ~ 19,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 0 & attention_test_sdst_120_correct <= 28  ~ 1,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 29 & attention_test_sdst_120_correct <= 33 ~ 2,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 34 & attention_test_sdst_120_correct <= 38 ~ 3,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 39 & attention_test_sdst_120_correct <= 43 ~ 4,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 44 & attention_test_sdst_120_correct <= 48 ~ 5,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 49 & attention_test_sdst_120_correct <= 53 ~ 6,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 54 & attention_test_sdst_120_correct <= 58 ~ 7,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 59 & attention_test_sdst_120_correct <= 63 ~ 8,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 64 & attention_test_sdst_120_correct <= 68 ~ 9,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 69 & attention_test_sdst_120_correct <= 72 ~ 10,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 73 & attention_test_sdst_120_correct <= 77 ~ 11,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 78 & attention_test_sdst_120_correct <= 82 ~ 12,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 83 & attention_test_sdst_120_correct <= 87 ~ 13,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 88 & attention_test_sdst_120_correct <= 91 ~ 14,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 92 & attention_test_sdst_120_correct <= 96 ~ 15,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 97 & attention_test_sdst_120_correct <= 101 ~ 16,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 102 & attention_test_sdst_120_correct <= 105 ~ 17,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 106 & attention_test_sdst_120_correct <= 110 ~ 18,
          age_rec > 44.92 & age_rec <= 54.92 & attention_test_sdst_120_correct >= 111 & attention_test_sdst_120_correct <= 135 ~ 19,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 0 & attention_test_sdst_120_correct <= 22  ~ 1,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 23 & attention_test_sdst_120_correct <= 27 ~ 2,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 28 & attention_test_sdst_120_correct <= 31 ~ 3,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 32 & attention_test_sdst_120_correct <= 36 ~ 4,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 37 & attention_test_sdst_120_correct <= 41 ~ 5,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 42 & attention_test_sdst_120_correct <= 46 ~ 6,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 47 & attention_test_sdst_120_correct <= 50 ~ 7,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 51 & attention_test_sdst_120_correct <= 55 ~ 8,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 56 & attention_test_sdst_120_correct <= 60 ~ 9,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 61 & attention_test_sdst_120_correct <= 65 ~ 10,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 66 & attention_test_sdst_120_correct <= 69 ~ 11,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 70 & attention_test_sdst_120_correct <= 74 ~ 12,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 75 & attention_test_sdst_120_correct <= 79 ~ 13,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 80 & attention_test_sdst_120_correct <= 83 ~ 14,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 84 & attention_test_sdst_120_correct <= 88 ~ 15,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 89 & attention_test_sdst_120_correct <= 93 ~ 16,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 94 & attention_test_sdst_120_correct <= 98 ~ 17,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 99 & attention_test_sdst_120_correct <= 102 ~ 18,
          age_rec > 54.92 & age_rec <= 64.92 & attention_test_sdst_120_correct >= 103 & attention_test_sdst_120_correct <= 135 ~ 19,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 0 & attention_test_sdst_120_correct <= 20  ~ 1,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 21 & attention_test_sdst_120_correct <= 22 ~ 2,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 23 & attention_test_sdst_120_correct <= 25 ~ 3,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 26 & attention_test_sdst_120_correct <= 29 ~ 4,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 30 & attention_test_sdst_120_correct <= 33 ~ 5,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 34 & attention_test_sdst_120_correct <= 36 ~ 6,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 37 & attention_test_sdst_120_correct <= 40 ~ 7,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 41 & attention_test_sdst_120_correct <= 46 ~ 8,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 47 & attention_test_sdst_120_correct <= 50 ~ 9,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 51 & attention_test_sdst_120_correct <= 55 ~ 10,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 56 & attention_test_sdst_120_correct <= 60 ~ 11,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 61 & attention_test_sdst_120_correct <= 64 ~ 12,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 65 & attention_test_sdst_120_correct <= 69 ~ 13,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 70 & attention_test_sdst_120_correct <= 74 ~ 14,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 75 & attention_test_sdst_120_correct <= 79 ~ 15,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 80 & attention_test_sdst_120_correct <= 84 ~ 16,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 85 & attention_test_sdst_120_correct <= 89 ~ 17,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 90 & attention_test_sdst_120_correct <= 95 ~ 18,
          age_rec > 64.92 & age_rec <= 74.92 & attention_test_sdst_120_correct >= 96 & attention_test_sdst_120_correct <= 135 ~ 19,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 0 & attention_test_sdst_120_correct <= 19  ~ 1,
          age_rec > 74.92  & attention_test_sdst_120_correct = 20 ~ 2,
          age_rec > 74.92  & attention_test_sdst_120_correct = 21 ~ 3,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 22 & attention_test_sdst_120_correct <= 23 ~ 4,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 24 & attention_test_sdst_120_correct <= 26 ~ 5,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 27 & attention_test_sdst_120_correct <= 29 ~ 6,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 30 & attention_test_sdst_120_correct <= 33 ~ 7,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 34 & attention_test_sdst_120_correct <= 36 ~ 8,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 37 & attention_test_sdst_120_correct <= 40 ~ 9,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 41 & attention_test_sdst_120_correct <= 43 ~ 10,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 44 & attention_test_sdst_120_correct <= 46 ~ 11,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 47 & attention_test_sdst_120_correct <= 50 ~ 12,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 51 & attention_test_sdst_120_correct <= 55 ~ 13,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 56 & attention_test_sdst_120_correct <= 61 ~ 14,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 62 & attention_test_sdst_120_correct <= 69 ~ 15,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 70 & attention_test_sdst_120_correct <= 75 ~ 16,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 76 & attention_test_sdst_120_correct <= 80 ~ 17,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 81 & attention_test_sdst_120_correct <= 86 ~ 18,
          age_rec > 74.92  & attention_test_sdst_120_correct >= 87 & attention_test_sdst_120_correct <= 135 ~ 19,
          TRUE ~ NA_real_  # Assign NA for other cases
        ))
    
      df <- df %>%
        mutate(priority_processing_speed_sdst_z = case_when(
          sdst_scaled >= 1 ~ -3.00,
          sdst_scaled >= 2 ~ -2.67,
          sdst_scaled >= 3 ~ -2.33,
          sdst_scaled >= 4 ~ -2.00,
          sdst_scaled >= 5 ~ -1.67,
          sdst_scaled >= 6 ~ -1.33,
          sdst_scaled >= 7 ~ -1.00,
          sdst_scaled >= 8 ~ -0.67,
          sdst_scaled >= 9 ~ -0.33,
          sdst_scaled >= 10 ~ 0,
          sdst_scaled >= 11 ~ 0.33,
          sdst_scaled >= 12 ~ 0.67,
          sdst_scaled >= 13 ~ 1.00,
          sdst_scaled >= 14 ~ 1.33,
          sdst_scaled >= 15 ~ 1.67,
          sdst_scaled >= 16 ~ 2.00,
          sdst_scaled >= 17 ~ 2.33,
          sdst_scaled >= 18 ~ 2.67,
          sdst_scaled >= 19 ~ 3.00,
          TRUE ~ NA_real_  # Assign NA for other cases  
        ))
    } else  {
      print("No measure for processing speed found, no z-score transformation possible")
    }

    df$education_low <- as.factor(df$education_low)
    df$education_high <- as.factor(df$education_high)

#This makes a table with means and standard deviations for the following variables per days since baseline
    descriptives_per_year_NPA_table <- df %>%
      dplyr::group_by(years_since_baseline) %>%
      dplyr::filter(dplyr::n_distinct(id) > 2) %>%
      dplyr::summarise(
        nr_participants = dplyr::n_distinct(id),
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
        mean_memory_immediate_recall_z = mean(priority_memory_im_z, na.rm = TRUE),
        sd_memory_immediate_recall_z = sd(priority_memory_im_z, na.rm = TRUE),
        mean_memory_delayed_recall_z = mean(priority_memory_dr_z, na.rm = TRUE),
        sd_memory_delayed_recall_z = sd(priority_memory_dr_z, na.rm = TRUE),
        mean_priority_language_z = mean(priority_language_z, na.rm = TRUE),
        sd_priority_language_z = sd(priority_language_z, na.rm = TRUE),
        mean_priority_processing_speed_sdst_z = mean(priority_processing_speed_sdst_z, na.rm = TRUE),
        sd_priority_processing_speed_sdst_z = sd(priority_processing_speed_sdst_z, na.rm = TRUE),
        mean_mmse = mean(mmse_total, na.rm = TRUE),
        sd_mmse = sd(mmse_total, na.rm = TRUE),
        mean_apoe = mean(apoe_carrier, na.rm = TRUE),
        sd_apoe = sd(apoe_carrier, na.rm = TRUE)
    )

    #same as above but here the table sorted by sex
    descriptives_by_sex_NPA_table <- df %>%
      dplyr::group_by(sex) %>%
      dplyr::filter(dplyr::n_distinct(id) > 2) %>%
      dplyr::summarise(
        nr_participants = dplyr::n_distinct(id),
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
        years_since_baseline = mean(years_since_baseline, na.rm = TRUE),
        sd_years_since_baseline = sd(years_since_baseline, na.rm = TRUE),
        mean_memory_immediate_recall_z = mean(priority_memory_im_z, na.rm = TRUE),
        sd_memory_immediate_recall_z = sd(priority_memory_im_z, na.rm = TRUE),
        mean_memory_delayed_recall_z = mean(priority_memory_dr_z, na.rm = TRUE),
        sd_memory_delayed_recall_z = sd(priority_memory_dr_z, na.rm = TRUE),
        mean_priority_language_z = mean(priority_language_z, na.rm = TRUE),
        sd_priority_language_z = sd(priority_language_z, na.rm = TRUE),
        mean_priority_processing_speed_sdst_z = mean(priority_processing_speed_sdst_z, na.rm = TRUE),
        sd_priority_processing_speed_sdst_z = sd(priority_processing_speed_sdst_z, na.rm = TRUE)
      )

    #same as above but here the table sorted by years since baseline and sex
   descriptives_by_sex_and_FU_NPA_table <- df %>%
     dplyr::group_by(years_since_baseline, sex) %>%
     dplyr::filter(dplyr::n_distinct(id) > 2) %>%
     dplyr::summarise(
      nr_participants = dplyr::n_distinct(id),
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
      mean_memory_immediate_recall_z = mean(priority_memory_im_z, na.rm = TRUE),
      sd_memory_immediate_recall_z = sd(priority_memory_im_z, na.rm = TRUE),
      mean_memory_delayed_recall_z = mean(priority_memory_dr_z, na.rm = TRUE),
      sd_memory_delayed_recall_z = sd(priority_memory_dr_z, na.rm = TRUE),
      mean_priority_language_z = mean(priority_language_z, na.rm = TRUE),
      sd_priority_language_z = sd(priority_language_z, na.rm = TRUE),
      mean_priority_processing_speed_sdst_z = mean(priority_processing_speed_sdst_z, na.rm = TRUE),
      sd_priority_processing_speed_sdst_z = sd(priority_processing_speed_sdst_z, na.rm = TRUE),
      mean_mmse = mean(mmse_total, na.rm = TRUE),
      sd_mmse = sd(mmse_total, na.rm = TRUE),
      mean_apoe = mean(apoe_carrier, na.rm = TRUE),
      sd_apoe = sd(apoe_carrier, na.rm = TRUE)
    )


    summary_post <- summary_stats(
      df,
      c(
        "p_tau", "amyloid_b_ratio_42_40", "gfap", "nfl", "priority_memory_dr",
        "priority_memory_dr_z", "age_rec", "age_cent", "years_since_baseline",
        "mmse_total", "priority_language_z",
        "memory_delayed_recall_z", "memory_immediate_recall_z"
      )
    )

    if (nrow(df) == 0) {
      return(list(
        "error_message" = "Empty dataset: no participants selected"
      ))
    }
    # RIRS model with unstructured covariance structure (add model for every biomarker x cognitive measure)
    #MMSE
    # vtg::log$info("RIRS_mmse_p_tau")
    # RIRS_mmse_p_tau <- nlme::lme(mmse_total ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + p_tau + p_tau * years_since_baseline,
    #                        data = df,
    #                        random = ~ years_since_baseline | id,
    #                        weights = nlme::varIdent(form= ~1 | years_since_baseline),
    #                        correlation = nlme::corSymm(form = ~1 | id),
    #                        method = "REML",
    #                        na.action = na.exclude,
    #                        control = nlme::lmeControl(opt='optim'))
    # vtg::log$info("RIRS_mmse_gfap")
    # RIRS_mmse_gfap <- nlme::lme(mmse_total ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + gfap + gfap * years_since_baseline,
    #                        data = df,
    #                        random = ~ years_since_baseline | id,
    #                        weights = nlme::varIdent(form= ~1 | years_since_baseline),
    #                        correlation = nlme::corSymm(form = ~1 | id),
    #                        method = "REML",
    #                        na.action = na.exclude,
    #                        control = nlme::lmeControl(opt='optim'))
    # vtg::log$info("RIRS_mmse_nfl")
    # RIRS_mmse_nfl <- nlme::lme(mmse_total ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + nfl + nfl * years_since_baseline,
    #                        data = df,
    #                        random = ~ years_since_baseline | id,
    #                        weights = nlme::varIdent(form= ~1 | years_since_baseline),
    #                        correlation = nlme::corSymm(form = ~1 | id),
    #                        method = "REML",
    #                        na.action = na.exclude,
    #                        control = nlme::lmeControl(opt='optim'))
    # vtg::log$info("RIRS_mmse_amyloid_b_ratio")
    # RIRS_mmse_amyloid_b_ratio <- nlme::lme(mmse_total ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + amyloid_b_ratio + amyloid_b_ratio * years_since_baseline,
    #                        data = df,
    #                        random = ~ years_since_baseline | id,
    #                        weights = nlme::varIdent(form= ~1 | years_since_baseline),
    #                        correlation = nlme::corSymm(form = ~1 | id),
    #                        method = "REML",
    #                        na.action = na.exclude,
    #                        control = nlme::lmeControl(opt='optim'))

    #Immediate recall
    vtg::log$info("RIRS_memory_p_tau_im")
    RIRS_memory_p_tau_im <- nlme::lme(priority_memory_im_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + p_tau + p_tau * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    summary_memory_p_tau_im <- tab_model(RIRS_memory_p_tau_im, p.val = "kr")

    vtg::log$info("RIRS_memory_gfap_im")
    RIRS_memory_gfap_im <- nlme::lme(priority_memory_im_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + gfap + gfap * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    summary_memory_gfap_im <- tab_model(RIRS_memory_gfap_im, p.val = "kr")

    vtg::log$info("RIRS_memory_nfl_im")
    RIRS_memory_nfl_im <- nlme::lme(priority_memory_im_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + nfl + nfl * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    summary_memory_nfl_im <- tab_model(RIRS_memory_nfl_im, p.val = "kr")

    vtg::log$info("RIRS_memory_amyloid_b_ratio_im")
    RIRS_memory_amyloid_b_ratio_im <- nlme::lme(priority_memory_im_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    summary_memory_amyloid_b_ratio_im <- tab_model(RIRS_memory_amyloid_b_ratio_im, p.val = "kr")

    #Delayed recall
    vtg::log$info("RIRS_memory_p_tau_dr")
    RIRS_memory_p_tau_dr <- nlme::lme(priority_memory_dr_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + p_tau + p_tau * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    summary_memory_p_tau_dr <- tab_model(RIRS_memory_p_tau_dr, p.val = "kr")

    vtg::log$info("RIRS_memory_gfap_dr")
    RIRS_memory_gfap_dr <- nlme::lme(priority_memory_dr_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + gfap + gfap * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    summary_memory_gfap_dr <- tab_model(RIRS_memory_gfap_dr, p.val = "kr")

    vtg::log$info("RIRS_memory_nfl_dr")
    RIRS_memory_nfl_dr <- nlme::lme(priority_memory_dr_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + nfl + nfl * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    summary_memory_nfl_dr <- tab_model(RIRS_memory_nfl_dr, p.val = "kr")

    vtg::log$info("RIRS_memory_amyloid_b_ratio_dr")
    RIRS_memory_amyloid_b_ratio_dr <- nlme::lme(priority_memory_dr_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    summary_memory_amyloid_b_ratio_dr <- tab_model(RIRS_memory_amyloid_b_ratio_dr, p.val = "kr")


    #Language
    vtg::log$info("RIRS_language_p_tau")
    RIRS_language_p_tau <- nlme::lme(priority_language_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + p_tau + p_tau * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))

    summary_language_p_tau <- tab_model(RIRS_language_p_tau)

    vtg::log$info("RIRS_language_gfap")
    RIRS_language_gfap <- nlme::lme(priority_language_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + gfap + gfap * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))

    summary_language_gfap <- tab_model(RIRS_language_gfap)

    vtg::log$info("RIRS_language_nfl")
    RIRS_language_nfl <- nlme::lme(priority_language_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + nfl + nfl * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))

    summary_language_nfl <- tab_model(RIRS_language_nfl)

    vtg::log$info("RIRS_language_amyloid_b_ratio")
    RIRS_language_amyloid_b_ratio <- nlme::lme(priority_language_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))

    summary_language_amyloid_b_ratio <- tab_model(RIRS_language_amyloid_b_ratio)

    #processing speed
    vtg::log$info("RIRS_processing_speed_p_tau")
    RIRS_processing_speed_p_tau <- nlme::lme(priority_processing_speed_sdst_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + p_tau + p_tau * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    summary_processing_speed_p_tau <- tab_model(RIRS_processing_speed_p_tau, p.val = "kr")

    vtg::log$info("RIRS_processing_speed_gfap")
    RIRS_processing_speed_gfap <- nlme::lme(priority_processing_speed_sdst_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + gfap + gfap * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    summary_processing_speed_gfap <- tab_model(RIRS_processing_speed_gfap, p.val = "kr")

    vtg::log$info("RIRS_processing_speed_nfl")
    RIRS_processing_speed_nfl <- nlme::lme(priority_processing_speed_sdst_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + nfl + nfl * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    summary_processing_speed_nfl <- tab_model(RIRS_processing_speed_nfl, p.val = "kr")

    vtg::log$info("RIRS_processing_speed_amyloid_b_ratio")
    RIRS_processing_speed_amyloid_b_ratio <- nlme::lme(priority_processing_speed_sdst_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    summary_processing_speed_amyloid_b_ratio <- tab_model(RIRS_processing_speed_amyloid_b_ratio, p.val = "kr")
    
    print(names(RIRS_memory_p_tau_im))
    # model_summary can't extract from lme models
    results <- list(
      "summary_memory_p_tau_im" = summary_memory_p_tau_im,
      "summary_memory_gfap_im" = summary_memory_gfap_im,
      "summary_memory_nfl_im" = summary_memory_nfl_im,
      "summary_memory_amyloid_b_ratio_im" = summary_memory_amyloid_b_ratio_im,
      "summary_memory_p_tau_dr" = summary_memory_p_tau_dr,
      "summary_memory_gfap_dr" = summary_memory_gfap_dr,
      "summary_memory_nfl_dr" = summary_memory_nfl_dr,
      "summary_memory_amyloid_b_ratio_dr" = summary_memory_amyloid_b_ratio_dr,

      "summary_language_p_tau" = summary_language_p_tau,
      "summary_language_gfap" = summary_language_gfap,
      "summary_language_nfl" = summary_language_nfl,
      "summary_language_amyloid_b_ratio" = summary_language_amyloid_b_ratio,

      "summary_processing_speed_p_tau" = summary_processing_speed_p_tau,
      "summary_processing_speed_gfap" = summary_processing_speed_gfap,
      "summary_processing_speed_nfl" = summary_processing_speed_nfl,
      "summary_processing_speed_amyloid_b_ratio" = summary_processing_speed_amyloid_b_ratio,

      "average_FU_time_table" = average_FU_time_table,
      "count_men_and_women_table" = count_men_and_women_table,
      "descriptives_education_table" = descriptives_education_table,
      "descriptives_per_year_table" = descriptives_per_year_table,
      "descriptives_by_sex_table" = descriptives_by_sex_table,
      "descriptives_by_sex_and_FU_table" = descriptives_by_sex_and_FU_table,
      "descriptives_by_sex_NPA_table" = descriptives_by_sex_NPA_table,
      # "descriptives_per_year_NPA_table" = descriptives_per_year_NPA_table,
      "descriptives_by_sex_and_FU_NPA_table" = descriptives_by_sex_and_FU_NPA_table,
      "n" = nrow(df),
      "db" = Sys.getenv("PGDATABASE")
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

