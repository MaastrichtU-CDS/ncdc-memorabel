#This is the overall models or most general models.
##without interactions and without apoe
RPC_models_overall_model <- function(df, config, model = "memory", exclude=c()) {
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
    check_names <- c("age", "sex", "education_category_3", "p_tau", "amyloid_b_ratio_42_40", "gfap", "nfl", "priority_memory_dr_ravlt", "apoe_carrier")
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
    } else {
      return(list(
        "error_message" = paste("Delayed recall test not found - name check")
      ))
    }
    vtg::log$info("Cognitive test available: '{memory_dr_test_name}'")

    df_plasma <- df[!is.na(df$p_tau),]
    df_baseline <- df[!is.na(df$age) & !is.na(df$sex),]
    df_baseline_education <- df[!is.na(df$education_category_verhage),]
    df_apoe <- df[!is.na(df$apoe_carrier),]
    # df_mmse <- df[!is.na(df[["mmse_total"]]) & df$id %in% df_plasma$id & df$id %in% df_baseline$id & df$id %in% df_baseline_education$id & df$id %in% df_apoe$id,]
      # dplyr::group_by(id, date) %>%
      # dplyr::filter(abs(difftime(date, df_plasma[df_plasma$id == id,]$date_plasma)) == min(abs(difftime(date, df_plasma[df_plasma$id == id,]$date_plasma))))
    # df_amyloid <- df[!is.na(df$amyloid_b_ratio_42_40),]

    # education_years - not available in most cohort (included here for now
    # to be available for the summarise function)
    df_plasma$date_plasma <- df_plasma$date
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
      by = "id",
      all.x = T
    )
    df_cogn_test <- df[df$id %in% df_grouped$id & (!is.na(df[["priority_memory_dr_15_word_list_correct"]]) | !is.na(df[["priority_memory_im_15_word_list_correct"]]) | !is.na(df[["mmse_total"]])
                                                   | !is.na(df[["attention_test_tmt_a_time"]]) | !is.na(df[["priority_executive_tmt_b_time"]])
                                                   | !is.na(df[["priority_language_animal_fluency_60_correct"]])),]
    df <- merge(
      x = df_cogn_test[c("id", "date", "priority_memory_dr_15_word_list_correct",
                         "priority_memory_im_15_word_list_correct", "attention_test_tmt_a_time",
                         "priority_executive_tmt_b_time", "priority_language_animal_fluency_60_correct", "mmse_total")],
      y = df_grouped,
      by = "id"
      # all.x = T
    )
    excluded <- unique(df$id[is.na(df$birth_year) | is.na(df$sex)])

    df[df==995.0] <- NA
    df[df==996.0] <- NA
    df[df==997.0] <- NA
    df[df==998.0] <- NA
    df[df==999.0] <- NA

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
      dplyr::filter(days_since_baseline >= -365.25)

    df$years_since_baseline <- as.integer(df$days_since_baseline/365.25, 0)

    df <- subset(df, years_since_baseline >= 0)

    #Create variable for number of follow-ups
    df <- df %>%
      dplyr::arrange(id, years_since_baseline) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(num_prior_visit = dplyr::row_number()-1) %>%
      dplyr::ungroup()

    #Take the square root of the number of follow-ups
    df$sqrt_prior_visit <- sqrt(df$num_prior_visit)

    vtg::log$info("Number of rows in the dataset after baseline subset: '{nrow(df)}'")

    # Age of participant:
    # current_year <- format(Sys.Date(), "%Y")
    # Year of birth will always be available (mandatory in OMOP), age is not guarantee
    df$age_rec <- as.numeric(format(df$date, "%Y")) - df$birth_year

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
    ##change the categorical variable with all subtypes into 2 options, either negative or positive (where positive is E2E4,E3E4,E4E4)
    # df$apoe_carrier <- ifelse(df$apoe_genotype %in% c("E3E4", "E2E4", "E4E4"), 1, 0)
    # df$apoe_carrier <- factor(df$apoe_carrier, levels = c(0, 1), labels = c("no","yes"))
    df$apoe_carrier <- factor(df$apoe_carrier, levels = c(F, T), labels = c("no","yes"))

    # Education levels
    df$education_category_3 <- dplyr::recode(df$education_category_verhage, "1"=0, "2"=0, "3"=0, "4"=1, "5"=1, "6"=2, "7"=2)
    df$education <- factor(df$education_category_3, levels = c(0, 1, 2), labels = c("low", "medium", "high"))

    # dummy variables:
    df$education_low <- ifelse(df$education == 'low', 1, 0)
    df$education_high <- ifelse(df$education == 'high', 1, 0)

    # In the original dataset, this variable may not
    # be associated with the plasma data but only with the visit date
    # May be necessary to first check if amyloid_b_42 and amyloid_b_40 are
    # available. If not available, use amyloid_b_ratio_42_40 directly from
    # the database.
    df$amyloid_b_ratio_42_40 <- ifelse(
      is.na(df$amyloid_b_42) | is.na(df$amyloid_b_40) | df$amyloid_b_40 == 0,
      df$amyloid_b_ratio_42_40,
      df$amyloid_b_42 / df$amyloid_b_40
    )
    df$amyloid_b_ratio <- df$amyloid_b_ratio_42_40

    df$id <- as.factor(as.character(df$id))
    # df %>% dplyr::mutate_if(is.character, as.factor)

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
        count_apoe = sum(apoe_carrier == "yes", na.rm = TRUE)
        # mean_apoe = mean(apoe_carrier, na.rm = TRUE),
        # sd_apoe = sd(apoe_carrier, na.rm = TRUE)
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
        count_apoe = sum(apoe_carrier == "yes", na.rm = TRUE)
        # mean_apoe = mean(apoe_carrier, na.rm = TRUE),
        # sd_apoe = sd(apoe_carrier, na.rm = TRUE)
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
      count_apoe = sum(apoe_carrier == "yes", na.rm = TRUE)
      # mean_apoe = mean(apoe_carrier, na.rm = TRUE),
      # sd_apoe = sd(apoe_carrier, na.rm = TRUE)
    )

     #Z-score transformations
    #Z-score: Picture word learning task (PWLT)
    #  We made our own cohort norm-scores for the PWLT - uses the residual standard error in the function as homoscedasticity is not violated 
    #   women = 1, men = 0
    if (c("priority_memory_im_pwlt") %in% colnames(df)) {
      df$priority_memory_im_z <-
        ((df$priority_memory_im_pwlt - (11.86 + (df$age_cent * -0.07) + (df$sex * 1.41) + (df$education_low * -0.45) + (df$education_high * 0.13))) / 1.981)
      df$priority_memory_im_z <- pmax(pmin(df$priority_memory_im_z, 5), -5)
    } else {
    return(list(
        "error_message" = paste("immediate recall test not found, no z-score transformation possible")
      ))
    }

    #Memory delayed recall z-transformations
    #Z-score: Picture word learning task (PWLT)
    #  We made our own cohort norm-scores for the PWLT - uses the residual standard error in the function as homoscedasticity is not violated 
    #   women = 1, men = 0
    if (memory_dr_test_name == "priority_memory_de_pwlt") {
      df$priority_memory_dr <- df$priority_memory_de_pwlt
      df$priority_memory_dr_z <- (
        df$priority_memory_de_pwlt - (32.52 + (df$age_cent * -0.23) +
           (df$sex * 2.92) + (df$education_low * -1.13)
         + (df$education_high * -0.04))) / 4.481
      df$priority_memory_dr_z <- pmax(pmin(df$priority_memory_dr_z, 5), -5)
    } else {
      return(list(
        "error_message" = paste("Delayed recall test not found")
      ))
    }

        #Z-score: processing speed
    #SDST WAIS-4 norms -> this is for the 120 sec. In LLS we have the 60 sec SDST.
    #  To fix this we time the SDST scores by 2 -> be mindful of this as this could lead to unrealistic scores
    
    attention_test_sdst_120_correct <- attention_test_sdst_60_correct*2
    
    if (c("attention_test_sdst_120_correct") %in% colnames(df)){
      df <- df %>%
        dplyr::mutate(sdst_scaled = dplyr::case_when(
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
          age_rec > 74.92  & attention_test_sdst_120_correct == 20 ~ 2,
          age_rec > 74.92  & attention_test_sdst_120_correct == 21 ~ 3,
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
        dplyr::mutate(priority_processing_speed_sdst_z = dplyr::case_when(
          sdst_scaled <= 1 ~ -3.00,
          sdst_scaled <= 2 ~ -2.67,
          sdst_scaled <= 3 ~ -2.33,
          sdst_scaled <= 4 ~ -2.00,
          sdst_scaled <= 5 ~ -1.67,
          sdst_scaled <= 6 ~ -1.33,
          sdst_scaled <= 7 ~ -1.00,
          sdst_scaled <= 8 ~ -0.67,
          sdst_scaled <= 9 ~ -0.33,
          sdst_scaled <= 10 ~ 0,
          sdst_scaled <= 11 ~ 0.33,
          sdst_scaled <= 12 ~ 0.67,
          sdst_scaled <= 13 ~ 1.00,
          sdst_scaled <= 14 ~ 1.33,
          sdst_scaled <= 15 ~ 1.67,
          sdst_scaled <= 16 ~ 2.00,
          sdst_scaled <= 17 ~ 2.33,
          sdst_scaled <= 18 ~ 2.67,
          sdst_scaled <= 19 ~ 3.00,
          TRUE ~ NA_real_  # Assign NA for other cases
        ))
    } else  {
      print("No measure for processing speed found, no z-score transformation possible")
    }

    ##Stroop: Van der Elst norms
    if (c("attention_test_stroop_1_time") %in% colnames(df) | c("attention_test_stroop_2_time")  %in% colnames(df)) {
      if(c("attention_test_stroop_1_time") %in% colnames(df)) {
        df$priority_attention_stroop_1_pred_score <- (41.517 + (df$age_cent * 0.131) + (df$age_cent2 * 0.003) + (df$education_low * 3.595) + (df$education_high * -1.507))
        df$priority_attention_stroop_1 <- df$attention_test_stroop_1_time
        df <- df %>%  dplyr::rowwise(id) %>% dplyr::mutate(
          priority_attention_stroop_1_z = ifelse(
            priority_attention_stroop_1_pred_score <= 40.209,
            ((attention_test_stroop_1_time - priority_attention_stroop_1_pred_score)/5.961),
            ifelse(
              priority_attention_stroop_1_pred_score >= 40.210 & priority_attention_stroop_1_pred_score <= 43.353,
              ((attention_test_stroop_1_time - priority_attention_stroop_1_pred_score)/6.400),
              ifelse(
                priority_attention_stroop_1_pred_score >= 43.354 & priority_attention_stroop_1_pred_score <= 46.059,
                ((attention_test_stroop_1_time - priority_attention_stroop_1_pred_score)/7.217),
                ((attention_test_stroop_1_time - priority_attention_stroop_1_pred_score)/7.921)
              )
            )
          )
        )
        df$priority_attention_stroop_1_z <- pmax(pmin(df$priority_attention_stroop_1_z, 5), -5)
        df$priority_attention_stroop_1_z <- -df$priority_attention_stroop_1_z
        } else {
        return(list(
          "error_message" = paste("stroop 1 not found")
        ))
      }
      if(c("attention_test_stroop_2_time") %in% colnames(df)) {
        df$priority_attention_stroop_2_pred_score <- (52.468 + (df$age_cent * 0.209) + (df$age_cent2 * 0.007) + (df$sex_num * 2.390) + (df$education_low * 4.235) + (df$education_high * -2.346))
        df$priority_attention_test_stroop_2 <- df$attention_test_stroop_2_time
        df <- df %>%  dplyr::rowwise(id) %>% dplyr::mutate(
          priority_attention_stroop_2_z = ifelse(
            priority_attention_stroop_2_pred_score <= 51.661,
            ((attention_test_stroop_2_time - priority_attention_stroop_2_pred_score)/7.988),
            ifelse(
              priority_attention_stroop_2_pred_score >= 51.662 & priority_attention_stroop_2_pred_score <= 55.861,
              ((attention_test_stroop_2_time - priority_attention_stroop_2_pred_score)/8.459),
              ifelse(
                priority_attention_stroop_2_pred_score >= 55.862 & priority_attention_stroop_2_pred_score <= 60.713,
                ((attention_test_stroop_2_time - priority_attention_stroop_2_pred_score)/9.419),
                ((attention_test_stroop_2_time - priority_attention_stroop_2_pred_score)/10.587)
              )
            )
          )
        )
        df$priority_attention_stroop_2_z <- pmax(pmin(df$priority_attention_stroop_2_z, 5), -5)
        df$priority_attention_stroop_2_z <- -df$priority_attention_stroop_2_z
        } else {
        return(list(
          "error_message" = paste("stroop 2 not found")
        ))
      }
      #make sure that if a value is missing it doesn't just divide the 1 available by 2
      if  (c("priority_attention_stroop_1_z") %in% colnames(df) & c("priority_attention_stroop_2_z") %in% colnames(df)) {
        df$priority_attention_stroop_average_z <- ((df$priority_attention_stroop_1_z + df$priority_attention_stroop_2_z) /2)
      } else {
        return(list(
          "error_message" = paste("either stroop 1 or stroop 2 is missing, no average was calculated")
        ))
      }

    } else {
      return(list(
        "error_message" = paste("attention test not found, no z-score transformation possible")
      ))
    }
     ##Z-score: executive functioning - interference
    ##stroop interference score, van der Elst norms
    df$priority_executive_interf_stroop_pred_score <- (36.066 + (df$age_rec * 0.500) + (df$age_cent2 * 0.016) + (df$sex_num * 3.010) + (df$education_low * 8.505) + (df$education_high * -2.092) + ((df$age_cent * df$education_low)*0.167) + ((df$age_cent * df$education_high)*0.167))
    df$priority_executive_stroop_interf <- (df$priority_executive_stroop_3 -((df$priority_attention_stroop_1 + df$priority_attention_test_stroop_2)/2))
    df <- df %>%  dplyr::rowwise(id) %>% dplyr::mutate(
      priority_executive_stroop_interf_z = ifelse(
        priority_executive_interf_stroop_pred_score <= 34.845,
        ((priority_executive_stroop_interf - priority_executive_interf_stroop_pred_score)/11.037),
        ifelse(
          priority_executive_interf_stroop_pred_score >= 34.846 & priority_executive_interf_stroop_pred_score <= 41.636,
          ((priority_executive_stroop_interf - priority_executive_interf_stroop_pred_score)/12.667),
          ifelse(
            priority_executive_interf_stroop_pred_score >= 41.637 & priority_executive_interf_stroop_pred_score <= 54.849,
            ((priority_executive_stroop_interf - priority_executive_interf_stroop_pred_score)/15.856),
            ((priority_executive_stroop_interf - priority_executive_interf_stroop_pred_score)/22.472)
          )
        )
      )
    )
    df$priority_executive_stroop_interf_z <- pmax(pmin(df$priority_executive_stroop_interf_z, 5), -5)
    df$priority_executive_stroop_interf_z <- -df$priority_executive_stroop_interf_z

    #This makes a table with means and standard deviations for the following variables per days since baseline
    descriptives_per_year_NPA_table <- df %>%
      dplyr::group_by(years_since_baseline) %>%
      dplyr::summarise(
        nr_participants = dplyr::n_distinct(id),
      mean_p_tau = mean(p_tau, na.rm = TRUE),
      sd_p_tau = sd(p_tau, na.rm = TRUE),
      n_p_tau = sum(!is.na(p_tau)),

      mean_amyloid_b_ratio = mean(amyloid_b_ratio_42_40, na.rm = TRUE),
      sd_amyloid_b_ratio = sd(amyloid_b_ratio_42_40, na.rm = TRUE),
      n_amyloid_b_ratio = sum(!is.na(amyloid_b_ratio_42_40)),

      mean_gfap = mean(gfap, na.rm = TRUE),
      sd_gfap = sd(gfap, na.rm = TRUE),
      n_gfap = sum(!is.na(gfap)),

      mean_nfl = mean(nfl, na.rm = TRUE),
      sd_nfl = sd(nfl, na.rm = TRUE),
      n_nfl = sum(!is.na(nfl)),

      high_edu = sum(education == "high"),
      medium_edu = sum(education == "medium"),
      low_edu = sum(education == "low"),

      mean_age = mean(age_rec, na.rm = TRUE),
      sd_age = sd(age_rec, na.rm = TRUE),

      mean_memory_immediate_recall_z = mean(priority_memory_im_z, na.rm = TRUE),
      sd_memory_immediate_recall_z   = sd(priority_memory_im_z, na.rm = TRUE),
      n_memory_immediate_recall_z    = sum(!is.na(priority_memory_im_z)),

      mean_memory_delayed_recall_z = mean(priority_memory_dr_z, na.rm = TRUE),
      sd_memory_delayed_recall_z   = sd(priority_memory_dr_z, na.rm = TRUE),
      n_memory_delayed_recall_z    = sum(!is.na(priority_memory_dr_z)),

      mean_priority_processing_speed_sdst_z = mean(priority_processing_speed_sdst_z, na.rm = TRUE),
      sd_priority_processing_speed_sdst_z   = sd(priority_processing_speed_sdst_z, na.rm = TRUE),
      n_priority_processing_speed_sdst_z    = sum(!is.na(priority_processing_speed_sdst_z)),

      mean_mmse = mean(mmse_total, na.rm = TRUE),
      sd_mmse   = sd(mmse_total, na.rm = TRUE),
      n_mmse    = sum(!is.na(mmse_total)),

      mean_priority_attention_stroop_1_z = mean(priority_attention_stroop_1_z, na.rm = TRUE),
      sd_priority_attention_stroop_1_z = sd(priority_attention_stroop_1_z, na.rm = TRUE),
      n_stroop1    = sum(!is.na(priority_attention_stroop_1_z)),

      mean_priority_attention_stroop_2_z = mean(priority_attention_stroop_2_z, na.rm = TRUE),
      sd_priority_attention_stroop_2_z = sd(priority_attention_stroop_2_z, na.rm = TRUE),
      n_stroop2    = sum(!is.na(priority_attention_stroop_2_z)),

      mean_priority_attention_stroop_average_z = mean(priority_attention_stroop_average_z, na.rm = TRUE),
      sd_priority_attention_stroop_average_z = sd(priority_attention_stroop_average_z, na.rm = TRUE),
      n_stroop_average    = sum(!is.na(priority_attention_stroop_average_z)),

      mean_priority_executive_stroop_3_z = mean(priority_executive_stroop_3_z, na.rm = TRUE),
      sd_priority_executive_stroop_3_z = sd(priority_executive_stroop_3_z, na.rm = TRUE),
      n_stroop_3    = sum(!is.na(priority_executive_stroop_3_z)),

      mean_priority_executive_stroop_interf_z = mean(priority_executive_stroop_interf_z, na.rm = TRUE),
      sd_priority_executive_stroop_interf_z = sd(priority_executive_stroop_interf_z, na.rm = TRUE),
      n_stroop_interf   = sum(!is.na(priority_executive_stroop_interf_z))
      )


    #same as above but here the table sorted by sex
    descriptives_by_sex_NPA_table <- df %>%
      dplyr::group_by(sex) %>%
      dplyr::filter(dplyr::n_distinct(id) > 2) %>%
      dplyr::summarise(
        nr_participants = dplyr::n_distinct(id),
      mean_p_tau = mean(p_tau, na.rm = TRUE),
      sd_p_tau = sd(p_tau, na.rm = TRUE),
      n_p_tau = sum(!is.na(p_tau)),

      mean_amyloid_b_ratio = mean(amyloid_b_ratio_42_40, na.rm = TRUE),
      sd_amyloid_b_ratio = sd(amyloid_b_ratio_42_40, na.rm = TRUE),
      n_amyloid_b_ratio = sum(!is.na(amyloid_b_ratio_42_40)),

      mean_gfap = mean(gfap, na.rm = TRUE),
      sd_gfap = sd(gfap, na.rm = TRUE),
      n_gfap = sum(!is.na(gfap)),

      mean_nfl = mean(nfl, na.rm = TRUE),
      sd_nfl = sd(nfl, na.rm = TRUE),
      n_nfl = sum(!is.na(nfl)),

      high_edu = sum(education == "high"),
      medium_edu = sum(education == "medium"),
      low_edu = sum(education == "low"),

      mean_age = mean(age_rec, na.rm = TRUE),
      sd_age = sd(age_rec, na.rm = TRUE),

      mean_memory_immediate_recall_z = mean(priority_memory_im_z, na.rm = TRUE),
      sd_memory_immediate_recall_z   = sd(priority_memory_im_z, na.rm = TRUE),
      n_memory_immediate_recall_z    = sum(!is.na(priority_memory_im_z)),

      mean_memory_delayed_recall_z = mean(priority_memory_dr_z, na.rm = TRUE),
      sd_memory_delayed_recall_z   = sd(priority_memory_dr_z, na.rm = TRUE),
      n_memory_delayed_recall_z    = sum(!is.na(priority_memory_dr_z)),

      mean_priority_processing_speed_sdst_z = mean(priority_processing_speed_sdst_z, na.rm = TRUE),
      sd_priority_processing_speed_sdst_z   = sd(priority_processing_speed_sdst_z, na.rm = TRUE),
      n_priority_processing_speed_sdst_z    = sum(!is.na(priority_processing_speed_sdst_z)),

      mean_mmse = mean(mmse_total, na.rm = TRUE),
      sd_mmse   = sd(mmse_total, na.rm = TRUE),
      n_mmse    = sum(!is.na(mmse_total)),

      mean_priority_attention_stroop_1_z = mean(priority_attention_stroop_1_z, na.rm = TRUE),
      sd_priority_attention_stroop_1_z = sd(priority_attention_stroop_1_z, na.rm = TRUE),
      n_stroop1    = sum(!is.na(priority_attention_stroop_1_z)),

      mean_priority_attention_stroop_2_z = mean(priority_attention_stroop_2_z, na.rm = TRUE),
      sd_priority_attention_stroop_2_z = sd(priority_attention_stroop_2_z, na.rm = TRUE),
      n_stroop2    = sum(!is.na(priority_attention_stroop_2_z)),

      mean_priority_attention_stroop_average_z = mean(priority_attention_stroop_average_z, na.rm = TRUE),
      sd_priority_attention_stroop_average_z = sd(priority_attention_stroop_average_z, na.rm = TRUE),
      n_stroop_average    = sum(!is.na(priority_attention_stroop_average_z)),

      mean_priority_executive_stroop_3_z = mean(priority_executive_stroop_3_z, na.rm = TRUE),
      sd_priority_executive_stroop_3_z = sd(priority_executive_stroop_3_z, na.rm = TRUE),
      n_stroop_3    = sum(!is.na(priority_executive_stroop_3_z)),

      mean_priority_executive_stroop_interf_z = mean(priority_executive_stroop_interf_z, na.rm = TRUE),
      sd_priority_executive_stroop_interf_z = sd(priority_executive_stroop_interf_z, na.rm = TRUE),
      n_stroop_interf   = sum(!is.na(priority_executive_stroop_interf_z))

      #count_apoe = sum(apoe_carrier == "yes", na.rm = TRUE)
    )

    #same as above but here the table sorted by years since baseline and sex
    descriptives_by_sex_and_FU_NPA_table <- df %>%
      dplyr::group_by(years_since_baseline, sex) %>%
      dplyr::filter(dplyr::n_distinct(id) > 2) %>%
      dplyr::summarise(
        nr_participants = dplyr::n_distinct(id),
      mean_p_tau = mean(p_tau, na.rm = TRUE),
      sd_p_tau = sd(p_tau, na.rm = TRUE),
      n_p_tau = sum(!is.na(p_tau)),

      mean_amyloid_b_ratio = mean(amyloid_b_ratio_42_40, na.rm = TRUE),
      sd_amyloid_b_ratio = sd(amyloid_b_ratio_42_40, na.rm = TRUE),
      n_amyloid_b_ratio = sum(!is.na(amyloid_b_ratio_42_40)),

      mean_gfap = mean(gfap, na.rm = TRUE),
      sd_gfap = sd(gfap, na.rm = TRUE),
      n_gfap = sum(!is.na(gfap)),

      mean_nfl = mean(nfl, na.rm = TRUE),
      sd_nfl = sd(nfl, na.rm = TRUE),
      n_nfl = sum(!is.na(nfl)),

      high_edu = sum(education == "high"),
      medium_edu = sum(education == "medium"),
      low_edu = sum(education == "low"),

      mean_age = mean(age_rec, na.rm = TRUE),
      sd_age = sd(age_rec, na.rm = TRUE),

      mean_memory_immediate_recall_z = mean(priority_memory_im_z, na.rm = TRUE),
      sd_memory_immediate_recall_z   = sd(priority_memory_im_z, na.rm = TRUE),
      n_memory_immediate_recall_z    = sum(!is.na(priority_memory_im_z)),

      mean_memory_delayed_recall_z = mean(priority_memory_dr_z, na.rm = TRUE),
      sd_memory_delayed_recall_z   = sd(priority_memory_dr_z, na.rm = TRUE),
      n_memory_delayed_recall_z    = sum(!is.na(priority_memory_dr_z)),

       mean_priority_processing_speed_sdst_z = mean(priority_processing_speed_sdst_z, na.rm = TRUE),
       sd_priority_processing_speed_sdst_z   = sd(priority_processing_speed_sdst_z, na.rm = TRUE),
       n_priority_processing_speed_sdst_z    = sum(!is.na(priority_processing_speed_sdst_z)),

      mean_mmse = mean(mmse_total, na.rm = TRUE),
      sd_mmse   = sd(mmse_total, na.rm = TRUE),
      n_mmse    = sum(!is.na(mmse_total)),

      mean_priority_attention_stroop_1_z = mean(priority_attention_stroop_1_z, na.rm = TRUE),
      sd_priority_attention_stroop_1_z = sd(priority_attention_stroop_1_z, na.rm = TRUE),
      n_stroop1    = sum(!is.na(priority_attention_stroop_1_z)),

      mean_priority_attention_stroop_2_z = mean(priority_attention_stroop_2_z, na.rm = TRUE),
      sd_priority_attention_stroop_2_z = sd(priority_attention_stroop_2_z, na.rm = TRUE),
      n_stroop2    = sum(!is.na(priority_attention_stroop_2_z)),

      mean_priority_attention_stroop_average_z = mean(priority_attention_stroop_average_z, na.rm = TRUE),
      sd_priority_attention_stroop_average_z = sd(priority_attention_stroop_average_z, na.rm = TRUE),
      n_stroop_average    = sum(!is.na(priority_attention_stroop_average_z)),

      mean_priority_executive_stroop_3_z = mean(priority_executive_stroop_3_z, na.rm = TRUE),
      sd_priority_executive_stroop_3_z = sd(priority_executive_stroop_3_z, na.rm = TRUE),
      n_stroop_3    = sum(!is.na(priority_executive_stroop_3_z)),

      mean_priority_executive_stroop_interf_z = mean(priority_executive_stroop_interf_z, na.rm = TRUE),
      sd_priority_executive_stroop_interf_z = sd(priority_executive_stroop_interf_z, na.rm = TRUE),
      n_stroop_interf   = sum(!is.na(priority_executive_stroop_interf_z))
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
    #Immediate recall
    vtg::log$info("summary_memory_p_tau_im")
    summary_memory_p_tau_im <- safe_lme_summary(priority_memory_im_z ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + apoe_carrier + p_tau
                                      + p_tau * years_since_baseline
                                      + apoe_carrier * p_tau
                                      + apoe_carrier * years_since_baseline
                                      + apoe_carrier * p_tau * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))

    vtg::log$info("summary_memory_gfap_im")
    summary_memory_gfap_im <- safe_lme_summary(priority_memory_im_z ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + apoe_carrier + gfap
                                     + gfap * years_since_baseline
                                     + apoe_carrier * gfap
                                     + apoe_carrier * years_since_baseline
                                     + apoe_carrier * gfap * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))

     vtg::log$info("summary_memory_nfl_im")
     summary_memory_nfl_im <- safe_lme_summary(priority_memory_im_z ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + apoe_carrier + nfl
                                     + nfl * years_since_baseline
                                     + apoe_carrier * nfl
                                     + apoe_carrier * years_since_baseline
                                     + apoe_carrier * nfl * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))

    vtg::log$info("summary_memory_amyloid_b_ratio_im")
    summary_memory_amyloid_b_ratio_im <- safe_lme_summary(priority_memory_im_z ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + apoe_carrier + amyloid_b_ratio_42_40
                                                + amyloid_b_ratio_42_40 * years_since_baseline
                                                + apoe_carrier * amyloid_b_ratio_42_40
                                                + apoe_carrier * years_since_baseline
                                                + apoe_carrier * amyloid_b_ratio_42_40 * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))

    #Delayed recall
    vtg::log$info("summary_memory_p_tau_dr")
    summary_memory_p_tau_dr <- safe_lme_summary(priority_memory_dr_z ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + apoe_carrier + p_tau
                                      + p_tau * years_since_baseline
                                      + apoe_carrier * p_tau
                                      + apoe_carrier * years_since_baseline
                                      + apoe_carrier * p_tau * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))

    vtg::log$info("summary_memory_gfap_dr")
    summary_memory_gfap_dr <- safe_lme_summary(priority_memory_dr_z ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + apoe_carrier + gfap
                                     + gfap * years_since_baseline
                                     + apoe_carrier * gfap
                                     + apoe_carrier * years_since_baseline
                                     + apoe_carrier * gfap * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))

     vtg::log$info("summary_memory_nfl_dr")
     summary_memory_nfl_dr <- safe_lme_summary(priority_memory_dr_z ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + apoe_carrier + nfl
                                     + nfl * years_since_baseline
                                     + apoe_carrier * nfl
                                     + apoe_carrier * years_since_baseline
                                     + apoe_carrier * nfl * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))

    vtg::log$info("summary_memory_amyloid_b_ratio_dr")
    summary_memory_amyloid_b_ratio_dr <- safe_lme_summary(priority_memory_dr_z ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + apoe_carrier + amyloid_b_ratio_42_40
                                                + amyloid_b_ratio_42_40 * years_since_baseline
                                                + apoe_carrier * amyloid_b_ratio_42_40
                                                + apoe_carrier * years_since_baseline
                                                + apoe_carrier * amyloid_b_ratio_42_40 * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))

    #processing speed
    vtg::log$info("summary_processing_speed_p_tau")
    summary_processing_speed_p_tau <- safe_lme_summary(priority_processing_speed_sdst_z ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + apoe_carrier + p_tau
                                             + p_tau * years_since_baseline
                                             + apoe_carrier * p_tau
                                             + apoe_carrier * years_since_baseline
                                             + apoe_carrier * p_tau * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))

    vtg::log$info("summary_processing_speed_gfap")
    summary_processing_speed_gfap <- safe_lme_summary(priority_processing_speed_sdst_z ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + apoe_carrier + gfap
                                     + gfap * years_since_baseline
                                     + apoe_carrier * gfap
                                     + apoe_carrier * years_since_baseline
                                     + apoe_carrier * gfap * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))

     vtg::log$info("summary_processing_speed_nfl")
     summary_processing_speed_nfl <- safe_lme_summary(priority_processing_speed_sdst_z ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + apoe_carrier + apoe_carrier + nfl
                                     + nfl * years_since_baseline
                                     + apoe_carrier * nfl
                                     + apoe_carrier * years_since_baseline
                                     + apoe_carrier * nfl * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))

    vtg::log$info("summary_processing_speed_amyloid_b_ratio")
    summary_processing_speed_amyloid_b_ratio <- safe_lme_summary(priority_processing_speed_sdst_z ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + apoe_carrier + amyloid_b_ratio_42_40
                                                + amyloid_b_ratio_42_40 * years_since_baseline
                                                + apoe_carrier * amyloid_b_ratio_42_40
                                                + apoe_carrier * years_since_baseline
                                                + apoe_carrier * amyloid_b_ratio_42_40 * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))

    #model_summary can't extract from lme models
    results <- list(

      "summary_memory_p_tau_im" = summary_memory_p_tau_im,
      "summary_memory_gfap_im" = summary_memory_gfap_im,
       "summary_memory_nfl_im" = summary_memory_nfl_im,
      "summary_memory_amyloid_b_ratio_im" = summary_memory_amyloid_b_ratio_im,

      "summary_memory_p_tau_dr" = summary_memory_p_tau_dr,
      "summary_memory_gfap_dr" = summary_memory_gfap_dr,
       "summary_memory_nfl_dr" = summary_memory_nfl_dr,
      "summary_memory_amyloid_b_ratio_dr" = summary_memory_amyloid_b_ratio_dr,

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
       "descriptives_per_year_NPA_table" = descriptives_per_year_NPA_table,
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
