##without apoe
RPC_models_ADC <- function(df, config, model = "memory", exclude=c()) {
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
      by = "id"
      # all.x = T
    )
    df_cogn_test <- df[!is.na(df[[memory_dr_test_name]]),]
    df <- merge(
      x = df_cogn_test[c("id", "date", "priority_memory_dr_15_word_list_correct",
                         "priority_memory_im_15_word_list_correct", "attention_test_tmt_a_time",
                         "priority_executive_tmt_b_time", "priority_language_animal_fluency_60_correct", "mmse_total")],
      y = df_grouped,
      by = "id"
      # all.x = T
    )
    excluded <- unique(df$id[is.na(df$birth_year) | is.na(df$sex)])

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
        count_edu_low = sum(education_low, na.rm = TRUE),
        count_edu_high = sum(education_high, na.rm = TRUE),
        mean_age = mean(age_rec, na.rm = TRUE),
        sd_age = sd(age_rec, na.rm = TRUE),
        count_apoe = sum(apoe_carrier == "yes", na.rm = TRUE)
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
        count_edu_low = sum(education_low, na.rm = TRUE),
        count_edu_high = sum(education_high, na.rm = TRUE),
        mean_age = mean(age_rec, na.rm = TRUE),
        sd_age = sd(age_rec, na.rm = TRUE),
        mean_years_since_baseline = mean(years_since_baseline, na.rm = TRUE),
        sd_years_since_baseline = sd(years_since_baseline, na.rm = TRUE),
        count_apoe = sum(apoe_carrier == "yes", na.rm = TRUE)
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
      count_edu_low = sum(education_low, na.rm = TRUE),
      count_edu_high = sum(education_high, na.rm = TRUE),
      mean_age = mean(age_rec, na.rm = TRUE),
      sd_age = sd(age_rec, na.rm = TRUE),
      count_apoe = sum(apoe_carrier == "yes", na.rm = TRUE)
    )

    #Z-score transformations
    #Z-score: Memory immediate recall
    #used van der Elst for 15 WLT
    if (c("priority_memory_im_15_word_list_correct") %in% colnames(df)) {
      df$priority_memory_im_z <-
        ((df$priority_memory_im_15_word_list_correct - (49.672+ (df$age_cent * -0.247) + (df$age_cent2 * -0.0033) + (df$sex_num * -4.227) + (df$education_low * -3.055) + (df$education_high * 2.496))) / 7.826)
      df$priority_memory_im_z <- pmax(pmin(df$priority_memory_im_z, 5), -5)
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
        "error_message" = paste("Delayed recall test not found 2")
      ))
    }


    #Z-score: language
    #Van der Elst, et al. norms for animal fluency
    if (c("priority_language_animal_fluency_60_correct") %in% colnames(df)) {
      df$priority_language_z <-
        ((df$priority_language_animal_fluency_60_correct - (24.777 +(df$age_cent * -0.097) + (df$education_low * -2.790) + (df$education_high * 1.586))) / 5.797)
      df$priority_language_z <- pmax(pmin(df$priority_language_z, 5), -5)
    } else {
      return(list(
        "error_message" = paste("language test not found, no z-score transformation possible")
      ))
    }

    #Z-score: attention (here we have the TMT and the Stroop)
    ##TMT-A z-scores calculated with NIP manual and excel sheet
    ###education and sex coded differently women = 2, men = 1
    if (c("attention_test_tmt_a_time") %in% colnames(df)) {
      df$sex_tmt <- ifelse(df$sex == 0, 2, df$sex)
      df$age2_cent_tmt <- ((df$age_rec-60)^2)
      df$log10_tmt_a <- log10(df$attention_test_tmt_a_time)
      df$priority_attention_tmt_a_z <-
        ((1.516 + (0.003 * df$age_rec) + (0.00011 * df$age2_cent_tmt) + (-0.082 * df$education_category_verhage) + (0.0008 * (df$age_rec * df$education_category_verhage)) - df$log10_tmt_a)/0.12734)
    }

    #Z-score: executive functioning (Stroop and TMT)
    #TMT b: NIP norms
    ##education and sex coded differently
    if (c("priority_executive_tmt_b_time") %in% colnames(df)) {#sex needs fixing
      df$sex_tmt <- ifelse(df$sex == 0, 2, df$sex)
      df$age2_cent_tmt <- ((df$age_rec-60)^2)
      df$log10_tmt_b <- log10(df$priority_executive_tmt_b_time)
      df$priority_executive_tmt_z <- (((1.686 + (df$age_rec * 0.00788) + (df$age2_cent_tmt * 0.00011) + (df$education_category_verhage* -0.046) + (df$sex_tmt * -0.031)) - df$log10_tmt_b) / 0.14567)

    #TMT shifting: NIP norms
    ##education and sex coded differently
      df$priority_executive_shift_tmt_z <- (((0.983 + (0.555 * df$log10_tmt_a) + (0.0041 * df$age_rec) + (0.00006 * df$age2_cent_tmt) + (-0.03 * df$education_category_verhage) + (-0.028 * df$sex_tmt)) - df$log10_tmt_b) / 0.12729)
    }

    df$education_low <- as.factor(df$education_low)
    df$education_high <- as.factor(df$education_high)

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
        count_edu_low = sum(education_low, na.rm = TRUE),
        count_edu_high = sum(education_high, na.rm = TRUE),
        mean_age = mean(age_rec, na.rm = TRUE),
        sd_age = sd(age_rec, na.rm = TRUE),
        mean_memory_immediate_recall_z = mean(priority_memory_im_z, na.rm = TRUE),
        sd_memory_immediate_recall_z = sd(priority_memory_im_z, na.rm = TRUE),
        mean_memory_delayed_recall_z = mean(priority_memory_dr_z, na.rm = TRUE),
        sd_memory_delayed_recall_z = sd(priority_memory_dr_z, na.rm = TRUE),
        mean_priority_language_z = mean(priority_language_z, na.rm = TRUE),
        sd_priority_language_z = sd(priority_language_z, na.rm = TRUE),
        mean_mmse = mean(mmse_total, na.rm = TRUE),
        sd_mmse = sd(mmse_total, na.rm = TRUE),
        count_apoe = sum(apoe_carrier == 'yes', na.rm = TRUE)
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
        count_edu_low = sum(education_low, na.rm = TRUE),
        count_edu_high = sum(education_high, na.rm = TRUE),
        mean_age = mean(age_rec, na.rm = TRUE),
        sd_age = sd(age_rec, na.rm = TRUE),
        mean_years_since_baseline = mean(years_since_baseline, na.rm = TRUE),
        sd_years_since_baseline = sd(years_since_baseline, na.rm = TRUE),
        mean_memory_immediate_recall_z = mean(priority_memory_im_z, na.rm = TRUE),
        sd_memory_immediate_recall_z = sd(priority_memory_im_z, na.rm = TRUE),
        mean_memory_delayed_recall_z = mean(priority_memory_dr_z, na.rm = TRUE),
        sd_memory_delayed_recall_z = sd(priority_memory_dr_z, na.rm = TRUE),
        mean_priority_language_z = mean(priority_language_z, na.rm = TRUE),
        sd_priority_language_z = sd(priority_language_z, na.rm = TRUE),
        mean_priority_attention_tmt_a_z = mean(priority_attention_tmt_a_z, na.rm = TRUE),
        sd_priority_attention_tmt_a_z = sd(priority_attention_tmt_a_z, na.rm = TRUE),
        mean_priority_executive_tmt_z = mean(priority_executive_tmt_z, na.rm = TRUE),
        sd_priority_executive_tmt_z = sd(priority_executive_tmt_z, na.rm = TRUE),
        mean_mmse = mean(mmse_total, na.rm = TRUE),
        sd_mmse = sd(mmse_total, na.rm = TRUE),
        count_apoe = sum(apoe_carrier == 'yes', na.rm = TRUE)
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
      mean_priority_attention_tmt_a_z = mean(priority_attention_tmt_a_z, na.rm = TRUE),
      sd_priority_attention_tmt_a_z = sd(priority_attention_tmt_a_z, na.rm = TRUE),
      mean_priority_executive_tmt_z = mean(priority_executive_tmt_z, na.rm = TRUE),
      sd_priority_executive_tmt_z = sd(priority_executive_tmt_z, na.rm = TRUE),
      mean_mmse = mean(mmse_total, na.rm = TRUE),
      sd_mmse = sd(mmse_total, na.rm = TRUE),
      count_apoe = sum(apoe_carrier == 'yes', na.rm = TRUE)
    )

    if (nrow(df) == 0) {
      return(list(
        "error_message" = "Empty dataset: no participants selected"
      ))
    }

    # Filter visit years with few patients
    print(table(df$years_since_baseline))
    df <- df %>%
      dplyr::group_by(years_since_baseline) %>%
      dplyr::filter(dplyr::n_distinct(id) > 30)
    print(table(df$years_since_baseline))

    # RIRS model with unstructured covariance structure (add model for every biomarker x cognitive measure)
    #Immediate recall
    vtg::log$info("RIRS_memory_p_tau_im_male")
    RIRS_memory_p_tau_im_male <- nlme::lme(priority_memory_im_z ~ years_since_baseline + sqrt_prior_visit
                                           + age_rec + education_low + education_high + p_tau + p_tau * years_since_baseline,
                                           data = subset(df, sex_num == 0),
                                           random = ~ years_since_baseline | id,
                                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                           correlation = nlme::corSymm(form = ~1 | id),
                                           method = "REML",
                                           na.action = na.exclude,
                                           control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_memory_p_tau_im_male <- sjPlot::tab_model(RIRS_memory_p_tau_im_male, digits = 10)

    vtg::log$info("RIRS_memory_p_tau_im_female")
    RIRS_memory_p_tau_im_female <- nlme::lme(priority_memory_im_z ~ years_since_baseline + sqrt_prior_visit
                                             + age_rec + education_low + education_high + p_tau + p_tau * years_since_baseline,
                                             data = subset(df, sex_num == 1),
                                             random = ~ years_since_baseline | id,
                                             weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                             correlation = nlme::corSymm(form = ~1 | id),
                                             method = "REML",
                                             na.action = na.exclude,
                                             control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_memory_p_tau_im_female <- sjPlot::tab_model(RIRS_memory_p_tau_im_female, digits = 10)

    vtg::log$info("RIRS_memory_gfap_im_male")
    RIRS_memory_gfap_im_male <- nlme::lme(priority_memory_im_z ~ years_since_baseline + sqrt_prior_visit
                                          + age_rec + education_low + education_high + gfap + gfap * years_since_baseline,
                                          data = subset(df, sex_num == 0),
                                          random = ~ years_since_baseline | id,
                                          weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                          correlation = nlme::corSymm(form = ~1 | id),
                                          method = "REML",
                                          na.action = na.exclude,
                                          control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_memory_gfap_im_male <- sjPlot::tab_model(RIRS_memory_gfap_im_male, digits = 10)

    vtg::log$info("RIRS_memory_gfap_im_female")
    RIRS_memory_gfap_im_female <- nlme::lme(priority_memory_im_z ~ years_since_baseline + sqrt_prior_visit
                                            + age_rec + education_low + education_high + gfap + gfap * years_since_baseline,
                                            data = subset(df, sex_num == 1),
                                            random = ~ years_since_baseline | id,
                                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                            correlation = nlme::corSymm(form = ~1 | id),
                                            method = "REML",
                                            na.action = na.exclude,
                                            control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_memory_gfap_im_female <- sjPlot::tab_model(RIRS_memory_gfap_im_female, digits = 10)


    vtg::log$info("RIRS_memory_nfl_im_male")
    RIRS_memory_nfl_im_male <- nlme::lme(priority_memory_im_z ~ years_since_baseline + sqrt_prior_visit
                                         + age_rec + education_low + education_high + nfl + nfl * years_since_baseline,
                                         data = subset(df, sex_num == 0),
                                         random = ~ years_since_baseline | id,
                                         weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                         correlation = nlme::corSymm(form = ~1 | id),
                                         method = "REML",
                                         na.action = na.exclude,
                                         control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_memory_nfl_im_male <- sjPlot::tab_model(RIRS_memory_nfl_im_male, digits = 10)

    vtg::log$info("RIRS_memory_nfl_im_female")
    RIRS_memory_nfl_im_female <- nlme::lme(priority_memory_im_z ~ years_since_baseline + sqrt_prior_visit
                                           + age_rec + education_low + education_high + nfl + nfl * years_since_baseline,
                                           data = subset(df, sex_num == 1),
                                           random = ~ years_since_baseline | id,
                                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                           correlation = nlme::corSymm(form = ~1 | id),
                                           method = "REML",
                                           na.action = na.exclude,
                                           control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_memory_nfl_im_female <- sjPlot::tab_model(RIRS_memory_nfl_im_female, digits = 10)

    vtg::log$info("RIRS_memory_amyloid_b_ratio_im_male")
    RIRS_memory_amyloid_b_ratio_im_male <- nlme::lme(priority_memory_im_z ~ years_since_baseline + sqrt_prior_visit
                                                     + age_rec + education_low + education_high + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                                                     data = subset(df, sex_num == 0),
                                                     random = ~ years_since_baseline | id,
                                                     weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                                     correlation = nlme::corSymm(form = ~1 | id),
                                                     method = "REML",
                                                     na.action = na.exclude,
                                                     control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_memory_amyloid_b_ratio_im_male <- sjPlot::tab_model(RIRS_memory_amyloid_b_ratio_im_male, digits = 10)

    vtg::log$info("RIRS_memory_amyloid_b_ratio_im_female")
    RIRS_memory_amyloid_b_ratio_im_female <- nlme::lme(priority_memory_im_z ~ years_since_baseline + sqrt_prior_visit
                                                       + age_rec + education_low + education_high + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                                                       data = subset(df, sex_num == 1),
                                                       random = ~ years_since_baseline | id,
                                                       weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                                       correlation = nlme::corSymm(form = ~1 | id),
                                                       method = "REML",
                                                       na.action = na.exclude,
                                                       control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_memory_amyloid_b_ratio_im_female <- sjPlot::tab_model(RIRS_memory_amyloid_b_ratio_im_female, digits = 10)

    #Delayed recall
    vtg::log$info("RIRS_memory_p_tau_dr_male")
    RIRS_memory_p_tau_dr_male <- nlme::lme(priority_memory_dr_z ~ years_since_baseline + sqrt_prior_visit
                                           + age_rec + education_low + education_high +
                                             p_tau + p_tau * years_since_baseline,
                                           data = subset(df, sex_num == 0),
                                           random = ~ years_since_baseline | id,
                                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                           correlation = nlme::corSymm(form = ~1 | id),
                                           method = "REML",
                                           na.action = na.exclude,
                                           control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_memory_p_tau_dr_male <- sjPlot::tab_model(RIRS_memory_p_tau_dr_male, digits = 10)

    vtg::log$info("RIRS_memory_p_tau_dr_female")
    RIRS_memory_p_tau_dr_female <- nlme::lme(priority_memory_dr_z ~ years_since_baseline + sqrt_prior_visit
                                             + age_rec + education_low + education_high +
                                               p_tau + p_tau * years_since_baseline,
                                             data = subset(df, sex_num == 1),
                                             random = ~ years_since_baseline | id,
                                             weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                             correlation = nlme::corSymm(form = ~1 | id),
                                             method = "REML",
                                             na.action = na.exclude,
                                             control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_memory_p_tau_dr_female <- sjPlot::tab_model(RIRS_memory_p_tau_dr_female, digits = 10)

    vtg::log$info("RIRS_memory_gfap_dr_male")
    RIRS_memory_gfap_dr_male <- nlme::lme(priority_memory_dr_z ~ years_since_baseline + sqrt_prior_visit
                                          + age_rec + education_low + education_high + gfap + gfap * years_since_baseline,
                                          data = subset(df, sex_num == 0),
                                          random = ~ years_since_baseline | id,
                                          weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                          correlation = nlme::corSymm(form = ~1 | id),
                                          method = "REML",
                                          na.action = na.exclude,
                                          control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_memory_gfap_dr_male <- sjPlot::tab_model(RIRS_memory_gfap_dr_male, digits = 10)

    vtg::log$info("RIRS_memory_gfap_dr_female")
    RIRS_memory_gfap_dr_female <- nlme::lme(priority_memory_dr_z ~ years_since_baseline + sqrt_prior_visit
                                            + age_rec + education_low + education_high + gfap + gfap * years_since_baseline,
                                            data = subset(df, sex_num == 1),
                                            random = ~ years_since_baseline | id,
                                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                            correlation = nlme::corSymm(form = ~1 | id),
                                            method = "REML",
                                            na.action = na.exclude,
                                            control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_memory_gfap_dr_female <- sjPlot::tab_model(RIRS_memory_gfap_dr_female, digits = 10)

    vtg::log$info("RIRS_memory_nfl_dr_male")
    RIRS_memory_nfl_dr_male <- nlme::lme(priority_memory_dr_z ~ years_since_baseline + sqrt_prior_visit
                                         + age_rec + education_low + education_high + nfl + nfl * years_since_baseline,
                                         data = subset(df, sex_num == 0),
                                         random = ~ years_since_baseline | id,
                                         weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                         correlation = nlme::corSymm(form = ~1 | id),
                                         method = "REML",
                                         na.action = na.exclude,
                                         control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_memory_nfl_dr_male <- sjPlot::tab_model(RIRS_memory_nfl_dr_male, digits = 10)

    vtg::log$info("RIRS_memory_nfl_dr_female")
    RIRS_memory_nfl_dr_female <- nlme::lme(priority_memory_dr_z ~ years_since_baseline + sqrt_prior_visit
                                           + age_rec + education_low + education_high + nfl + nfl * years_since_baseline,
                                           data = subset(df, sex_num == 1),
                                           random = ~ years_since_baseline | id,
                                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                           correlation = nlme::corSymm(form = ~1 | id),
                                           method = "REML",
                                           na.action = na.exclude,
                                           control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_memory_nfl_dr_female <- sjPlot::tab_model(RIRS_memory_nfl_dr_female, digits = 10)

    vtg::log$info("RIRS_memory_amyloid_b_ratio_dr_male")
    RIRS_memory_amyloid_b_ratio_dr_male <- nlme::lme(priority_memory_dr_z ~ years_since_baseline + sqrt_prior_visit
                                                     + age_rec + education_low + education_high + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                                                     data = subset(df, sex_num == 0),
                                                     random = ~ years_since_baseline | id,
                                                     weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                                     correlation = nlme::corSymm(form = ~1 | id),
                                                     method = "REML",
                                                     na.action = na.exclude,
                                                     control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_memory_amyloid_b_ratio_dr_male <- sjPlot::tab_model(RIRS_memory_amyloid_b_ratio_dr_male, digits = 10)

    vtg::log$info("RIRS_memory_amyloid_b_ratio_dr_female")
    RIRS_memory_amyloid_b_ratio_dr_female <- nlme::lme(priority_memory_dr_z ~ years_since_baseline + sqrt_prior_visit
                                                       + age_rec + education_low + education_high + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                                                       data = subset(df, sex_num == 1),
                                                       random = ~ years_since_baseline | id,
                                                       weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                                       correlation = nlme::corSymm(form = ~1 | id),
                                                       method = "REML",
                                                       na.action = na.exclude,
                                                       control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_memory_amyloid_b_ratio_dr_female <- sjPlot::tab_model(RIRS_memory_amyloid_b_ratio_dr_female, digits = 10)


    #Language
    vtg::log$info("RIRS_language_p_tau_male")
    RIRS_language_p_tau_male <- nlme::lme(priority_language_z ~ years_since_baseline + sqrt_prior_visit
                                          + age_rec + education_low + education_high + p_tau + p_tau * years_since_baseline,
                                          data = subset(df, sex_num == 0),
                                          random = ~ years_since_baseline | id,
                                          weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                          correlation = nlme::corSymm(form = ~1 | id),
                                          method = "REML",
                                          na.action = na.exclude,
                                          control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_language_p_tau_male <- sjPlot::tab_model(RIRS_language_p_tau_male, digits = 10)

    vtg::log$info("RIRS_language_p_tau_female")
    RIRS_language_p_tau_female <- nlme::lme(priority_language_z ~ years_since_baseline + sqrt_prior_visit
                                            + age_rec + education_low + education_high + p_tau + p_tau * years_since_baseline,
                                            data = subset(df, sex_num == 1),
                                            random = ~ years_since_baseline | id,
                                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                            correlation = nlme::corSymm(form = ~1 | id),
                                            method = "REML",
                                            na.action = na.exclude,
                                            control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_language_p_tau_female <- sjPlot::tab_model(RIRS_language_p_tau_female, digits = 10)

    vtg::log$info("RIRS_language_gfap_male")
    RIRS_language_gfap_male <- nlme::lme(priority_language_z ~ years_since_baseline + sqrt_prior_visit
                                         + age_rec + education_low + education_high + gfap + gfap * years_since_baseline,
                                         data = subset(df, sex_num == 0),
                                         random = ~ years_since_baseline | id,
                                         weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                         correlation = nlme::corSymm(form = ~1 | id),
                                         method = "REML",
                                         na.action = na.exclude,
                                         control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_language_gfap_male <- sjPlot::tab_model(RIRS_language_gfap_male, digits = 10)

    vtg::log$info("RIRS_language_gfap_female")
    RIRS_language_gfap_female <- nlme::lme(priority_language_z ~ years_since_baseline + sqrt_prior_visit
                                           + age_rec + education_low + education_high + gfap + gfap * years_since_baseline,
                                           data = subset(df, sex_num == 1),
                                           random = ~ years_since_baseline | id,
                                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                           correlation = nlme::corSymm(form = ~1 | id),
                                           method = "REML",
                                           na.action = na.exclude,
                                           control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_language_gfap_female <- sjPlot::tab_model(RIRS_language_gfap_female, digits = 10)

    vtg::log$info("RIRS_language_nfl_male")
    RIRS_language_nfl_male <- nlme::lme(priority_language_z ~ years_since_baseline + sqrt_prior_visit
                                        + age_rec + education_low + education_high + nfl + nfl * years_since_baseline,
                                        data = subset(df, sex_num == 0),
                                        random = ~ years_since_baseline | id,
                                        weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                        correlation = nlme::corSymm(form = ~1 | id),
                                        method = "REML",
                                        na.action = na.exclude,
                                        control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_language_nfl_male <- sjPlot::tab_model(RIRS_language_nfl_male, digits = 10)

    vtg::log$info("RIRS_language_nfl_female")
    RIRS_language_nfl_female <- nlme::lme(priority_language_z ~ years_since_baseline + sqrt_prior_visit
                                          + age_rec + education_low + education_high + nfl + nfl * years_since_baseline,
                                          data = subset(df, sex_num == 1),
                                          random = ~ years_since_baseline | id,
                                          weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                          correlation = nlme::corSymm(form = ~1 | id),
                                          method = "REML",
                                          na.action = na.exclude,
                                          control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_language_nfl_female <- sjPlot::tab_model(RIRS_language_nfl_female, digits = 10)

    vtg::log$info("RIRS_language_amyloid_b_ratio_male")
    RIRS_language_amyloid_b_ratio_male <- nlme::lme(priority_language_z ~ years_since_baseline + sqrt_prior_visit
                                                    + age_rec + education_low + education_high + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                                                    data = subset(df, sex_num == 0),
                                                    random = ~ years_since_baseline | id,
                                                    weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                                    correlation = nlme::corSymm(form = ~1 | id),
                                                    method = "REML",
                                                    na.action = na.exclude,
                                                    control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_language_amyloid_b_ratio_male <- sjPlot::tab_model(RIRS_language_amyloid_b_ratio_male, digits = 10)

    vtg::log$info("RIRS_language_amyloid_b_ratio_female")
    RIRS_language_amyloid_b_ratio_female <- nlme::lme(priority_language_z ~ years_since_baseline + sqrt_prior_visit
                                                      + age_rec + education_low + education_high + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                                                      data = subset(df, sex_num == 1),
                                                      random = ~ years_since_baseline | id,
                                                      weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                                      correlation = nlme::corSymm(form = ~1 | id),
                                                      method = "REML",
                                                      na.action = na.exclude,
                                                      control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_language_amyloid_b_ratio_female <- sjPlot::tab_model(RIRS_language_amyloid_b_ratio_female, digits = 10)

    #Attention
     vtg::log$info("RIRS_attention_tmt_p_tau_male")
     RIRS_attention_tmt_p_tau_male <- nlme::lme(priority_attention_tmt_a_z ~ years_since_baseline
                                               + age_rec + sqrt_prior_visit + education_low + education_high + p_tau + p_tau * years_since_baseline,
                                               data = subset(df, sex_numr == "0"),
                                               random = ~ years_since_baseline | id,
                                               weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                               correlation = nlme::corSymm(form = ~1 | id),
                                               method = "REML",
                                               na.action = na.exclude,
                                               control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
     summary_attention_tmt_p_tau_male <- sjPlot::tab_model(RIRS_attention_tmt_p_tau_male, digits = 10)

    vtg::log$info("RIRS_attention_tmt_p_tau_female")
    RIRS_attention_tmt_p_tau_female <- nlme::lme(priority_attention_tmt_a_z ~ years_since_baseline
                                              + age_rec + sqrt_prior_visit + education_low + education_high + p_tau + p_tau * years_since_baseline,
                                              data = subset(df, sex_numr == "1"),
                                              random = ~ years_since_baseline | id,
                                              weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                              correlation = nlme::corSymm(form = ~1 | id),
                                              method = "REML",
                                              na.action = na.exclude,
                                              control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_attention_tmt_p_tau_female <- sjPlot::tab_model(RIRS_attention_tmt_p_tau_female, digits = 10)

    vtg::log$info("RIRS_attention_tmt_gfap_male")
    RIRS_attention_tmt_gfap_male <- nlme::lme(priority_attention_tmt_a_z ~ years_since_baseline
                                             + age_rec + sqrt_prior_visit + education_low + education_high + gfap + gfap * years_since_baseline,
                                             data = subset(df, sex_numr == "0"),
                                             random = ~ years_since_baseline | id,
                                             weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                             correlation = nlme::corSymm(form = ~1 | id),
                                             method = "REML",
                                             na.action = na.exclude,
                                             control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_attention_tmt_gfap_male <- sjPlot::tab_model(RIRS_attention_tmt_gfap_male, digits = 10)

    vtg::log$info("RIRS_attention_tmt_gfap_female")
    RIRS_attention_tmt_gfap_female <- nlme::lme(priority_attention_tmt_a_z ~ years_since_baseline
                                             + age_rec + sqrt_prior_visit + education_low + education_high + gfap + gfap * years_since_baseline,
                                             data = subset(df, sex_numr == "1"),
                                             random = ~ years_since_baseline | id,
                                             weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                             correlation = nlme::corSymm(form = ~1 | id),
                                             method = "REML",
                                             na.action = na.exclude,
                                             control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_attention_tmt_gfap_female <- sjPlot::tab_model(RIRS_attention_tmt_gfap_female, digits = 10)

    vtg::log$info("RIRS_attention_tmt_nfl_male")
    RIRS_attention_tmt_nfl_male <- nlme::lme(priority_attention_tmt_a_z ~ years_since_baseline
                                            + age_rec + sqrt_prior_visit + education_low + education_high + nfl + nfl * years_since_baseline,
                                            data = subset(df, sex_numr == "0"),
                                            random = ~ years_since_baseline | id,
                                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                            correlation = nlme::corSymm(form = ~1 | id),
                                            method = "REML",
                                            na.action = na.exclude,
                                            control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_attention_tmt_nfl_male <- sjPlot::tab_model(RIRS_attention_tmt_nfl_male, digits = 10)

    vtg::log$info("RIRS_attention_tmt_nfl_female")
    RIRS_attention_tmt_nfl_female <- nlme::lme(priority_attention_tmt_a_z ~ years_since_baseline
                                            + age_rec + sqrt_prior_visit + education_low + education_high + nfl + nfl * years_since_baseline,
                                            data = subset(df, sex_numr == "1"),
                                            random = ~ years_since_baseline | id,
                                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                            correlation = nlme::corSymm(form = ~1 | id),
                                            method = "REML",
                                            na.action = na.exclude,
                                            control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_attention_tmt_nfl_female <- sjPlot::tab_model(RIRS_attention_tmt_nfl_female, digits = 10)

    vtg::log$info("RIRS_attention_tmt_amyloid_b_ratio_male")
     RIRS_attention_tmt_amyloid_b_ratio_male <- nlme::lme(priority_attention_tmt_a_z ~ years_since_baseline
                                                         + age_rec + sqrt_prior_visit + education_low + education_high + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                                                         data = subset(df, sex_numr == "0"),
                                                         random = ~ years_since_baseline | id,
                                                         weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                                         correlation = nlme::corSymm(form = ~1 | id),
                                                         method = "REML",
                                                         na.action = na.exclude,
                                                         control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
     summary_attention_tmt_amyloid_b_ratio_male <- sjPlot::tab_model(RIRS_attention_tmt_amyloid_b_ratio_male, digits = 10)

    vtg::log$info("RIRS_attention_tmt_amyloid_b_ratio_female")
    RIRS_attention_tmt_amyloid_b_ratio_female <- nlme::lme(priority_attention_tmt_a_z ~ years_since_baseline
                                                        + age_rec + sqrt_prior_visit + education_low + education_high + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                                                        data = subset(df, sex_numr == "1"),
                                                        random = ~ years_since_baseline | id,
                                                        weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                                        correlation = nlme::corSymm(form = ~1 | id),
                                                        method = "REML",
                                                        na.action = na.exclude,
                                                        control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_attention_tmt_amyloid_b_ratio_female <- sjPlot::tab_model(RIRS_attention_tmt_amyloid_b_ratio_female, digits = 10)

    #Executive function
    vtg::log$info("RIRS_executive_tmt_p_tau_male")
    RIRS_executive_tmt_p_tau_male <- nlme::lme(priority_executive_tmt_z ~ years_since_baseline
                                              + age_rec + sqrt_prior_visit + education_low + education_high + p_tau + p_tau * years_since_baseline,
                                              data = subset(df, sex_numr == "0"),
                                              random = ~ years_since_baseline | id,
                                              weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                              correlation = nlme::corSymm(form = ~1 | id),
                                              method = "REML",
                                              na.action = na.exclude,
                                              control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_executive_tmt_p_tau_male <- sjPlot::tab_model(RIRS_executive_tmt_p_tau_male, digits = 10)

    vtg::log$info("RIRS_executive_tmt_p_tau_female")
    RIRS_executive_tmt_p_tau_female <- nlme::lme(priority_executive_tmt_z ~ years_since_baseline
                                              + age_rec + sqrt_prior_visit + education_low + education_high + p_tau + p_tau * years_since_baseline,
                                              data = subset(df, sex_numr == "1"),
                                              random = ~ years_since_baseline | id,
                                              weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                              correlation = nlme::corSymm(form = ~1 | id),
                                              method = "REML",
                                              na.action = na.exclude,
                                              control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_executive_tmt_p_tau_female <- sjPlot::tab_model(RIRS_executive_tmt_p_tau_female, digits = 10)

    vtg::log$info("RIRS_executive_tmt_gfap_male")
    RIRS_executive_tmt_gfap_male <- nlme::lme(priority_executive_tmt_z ~ years_since_baseline
                                             + age_rec + sqrt_prior_visit + education_low + education_high + gfap + gfap * years_since_baseline,
                                             data = subset(df, sex_numr == "0"),
                                             random = ~ years_since_baseline | id,
                                             weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                             correlation = nlme::corSymm(form = ~1 | id),
                                             method = "REML",
                                             na.action = na.exclude,
                                             control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_executive_tmt_gfap_male <- sjPlot::tab_model(RIRS_executive_tmt_gfap_male, digits = 10)

    vtg::log$info("RIRS_executive_tmt_gfap_female")
    RIRS_executive_tmt_gfap_female <- nlme::lme(priority_executive_tmt_z ~ years_since_baseline
                                             + age_rec + sqrt_prior_visit + education_low + education_high + gfap + gfap * years_since_baseline,
                                             data = subset(df, sex_numr == "1"),
                                             random = ~ years_since_baseline | id,
                                             weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                             correlation = nlme::corSymm(form = ~1 | id),
                                             method = "REML",
                                             na.action = na.exclude,
                                             control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_executive_tmt_gfap_female <- sjPlot::tab_model(RIRS_executive_tmt_gfap_female, digits = 10)

    vtg::log$info("RIRS_executive_tmt_nfl_male")
    RIRS_executive_tmt_nfl_male <- nlme::lme(priority_executive_tmt_z ~ years_since_baseline
                                            + age_rec + sqrt_prior_visit + education_low + education_high + nfl + nfl * years_since_baseline,
                                            data = subset(df, sex_numr == "0"),
                                            random = ~ years_since_baseline | id,
                                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                            correlation = nlme::corSymm(form = ~1 | id),
                                            method = "REML",
                                            na.action = na.exclude,
                                            control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_executive_tmt_nfl_male <- sjPlot::tab_model(RIRS_executive_tmt_nfl_male, digits = 10)

    vtg::log$info("RIRS_executive_tmt_nfl_female")
    RIRS_executive_tmt_nfl_female <- nlme::lme(priority_executive_tmt_z ~ years_since_baseline
                                            + age_rec + sqrt_prior_visit + education_low + education_high + nfl + nfl * years_since_baseline,
                                            data = subset(df, sex_numr == "1"),
                                            random = ~ years_since_baseline | id,
                                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                            correlation = nlme::corSymm(form = ~1 | id),
                                            method = "REML",
                                            na.action = na.exclude,
                                            control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_executive_tmt_nfl_female <- sjPlot::tab_model(RIRS_executive_tmt_nfl_female, digits = 10)

    vtg::log$info("RIRS_executive_tmt_amyloid_b_ratio_male")
    RIRS_executive_tmt_amyloid_b_ratio_male <- nlme::lme(priority_executive_tmt_z ~ years_since_baseline
                                                        + age_rec + sqrt_prior_visit + education_low + education_high + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                                                        data = subset(df, sex_numr == "0"),
                                                        random = ~ years_since_baseline | id,
                                                        weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                                        correlation = nlme::corSymm(form = ~1 | id),
                                                        method = "REML",
                                                        na.action = na.exclude,
                                                        control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_executive_tmt_amyloid_b_ratio_male <- sjPlot::tab_model(RIRS_executive_tmt_amyloid_b_ratio_male, digits = 10)

    vtg::log$info("RIRS_executive_tmt_amyloid_b_ratio_female")
    RIRS_executive_tmt_amyloid_b_ratio_female <- nlme::lme(priority_executive_tmt_z ~ years_since_baseline
                                                        + age_rec + sqrt_prior_visit + education_low + education_high + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                                                        data = subset(df, sex_numr == "1"),
                                                        random = ~ years_since_baseline | id,
                                                        weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                                        correlation = nlme::corSymm(form = ~1 | id),
                                                        method = "REML",
                                                        na.action = na.exclude,
                                                        control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_executive_tmt_amyloid_b_ratio_female <- sjPlot::tab_model(RIRS_executive_tmt_amyloid_b_ratio_female, digits = 10)

    #Interference score
    vtg::log$info("RIRS_executive_tmt_shift_p_tau_male")
    RIRS_executive_tmt_shift_p_tau_male <- nlme::lme(priority_executive_tmt_shift_z ~ years_since_baseline
                                              + age_rec + sqrt_prior_visit + education_low + education_high + p_tau + p_tau * years_since_baseline,
                                              data = subset(df, sex_numr == "0"),
                                              random = ~ years_since_baseline | id,
                                              weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                              correlation = nlme::corSymm(form = ~1 | id),
                                              method = "REML",
                                              na.action = na.exclude,
                                              control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_executive_tmt_shift_p_tau_male <- sjPlot::tab_model(RIRS_executive_tmt_shift_p_tau_male, digits = 10)

    vtg::log$info("RIRS_executive_tmt_shift_p_tau_female")
    RIRS_executive_tmt_shift_p_tau_female <- nlme::lme(priority_executive_tmt_shift_z ~ years_since_baseline
                                              + age_rec + sqrt_prior_visit + education_low + education_high + p_tau + p_tau * years_since_baseline,
                                              data = subset(df, sex_numr == "1"),
                                              random = ~ years_since_baseline | id,
                                              weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                              correlation = nlme::corSymm(form = ~1 | id),
                                              method = "REML",
                                              na.action = na.exclude,
                                              control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_executive_tmt_shift_p_tau_female <- sjPlot::tab_model(RIRS_executive_tmt_shift_p_tau_female, digits = 10)

    vtg::log$info("RIRS_executive_tmt_shift_gfap_male")
    RIRS_executive_tmt_shift_gfap_male <- nlme::lme(priority_executive_tmt_shift_z ~ years_since_baseline
                                             + age_rec + sqrt_prior_visit + education_low + education_high + gfap + gfap * years_since_baseline,
                                             data = subset(df, sex_numr == "0"),
                                             random = ~ years_since_baseline | id,
                                             weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                             correlation = nlme::corSymm(form = ~1 | id),
                                             method = "REML",
                                             na.action = na.exclude,
                                             control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_executive_tmt_shift_gfap_male <- sjPlot::tab_model(RIRS_executive_tmt_shift_gfap_male, digits = 10)

    vtg::log$info("RIRS_executive_tmt_shift_gfap_female")
    RIRS_executive_tmt_shift_gfap_female <- nlme::lme(priority_executive_tmt_shift_z ~ years_since_baseline
                                             + age_rec + sqrt_prior_visit + education_low + education_high + gfap + gfap * years_since_baseline,
                                             data = subset(df, sex_numr == "1"),
                                             random = ~ years_since_baseline | id,
                                             weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                             correlation = nlme::corSymm(form = ~1 | id),
                                             method = "REML",
                                             na.action = na.exclude,
                                             control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_executive_tmt_shift_gfap_female <- sjPlot::tab_model(RIRS_executive_tmt_shift_gfap_female, digits = 10)

    vtg::log$info("RIRS_executive_tmt_shift_nfl_male")
    RIRS_executive_tmt_shift_nfl_male <- nlme::lme(priority_executive_tmt_shift_z ~ years_since_baseline
                                            + age_rec + sqrt_prior_visit + education_low + education_high + nfl + nfl * years_since_baseline,
                                            data = subset(df, sex_numr == "0"),
                                            random = ~ years_since_baseline | id,
                                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                            correlation = nlme::corSymm(form = ~1 | id),
                                            method = "REML",
                                            na.action = na.exclude,
                                            control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_executive_tmt_shift_nfl_male <- sjPlot::tab_model(RIRS_executive_tmt_shift_nfl_male, digits = 10)

    vtg::log$info("RIRS_executive_tmt_shift_nfl_female")
    RIRS_executive_tmt_shift_nfl_female <- nlme::lme(priority_executive_tmt_shift_z ~ years_since_baseline
                                            + age_rec + sqrt_prior_visit + education_low + education_high + nfl + nfl * years_since_baseline,
                                            data = subset(df, sex_numr == "1"),
                                            random = ~ years_since_baseline | id,
                                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                            correlation = nlme::corSymm(form = ~1 | id),
                                            method = "REML",
                                            na.action = na.exclude,
                                            control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_executive_tmt_shift_nfl_female <- sjPlot::tab_model(RIRS_executive_tmt_shift_nfl_female, digits = 10)

    vtg::log$info("RIRS_executive_tmt_shift_amyloid_b_ratio_male")
    RIRS_executive_tmt_shift_amyloid_b_ratio_male <- nlme::lme(priority_executive_tmt_shift_z ~ years_since_baseline
                                                        + age_rec + sqrt_prior_visit + education_low + education_high + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                                                        data = subset(df, sex_numr == "0"),
                                                        random = ~ years_since_baseline | id,
                                                        weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                                        correlation = nlme::corSymm(form = ~1 | id),
                                                        method = "REML",
                                                        na.action = na.exclude,
                                                        control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_executive_tmt_shift_amyloid_b_ratio_male <- sjPlot::tab_model(RIRS_executive_tmt_shift_amyloid_b_ratio_male, digits = 10)

    vtg::log$info("RIRS_executive_tmt_shift_amyloid_b_ratio_female")
    RIRS_executive_tmt_shift_amyloid_b_ratio_female <- nlme::lme(priority_executive_tmt_shift_z ~ years_since_baseline
                                                        + age_rec + sqrt_prior_visit + education_low + education_high + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                                                        data = subset(df, sex_numr == "1"),
                                                        random = ~ years_since_baseline | id,
                                                        weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                                        correlation = nlme::corSymm(form = ~1 | id),
                                                        method = "REML",
                                                        na.action = na.exclude,
                                                        control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    summary_executive_tmt_shift_amyloid_b_ratio_female <- sjPlot::tab_model(RIRS_executive_tmt_shift_amyloid_b_ratio_female, digits = 10)


    results <- list(
      "summary_memory_p_tau_im_male" = summary_memory_p_tau_im_male,
      "summary_memory_gfap_im_male" = summary_memory_gfap_im_male,
      "summary_memory_nfl_im_male" = summary_memory_nfl_im_male,
      "summary_memory_amyloid_b_ratio_im_male" = summary_memory_amyloid_b_ratio_im_male,
      "summary_memory_p_tau_im_female" = summary_memory_p_tau_im_female,
      "summary_memory_gfap_im_female" = summary_memory_gfap_im_female,
      "summary_memory_nfl_im_female" = summary_memory_nfl_im_female,
      "summary_memory_amyloid_b_ratio_im_female" = summary_memory_amyloid_b_ratio_im_female,

      "summary_memory_p_tau_dr_male" = summary_memory_p_tau_dr_male,
      "summary_memory_gfap_dr_male" = summary_memory_gfap_dr_male,
      "summary_memory_nfl_dr_male" = summary_memory_nfl_dr_male,
      "summary_memory_amyloid_b_ratio_dr_male" = summary_memory_amyloid_b_ratio_dr_male,
      "summary_memory_p_tau_dr_female" = summary_memory_p_tau_dr_female,
      "summary_memory_gfap_dr_female" = summary_memory_gfap_dr_female,
      "summary_memory_nfl_dr_female" = summary_memory_nfl_dr_female,
      "summary_memory_amyloid_b_ratio_dr_female" = summary_memory_amyloid_b_ratio_dr_female,

      "summary_language_p_tau_male" = summary_language_p_tau_male,
      "summary_language_gfap_male" = summary_language_gfap_male,
      "summary_language_nfl_male" = summary_language_nfl_male,
      "summary_language_amyloid_b_ratio_male" = summary_language_amyloid_b_ratio_male,
      "summary_language_p_tau_female" = summary_language_p_tau_female,
      "summary_language_gfap_female" = summary_language_gfap_female,
      "summary_language_nfl_female" = summary_language_nfl_female,
      "summary_language_amyloid_b_ratio_female" = summary_language_amyloid_b_ratio_female,

       "summary_attention_tmt_p_tau_male" = summary_attention_tmt_p_tau_male,
      "summary_attention_tmt_p_tau_female" = summary_attention_tmt_p_tau_female,
      "summary_attention_tmt_gfap_male" = summary_attention_tmt_gfap_male,
      "summary_attention_tmt_gfap_female" = summary_attention_tmt_gfap_female,
      "summary_attention_tmt_nfl_male" = summary_attention_tmt_nfl_male,
      "summary_attention_tmt_nfl_female" = summary_attention_tmt_nfl_female,
      "summary_attention_tmt_amyloid_b_ratio_male" = summary_attention_tmt_amyloid_b_ratio_male,
      "summary_attention_tmt_amyloid_b_ratio_female" = summary_attention_tmt_amyloid_b_ratio_female,

      "summary_executive_tmt_p_tau_male" = summary_executive_tmt_p_tau_male,
      "summary_executive_tmt_p_tau_female" = summary_executive_tmt_p_tau_female,
      "summary_executive_tmt_gfap_male" = summary_executive_tmt_gfap_male,
      "summary_executive_tmt_gfap_female" = summary_executive_tmt_gfap_female,
      "summary_executive_tmt_nfl_male" = summary_executive_tmt_nfl_male,
      "summary_executive_tmt_nfl_female" = summary_executive_tmt_nfl_female,
      "summary_executive_tmt_amyloid_b_ratio_male" = summary_executive_tmt_amyloid_b_ratio_male,
      "summary_executive_tmt_amyloid_b_ratio_female" = summary_executive_tmt_amyloid_b_ratio_female,

      "summary_executive_tmt_shift_p_tau_male" = summary_executive_tmt_shift_p_tau_male,
      "summary_executive_tmt_shift_p_tau_female" = summary_executive_tmt_shift_p_tau_female,
      "summary_executive_tmt_shift_gfap_male" = summary_executive_tmt_shift_gfap_male,
      "summary_executive_tmt_shift_gfap_female" = summary_executive_tmt_shift_gfap_female,
      "summary_executive_tmt_shift_nfl_male" = summary_executive_tmt_shift_nfl_male,
      "summary_executive_tmt_shift_nfl_female" = summary_executive_tmt_shift_nfl_female,
      "summary_executive_tmt_shift_amyloid_b_ratio_male" = summary_executive_tmt_shift_amyloid_b_ratio_male,
      "summary_executive_tmt_shift_amyloid_b_ratio_female" = summary_executive_tmt_shift_amyloid_b_ratio_female,


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
