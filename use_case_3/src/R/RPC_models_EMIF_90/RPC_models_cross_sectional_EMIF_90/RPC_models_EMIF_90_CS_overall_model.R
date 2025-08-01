RPC_models_EMIF_90_overall_model <- function(df, config, model = "memory", exclude=c()) {
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
    check_names <- c("age", "sex", "education_category_verhage", "p_tau", "amyloid_b_ratio_42_40", "gfap", "nfl", "priority_memory_dr_ravlt")
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
    df_baseline <- df[!is.na(df$birth_year) & !is.na(df$sex),]
    df_baseline_education <- df[!is.na(df$education_category_verhage),]
    df_baseline_education <- df_baseline_education[! duplicated(df_baseline_education$id),]
    df_apoe <- df[!is.na(df$apoe_carrier),]
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
    df_cogn_test <- df[df$id %in% df_grouped$id & (!is.na(df[["attention_test_tmt_a_time"]]) | !is.na(df[["attention_test_sdst_90_ts"]])
      | !is.na(df[["priority_memory_im_cerad"]]) | !is.na(df[["dexterity_clock_drawing"]]) | !is.na(df[["priority_language_animal_fluency_60_correct"]])
      | !is.na(df[["priority_memory_dr_cerad"]]) | !is.na(df[["priority_executive_tmt_b_time"]]) | !is.na(df[["mmse_total"]])),]
    df <- merge(
      x = df_cogn_test[c("id", "date", "attention_test_tmt_a_time", "attention_test_sdst_90_ts",
        "priority_memory_im_cerad", "dexterity_clock_drawing", "priority_language_animal_fluency_60_correct",
        "mmse_total", "priority_memory_dr_cerad", "priority_executive_tmt_b_time")],
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
      dplyr::filter(days_since_baseline >= -365.25)

    df$years_since_baseline <- as.integer(df$days_since_baseline/365.25, 0)

    df <- subset(df, years_since_baseline == 0)

    #Create variable for number of follow-ups
    df <- df %>%
      dplyr::arrange(id, years_since_baseline) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(num_prior_visit = dplyr::row_number()-1) %>%
      dplyr::ungroup()

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
    # df$apoe_carrier <- factor(df$apoe_carrier, levels = c(0, 1), labels = c("no","yes"))
    df$apoe_carrier <- factor(df$apoe_carrier, levels = c(F, T), labels = c("no","yes"))

    # Education levels
    # df$education_category_3 <- ifelse(
    #   is.na(df$education_category_3),
    #   dplyr::recode(df$education_category_verhage, "1"=0, "2"=0, "3"=0, "4"=1, "5"=1, "6"=2, "7"=2),
    #   df$education_category_3
    # )
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
        #mean_edu_years = mean(education_years, na.rm = TRUE),
        #sd_edu_years = sd(education_years, na.rm = TRUE),
        high_edu = sum(education == "high"),
        medium_edu = sum(education == "medium"),
        low_edu = sum(education == "low"),
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
        #mean_edu_years = mean(education_years, na.rm = TRUE),
        #sd_edu_years = sd(education_years, na.rm = TRUE),
        high_edu = sum(education == "high"),
        medium_edu = sum(education == "medium"),
        low_edu = sum(education == "low"),
        mean_age = mean(age_rec, na.rm = TRUE),
        sd_age = sd(age_rec, na.rm = TRUE),
        years_since_baseline = mean(years_since_baseline, na.rm = TRUE),
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
      #mean_edu_years = mean(education_years, na.rm = TRUE),
      #sd_edu_years = sd(education_years, na.rm = TRUE),
      high_edu = sum(education == "high"),
      medium_edu = sum(education == "medium"),
      low_edu = sum(education == "low"),
      mean_age = mean(age_rec, na.rm = TRUE),
      sd_age = sd(age_rec, na.rm = TRUE),
      count_apoe = sum(apoe_carrier == "yes", na.rm = TRUE)
    )


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

    #Z-score: processing speed
    #SDST; Burggraaf et al (2016) norms
    ##education is coded in years for this formula.. this needs to be fixed
    ##sex is coded male=0, female=1
    if (c("attention_test_sdst_90_ts") %in% colnames(df)) {
      df$attention_test_sdst_60 <- df$attention_test_sdst_90_ts * (2/3)
      df$sex_sdst <- ifelse(df$sex_num == 1, 0, 1)
      df$age_cent_sdst <- df$age_rec-46
      df$age_cent_sdst2 <- df$age_cent_sdst^2
      df$priority_processing_speed_sdst_z <-
        ((df$attention_test_sdst_60 - (7.653 + (df$age_cent_sdst * -0.0806) + (df$age_cent_sdst2 * -0.000449) + (df$sex_sdst * -0.470) + (df$education_years))) / 2.777)
      df$priority_processing_speed_sdst <-  df$attention_test_sdst_60
      df$priority_processing_speed_sdst_z <- pmax(pmin(df$priority_processing_speed_sdst_z, 5), -5)
    }
    else  {
      return(list(
        "error_message" = paste("No measure for processing speed found, no z-score transformation possible")
      ))
    }


    #Z-score: attention (here we have the TMT and the Stroop)
    ##TMT-A z-scores calculated with NIP manual and excel sheet
    ###education and sex coded differently women = 2, men = 1
    if (c("attention_test_tmt_a_time") %in% colnames(df)) {
      df$sex_tmt <- ifelse(df$sex_num == 0, 2, df$sex)
      df$age2_cent_tmt <- ((df$age_rec-60)^2)
      df$log10_tmt_a <- log10(df$attention_test_tmt_a_time)
      df$priority_attention_tmt_a_z <-
        ((1.516 + (0.003 * df$age_rec) + (0.00011 * df$age2_cent_tmt) + (-0.082 * df$education_category_verhage) + (0.0008 * (df$age_rec * df$education_category_verhage)) - df$log10_tmt_a)/0.12734)
      df$priority_attention_tmt_a_z <- pmax(pmin(df$priority_attention_tmt_a_z, 5), -5)
      df$priority_attention_tmt_a_z <- -df$priority_attention_tmt_a_z
    }

    #Z-score: executive functioning (Stroop and TMT)
    #TMT b: NIP norms
    ##education and sex coded differently
    if (c("priority_executive_tmt_b_time") %in% colnames(df)) {
      df$sex_tmt <- ifelse(df$sex_num == 0, 2, df$sex)
      df$age2_cent_tmt <- ((df$age_rec-60)^2)
      df$log10_tmt_b <- log10(df$priority_executive_tmt_b_time)
      df$priority_executive_tmt_z <- (((1.686 + (df$age_rec * 0.00788) + (df$age2_cent_tmt * 0.00011) + (df$education_category_verhage* -0.046) + (df$sex_tmt * -0.031)) - df$log10_tmt_b) / 0.14567)
      df$priority_executive_tmt_z <- pmax(pmin(df$priority_executive_tmt_z, 5), -5)
      df$priority_executive_tmt_z <- -df$priority_executive_tmt_z

    #TMT shifting: NIP norms
    ##education and sex coded differently
      df$priority_executive_shift_tmt_z <- (((0.983 + (0.555 * df$log10_tmt_a) + (0.0041 * df$age_rec) + (0.00006 * df$age2_cent_tmt) + (-0.03 * df$education_category_verhage) + (-0.028 * df$sex_tmt)) - df$log10_tmt_b) / 0.12729)
      df$priority_executive_shift_tmt_z <- pmax(pmin(df$priority_executive_shift_tmt_z, 5), -5)
      df$priority_executive_shift_tmt_z <- -df$priority_executive_shift_tmt_z
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
        #mean_edu_years = mean(education_years, na.rm = TRUE),
        #sd_edu_years = sd(education_years, na.rm = TRUE),
        high_edu = sum(education == "high"),
        medium_edu = sum(education == "medium"),
        low_edu = sum(education == "low"),
        mean_age = mean(age_rec, na.rm = TRUE),
        sd_age = sd(age_rec, na.rm = TRUE),
        mean_priority_language_z = mean(priority_language_z, na.rm = TRUE),
        sd_priority_language_z = sd(priority_language_z, na.rm = TRUE),
        mean_priority_processing_speed_sdst_z = mean(priority_processing_speed_sdst_z, na.rm = TRUE),
        sd_priority_processing_speed_sdst_z = sd(priority_processing_speed_sdst_z, na.rm = TRUE),
        mean_priority_attention_tmt_a_z = mean(priority_attention_tmt_a_z, na.rm = TRUE),
        sd_priority_attention_tmt_a_z = sd(priority_attention_tmt_a_z, na.rm = TRUE),
        mean_priority_executive_tmt_z = mean(priority_executive_tmt_z, na.rm = TRUE),
        sd_priority_executive_tmt_z = sd(priority_executive_tmt_z, na.rm = TRUE),
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
        #mean_edu_years = mean(education_years, na.rm = TRUE),
        #sd_edu_years = sd(education_years, na.rm = TRUE),
        high_edu = sum(education == "high"),
        medium_edu = sum(education == "medium"),
        low_edu = sum(education == "low"),
        mean_age = mean(age_rec, na.rm = TRUE),
        sd_age = sd(age_rec, na.rm = TRUE),
        years_since_baseline = mean(years_since_baseline, na.rm = TRUE),
        sd_years_since_baseline = sd(years_since_baseline, na.rm = TRUE),
        mean_priority_language_z = mean(priority_language_z, na.rm = TRUE),
        sd_priority_language_z = sd(priority_language_z, na.rm = TRUE),
        mean_priority_processing_speed_sdst_z = mean(priority_processing_speed_sdst_z, na.rm = TRUE),
        sd_priority_processing_speed_sdst_z = sd(priority_processing_speed_sdst_z, na.rm = TRUE),
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
      #mean_edu_years = mean(education_years, na.rm = TRUE),
      #sd_edu_years = sd(education_years, na.rm = TRUE),
      high_edu = sum(education == "high"),
      medium_edu = sum(education == "medium"),
      low_edu = sum(education == "low"),
      mean_age = mean(age_rec, na.rm = TRUE),
      sd_age = sd(age_rec, na.rm = TRUE),
      mean_priority_language_z = mean(priority_language_z, na.rm = TRUE),
      sd_priority_language_z = sd(priority_language_z, na.rm = TRUE),
      mean_priority_processing_speed_sdst_z = mean(priority_processing_speed_sdst_z, na.rm = TRUE),
      sd_priority_processing_speed_sdst_z = sd(priority_processing_speed_sdst_z, na.rm = TRUE),
      mean_priority_attention_tmt_a_z = mean(priority_attention_tmt_a_z, na.rm = TRUE),
      sd_priority_attention_tmt_a_z = sd(priority_attention_tmt_a_z, na.rm = TRUE),
      mean_priority_executive_tmt_z = mean(priority_executive_tmt_z, na.rm = TRUE),
      sd_priority_executive_tmt_z = sd(priority_executive_tmt_z, na.rm = TRUE),
      mean_mmse = mean(mmse_total, na.rm = TRUE),
      sd_mmse = sd(mmse_total, na.rm = TRUE),
      count_apoe = sum(apoe_carrier == 'yes', na.rm = TRUE)
    )


    summary_post <- summary_stats(
      df,
      c(
        "p_tau", "amyloid_b_ratio_42_40", "gfap", "nfl", "age_rec", "age_cent", "years_since_baseline", "mmse_total"
      )
    )

    if (nrow(df) == 0) {
      return(list(
        "error_message" = "Empty dataset: no participants selected"
      ))
    }


    # CS model with unstructured covariance structure (add model for every biomarker x cognitive measure)
    # RIRS model with unstructured covariance structure (add model for every biomarker x cognitive measure)
    #Language
    vtg::log$info("CS_RIRS_language_p_tau")
    CS_RIRS_language_p_tau <- lm(priority_language_z ~ age_rec + sex + education_low + education_high + p_tau,
                                     data = df,
                                     na.action = na.exclude)
    summary_CS_language_p_tau <- sjPlot::tab_model(CS_RIRS_language_p_tau, digits = 10)

    vtg::log$info("CS_RIRS_language_gfap")
    CS_RIRS_language_gfap <- lm(priority_language_z ~ age_rec + sex + education_low + education_high + gfap,
                                    data = df,
                                    na.action = na.exclude)
    summary_CS_language_gfap <- sjPlot::tab_model(CS_RIRS_language_gfap, digits = 10)

    vtg::log$info("CS_RIRS_language_nfl")
    CS_RIRS_language_nfl <- lm(priority_language_z ~ age_rec + sex + education_low + education_high + nfl,
                                   data = df,
                                   na.action = na.exclude)
    summary_CS_language_nfl <- sjPlot::tab_model(CS_RIRS_language_nfl, digits = 10)

    vtg::log$info("CS_RIRS_language_amyloid_b_ratio")
    CS_RIRS_language_amyloid_b_ratio <- lm(priority_language_z ~ age_rec + sex + education_low + education_high + amyloid_b_ratio_42_40,
                                               data = df,
                                               na.action = na.exclude)
    summary_CS_language_amyloid_b_ratio <- sjPlot::tab_model(CS_RIRS_language_amyloid_b_ratio, digits = 10)

    #processing speed
    vtg::log$info("CS_RIRS_processing_speed_p_tau")
    CS_RIRS_processing_speed_p_tau <- lm(priority_processing_speed_sdst_z ~ age_rec + sex + education_low + education_high + p_tau,
                           data = df,
                           na.action = na.exclude)
    summary_CS_processing_speed_p_tau <- sjPlot::tab_model(CS_RIRS_processing_speed_p_tau, digits = 10)

    vtg::log$info("CS_RIRS_processing_speed_gfap")
    CS_RIRS_processing_speed_gfap <- lm(priority_processing_speed_sdst_z ~ age_rec + sex + education_low + education_high + gfap,
                           data = df,
                           na.action = na.exclude)
    summary_CS_processing_speed_gfap <- sjPlot::tab_model(CS_RIRS_processing_speed_gfap, digits = 10)

    vtg::log$info("CS_RIRS_processing_speed_nfl")
    CS_RIRS_processing_speed_nfl <- lm(priority_processing_speed_sdst_z ~ age_rec + sex + education_low + education_high + nfl,
                           data = df,
                           na.action = na.exclude)
    summary_CS_processing_speed_nfl <- sjPlot::tab_model(CS_RIRS_processing_speed_nfl, digits = 10)

    vtg::log$info("CS_RIRS_processing_speed_amyloid_b_ratio")
    CS_RIRS_processing_speed_amyloid_b_ratio <- lm(priority_processing_speed_sdst_z ~ age_rec + sex + education_low + education_high + amyloid_b_ratio_42_40,
                           data = df,
                           na.action = na.exclude)
    summary_CS_processing_speed_amyloid_b_ratio <- sjPlot::tab_model(CS_RIRS_processing_speed_amyloid_b_ratio, digits = 10)

    #Attention
    vtg::log$info("CS_RIRS_attention_tmt_a_p_tau")
    CS_RIRS_attention_tmt_a_p_tau <- lm(priority_attention_tmt_a_z ~ age_rec + sex + education_low + education_high + p_tau,
                            data = df,
                            na.action = na.exclude)
    summary_CS_attention_tmt_a_p_tau <- sjPlot::tab_model(CS_RIRS_attention_tmt_a_p_tau, digits = 10)

    vtg::log$info("CS_RIRS_attention_tmt_a_gfap")
    CS_RIRS_attention_tmt_a_gfap <- lm(priority_attention_tmt_a_z ~ age_rec + sex + education_low + education_high + gfap,
                            data = df,
                            na.action = na.exclude)
    summary_CS_attention_tmt_a_gfap <- sjPlot::tab_model(CS_RIRS_attention_tmt_a_gfap, digits = 10)

    vtg::log$info("CS_RIRS_attention_tmt_a_nfl")
    CS_RIRS_attention_tmt_a_nfl <- lm(priority_attention_tmt_a_z ~ age_rec + sex + education_low + education_high + nfl,
                            data = df,
                            na.action = na.exclude)
    summary_CS_attention_tmt_a_nfl <- sjPlot::tab_model(CS_RIRS_attention_tmt_a_nfl, digits = 10)

    vtg::log$info("CS_RIRS_attention_tmt_a_amyloid_b_ratio")
    CS_RIRS_attention_tmt_a_amyloid_b_ratio <- lm(priority_attention_tmt_a_z ~ age_rec + sex + education_low + education_high + amyloid_b_ratio_42_40,
                            data = df,
                            na.action = na.exclude)
    summary_CS_attention_tmt_a_amyloid_b_ratio <- sjPlot::tab_model(CS_RIRS_attention_tmt_a_amyloid_b_ratio, digits = 10)

    #Executive function
    vtg::log$info("CS_RIRS_priority_executive_tmt_b_time_p_tau")
    CS_RIRS_priority_executive_tmt_b_p_tau <- lm(priority_executive_tmt_z ~ age_rec + sex + education_low + education_high + p_tau,
                            data = df,
                            na.action = na.exclude)
    summary_CS_priority_executive_tmt_b_p_tau <- sjPlot::tab_model(CS_RIRS_priority_executive_tmt_b_p_tau, digits = 10)

    vtg::log$info("CS_RIRS_priority_executive_tmt_b_time_gfap")
    CS_RIRS_priority_executive_tmt_b_gfap <- lm(priority_executive_tmt_z ~ age_rec + sex + education_low + education_high + gfap,
                            data = df,
                            na.action = na.exclude)
    summary_CS_priority_executive_tmt_b_gfap <- sjPlot::tab_model(CS_RIRS_priority_executive_tmt_b_gfap, digits = 10)

    vtg::log$info("CS_RIRS_priority_executive_tmt_b_time_nfl")
    CS_RIRS_priority_executive_tmt_b_nfl <- lm(priority_executive_tmt_z ~ age_rec + sex + education_low + education_high + nfl,
                            data = df,
                            na.action = na.exclude)
    summary_CS_priority_executive_tmt_b_nfl <- sjPlot::tab_model(CS_RIRS_priority_executive_tmt_b_nfl, digits = 10)

    vtg::log$info("CS_RIRS_priority_executive_tmt_b_time_amyloid_b_ratio")
    CS_RIRS_priority_executive_tmt_b_amyloid_b_ratio <- lm(priority_executive_tmt_z ~ age_rec + sex + education_low + education_high + amyloid_b_ratio_42_40,
                            data = df,
                            na.action = na.exclude)
    summary_CS_priority_executive_tmt_b_amyloid_b_ratio <- sjPlot::tab_model(CS_RIRS_priority_executive_tmt_b_amyloid_b_ratio, digits = 10)

    #Interference score
    vtg::log$info("CS_RIRS_priority_executive_shift_tmt_z_p_tau")
    CS_RIRS_priority_executive_shift_tmt_z_p_tau <- lm(priority_executive_shift_tmt_z ~ age_rec + sex + education_low + education_high + p_tau,
                            data = df,
                            na.action = na.exclude)
    summary_CS_priority_executive_shift_tmt_z_p_tau <- sjPlot::tab_model(CS_RIRS_priority_executive_shift_tmt_z_p_tau, digits = 10)

    vtg::log$info("CS_RIRS_priority_executive_shift_tmt_z_gfap")
    CS_RIRS_priority_executive_shift_tmt_z_gfap <- lm(priority_executive_shift_tmt_z ~ age_rec + sex + education_low + education_high + gfap,
                            data = df,
                            na.action = na.exclude)
    summary_CS_priority_executive_shift_tmt_z_gfap <- sjPlot::tab_model(CS_RIRS_priority_executive_shift_tmt_z_gfap, digits = 10)

    vtg::log$info("CS_RIRS_priority_executive_shift_tmt_z_nfl")
    CS_RIRS_priority_executive_shift_tmt_z_nfl <- lm(priority_executive_shift_tmt_z ~ age_rec + sex + education_low + education_high + nfl,
                            data = df,
                            na.action = na.exclude)
    summary_CS_priority_executive_shift_tmt_z_nfl <- sjPlot::tab_model(CS_RIRS_priority_executive_shift_tmt_z_nfl, digits = 10)

    vtg::log$info("CS_RIRS_priority_executive_shift_tmt_z_amyloid_b_ratio")
    CS_RIRS_priority_executive_shift_tmt_z_amyloid_b_ratio <- lm(priority_executive_shift_tmt_z ~ age_rec + sex + education_low + education_high + amyloid_b_ratio_42_40,
                            data = df,
                            na.action = na.exclude)
    summary_CS_priority_executive_shift_tmt_z_amyloid_b_ratio <- sjPlot::tab_model(CS_RIRS_priority_executive_shift_tmt_z_amyloid_b_ratio, digits = 10)

    #model_summary can't extract from lme models
    results <- list(
      "summary_CS_language_p_tau" = summary_CS_language_p_tau,
      "summary_CS_language_gfap" = summary_CS_language_gfap,
      "summary_CS_language_nfl" = summary_CS_language_nfl,
      "summary_CS_language_amyloid_b_ratio" = summary_CS_language_amyloid_b_ratio,

      "summary_CS_processing_speed_p_tau" = summary_CS_processing_speed_p_tau,
      "summary_CS_processing_speed_gfap" = summary_CS_processing_speed_gfap,
      "summary_CS_processing_speed_nfl" = summary_CS_processing_speed_nfl,
      "summary_CS_processing_speed_amyloid_b_ratio" = summary_CS_processing_speed_amyloid_b_ratio,

      "summary_CS_attention_tmt_a_p_tau" = summary_CS_attention_tmt_a_p_tau,
      "summary_CS_attention_tmt_a_gfap" = summary_CS_attention_tmt_a_gfap,
      "summary_CS_attention_tmt_a_nfl" = summary_CS_attention_tmt_a_nfl,
      "summary_CS_attention_tmt_a_amyloid_b_ratio" = summary_CS_attention_tmt_a_amyloid_b_ratio,

      "summary_CS_priority_executive_tmt_b_p_tau" = summary_CS_priority_executive_tmt_b_p_tau,
      "summary_CS_priority_executive_tmt_b_gfap" = summary_CS_priority_executive_tmt_b_gfap,
      "summary_CS_priority_executive_tmt_b_nfl" = summary_CS_priority_executive_tmt_b_nfl,
      "summary_CS_priority_executive_tmt_b_amyloid_b_ratio" = summary_CS_priority_executive_tmt_b_amyloid_b_ratio,

      "summary_CS_priority_executive_shift_tmt_z_p_tau" = summary_CS_priority_executive_shift_tmt_z_p_tau,
      "summary_CS_priority_executive_shift_tmt_z_gfap" = summary_CS_priority_executive_shift_tmt_z_gfap,
      "summary_CS_priority_executive_shift_tmt_z_nfl" = summary_CS_priority_executive_shift_tmt_z_nfl,
      "summary_CS_priority_executive_shift_tmt_z_amyloid_b_ratio" = summary_CS_priority_executive_shift_tmt_z_amyloid_b_ratio,

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
