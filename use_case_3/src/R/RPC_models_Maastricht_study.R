
RPC_models_Maastricht <- function(df, config, model = "memory", exclude=c()) {
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
    df_cogn_test <- df[!is.na(df[[memory_dr_test_name]]) & df$id %in% df_plasma$id & df$id %in% df_baseline$id,]
    df_mmse <- df[!is.na(df[["mmse_total"]]) & df$id %in% df_plasma$id & df$id %in% df_baseline$id,]
      # dplyr::group_by(id, date) %>%
      # dplyr::filter(abs(difftime(date, df_plasma[df_plasma$id == id,]$date_plasma)) == min(abs(difftime(date, df_plasma[df_plasma$id == id,]$date_plasma))))
    # df_amyloid <- df[!is.na(df$amyloid_b_ratio_42_40),]

    # education_years - not available in most cohort (included here for now
    # to be available for the summarise function)
    df_grouped <- merge(
      x = df_baseline[c("id", "age", "sex", "birth_year", "education_category_3", "education_years")],
      y = df_plasma[c("id", "date_plasma", "p_tau", "gfap", "nfl", "amyloid_b_42", "amyloid_b_40", "amyloid_b_ratio_42_40", "apoe_carrier")],
      by = "id"
    )
    df_grouped <- df_grouped[! duplicated(df_grouped$id),]
    df <- merge(
      x = df_cogn_test[c("id", "date", memory_dr_test_name,
                         "priority_memory_im_ravlt", "attention_test_stroop_1_time",
                         "attention_test_stroop_2_time", "priority_language_animal_fluency_60_correct","priority_attention_test_tmt_a_time", "priority_executive_test_tmt_b_time", "attention_test_ldst_60_correct", "priority_executive_stroop_3_time")],
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
    df$sex_num <- factor(df$sex_num, levels = c(0, 1), labels = c("female", "male"))
    df$sex <- factor(df$sex, levels = c(0, 1), labels = c("male", "female"))

    # Apoe
    df$apoe_carrier <- factor(df$apoe_carrier, levels = c(0, 1), labels = c("no","yes"))

    # Education levels
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
      !(is.na(df$amyloid_b_42) & is.na(df$amyloid_b_40) & df$amyloid_b_40 != 0),
      df$amyloid_b_42 / df$amyloid_b_40,
      df$amyloid_b_ratio_42_40
    )

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
    dplyr::summarise(count = dplyr::n())

    #This makes a table with means and standard deviations for the following variables per days since baseline
    ##(this should become years (I think...))
    ##Here we are missing all the NPA results
    descriptives_per_year_table <- df %>%
      dplyr::group_by(years_since_baseline) %>%
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
    if (c("priority_memory_im_ravlt") %in% colnames(df)) {
      df$priority_memory_im_z <-
        ((df$priority_memory_im_ravlt - (49.672+ (df$age_cent * -0.247) + (df$age_cent2 * -0.0033) + (df$sex_num * -4.227) + (df$education_low * -3.055) + (df$education_high * 2.496))) / 7.826)
    } else {
    return(list(
        "error_message" = paste("immediate recall test not found, no z-score transformation possible")
      ))
    }

    #Memory delayed recall z-transformations
    #used van der Elst for RAVLT
    #used norm scores from ADC for logical memory
    if (memory_dr_test_name == "priority_memory_dr_ravlt") {
      df$priority_memory_dr <- df$priority_memory_dr_ravlt
      df$priority_memory_dr_z <- (
        df$priority_memory_dr_ravlt - (10.924 + (df$age_cent * -0.073) +
          (df$age_cent2 * -0.0009) + (df$sex_num * -1.197) + (df$education_low * -0.844)
         + (df$education_high * 0.424))) / 2.496
    } else {
      return(list(
        "error_message" = paste("Delayed recall test not found")
      ))
    }

    #Z-score: language
   print("Animal Fluency")
   print(sum(is.na(df["priority_language_animal_fluency_60_correct"])))
    #Van der Elst, et al. norms for animal fluency
     if (c("priority_language_animal_fluency_60_correct") %in% colnames(df)) {
    df$priority_language_z <-
      (df$priority_language_animal_fluency_60_correct - (24.777 +(df$age_cent * -0.097) + (df$education_low * -2.790) + (df$education_high * 1.586)) / 5.797)
    } else {
      return(list(
        "error_message" = paste("language test not found, no z-score transformation possible")
      ))
    }

    #Z-score: processing speed
    #LDST; Van der Elst norms - AGE IS NOT CENTERED IN THIS ARTICLE!
    if (c("attention_test_ldst_60_correct") %in% colnames(df)) {
    df$priority_processing_speed_ldst_z <-
      (df$attention_test_ldst_60_correct - (48.27 + (df$age_rec * -0.28) + (df$sex_num * -0.81) + (df$education_low * -4.53) + (df$education_high * 1.12)) / 5.63)
    }  else {
      return(list(
        "error_message" = paste("processing speed test not found, no z-score transformation possible")
      ))
    }

    #Z-score: attention (here we have the TMT and the Stroop)
    ##TMT-A z-scores calculated with NIP manual and excel sheet
    ###education and sex coded differently women = 2, men = 1
    if (c("priority_attention_test_tmt_a_time") %in% colnames(df)) {
      df$sex_tmt <- ifelse(df$sex == 0, 2, df$sex)
      df$age2_cent_tmt <- ((df$age_rec-60)^2)
      df$log10_tmt_a <- log10(df$attention_test_tmt_a_time)
      df$priority_attention_tmt_a_z <-
        ((1.516 + (0.003 * df$age_rec) + (0.00011 * df$age2_cent_tmt) + (-0.082 * df$education_category_verhage) + (0.0008 * (df$age_rec * df$education_category_verhage)) - df$log10_tmt_a)/0.12734)
    }

    ##Stroop: Van der Elst norms
     if (c("attention_test_stroop_1_time") %in% colnames(df) | c("attention_test_stroop_2_time")  %in% colnames(df)) {
       if(c("attention_test_stroop_1") %in% colnames(df)) {
         df$priority_attention_stroop_1_pred_score <- (41.517 + (df$age_cent * 0.131) + (df$age_cent2 * 0.003) + (df$education_low * 3.595) + (df$education_high * -1.507))
         df$priority_attention_stroop_1 <- df$attention_test_stroop_1_time
         if (df$priority_attention_stroop_1_pred_score <= 40.209) {
           df$priority_attention_stroop_1_z <- ((df$attention_test_stroop_1_time - df$priority_attention_stroop_1_pred_score)/5.961)
         } else if (df$priority_attention_stroop_1_pred_score <= 40.210 & df$priority_attention_stroop_1_pred_score >= 43.353) {
           df$priority_attention_stroop_1_z <- ((df$attention_test_stroop_1_time - df$priority_attention_stroop_1_pred_score)/6.400)
         } else if (df$priority_attention_stroop_1_pred_score <= 43.354 & df$priority_attention_stroop_1_pred_score >= 46.059) {
           df$priority_attention_stroop_1_z <- ((df$attention_test_stroop_1_time - df$priority_attention_stroop_1_pred_score)/7.217)
         } else {
           df$priority_attention_stroop_1_z <- ((df$attention_test_stroop_1_time - df$priority_attention_stroop_1_pred_score)/7.921)
         }
       } else {
           return(list(
             "error_message" = paste("stroop 1 not found")
           ))
         }

       if(c("attention_test_stroop_2_time") %in% colnames(df)) {
         df$priority_attention_stroop_2_pred_score <- (52.468 + (df$age_cent * 0.209) + (df$age_cent2 * 0.007) + (df$education_low * 4.235) (df$education_high * -2.346))
         df$priority_attention_test_stroop_2 <- df$attention_test_stroop_2_time
         if (df$priority_attention_stroop_2_pred_score <= 51.661) {
           df$priority_attention_stroop_2_z <- ((df$attention_test_stroop_2_time - df$priority_attention_stroop_2_pred_score)/7.988)
         } else if (df$priority_attention_stroop_2_pred_score <= 51.662 & df$priority_attention_stroop_2_pred_score >= 55.861) {
           df$priority_attention_stroop_2_z <- ((df$attention_test_stroop_2_time - df$priority_attention_stroop_2_pred_score)/8.459)
         } else if (df$priority_attention_stroop_2_pred_score <= 55.862 & df$priority_attention_stroop_2_pred_score >= 60.713) {
           df$priority_attention_stroop_2_z <- ((df$attention_test_stroop_2_time - df$priority_attention_stroop_2_pred_score)/9.419)
         } else {
           df$priority_attention_stroop_2_z <- ((df$attention_test_stroop_2_time - df$priority_attention_stroop_2_pred_score)/10.587)
           }
       } else {
           return(list(
             "error_message" = paste("stroop 2 not found")
           ))
         }

       #make sure that if a value is missing it doesn't just divide the 1 available by 2
       if  (c("priority_attention_stroop_1_z") & c("priority_attention_stroop_2_z") %in% colnames(df)) {
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

    #Z-score: executive functioning (Stroop and TMT)
    #TMT b: NIP norms
    ##education and sex coded differently
    if (c("priority_executive_tmt_b_time") %in% colnames(df)) {#sex needs fixing
      df$sex_tmt <- ifelse(df$sex == 0, 2, df$sex)
      df$age2_cent_tmt <- ((df$age_rec-60)^2)
      df$log10_tmt_b <- log10(df$attention_test_tmt_b_time)
      df$priority_executive_tmt_z <- (((1.686 + (df$age_rec * 0.00788) + (df$age2_cent_tmt * 0.00011) + (df$education_category_verhage* -0.046) + (df$sex_tmt * -0.031)) - df$log10_tmt_b) / 0.14567)

    #TMT shifting: NIP norms
    ##education and sex coded differently
      df$priority_executive_shift_tmt_z <- (((0.983 + (0.555 * df$log10_tmt_a) + (0.0041 * df$age_rec) + (0.00006 * df$age2_cent_tmt) + (-0.03 * df$education_category_verhage) + (-0.028 * df$sex_tmt)) - df$log10_tmt_b) / 0.12729)
    }

    ##Stroop: van der Elst norms
       df$priority_executive_stroop_3_pred_score <- (82.601 + (df$age_rec * 0.714) + (df$age_cent2 * 0.023) + (df$sex_num * 4.470) + (df$education_low * 13.285) + (df$education_high * -3.873))
       df$priority_executive_stroop_3 <- df$priority_executive_stroop_3_time
         if (df$priority_executive_stroop_3_pred_score <= 79.988) {
           df$priority_executive_stroop_3_z <- ((df$priority_executive_stroop_3 - df$priority_executive_stroop_3_pred_score)/13.963)
         } else if (df$priority_executive_stroop_3_pred_score <= 79.989 & df$priority_executive_stroop_3_pred_score >= 92.862) {
           df$priority_executive_stroop_3_z <- ((df$priority_executive_stroop_3 - df$priority_executive_stroop_3_pred_score)/16.367)
         } else if (df$priority_executive_stroop_3_pred_score <= 92.863 & df$priority_executive_stroop_3_pred_score >= 108.585) {
           df$priority_executive_stroop_3_z <- ((df$priority_executive_stroop_3 - df$priority_executive_stroop_3_pred_score)/19.506)
         } else {
           df$priority_executive_stroop_3_z <- ((df$priority_executive_stroop_3 - df$priority_executive_stroop_3_pred_score)/25.936)
           }

    ##Z-score: executive functioning - interference
    ##stroop interference score, van der Elst norms
       df$priority_executive_interf_stroop_pred_score <- (36.066 + (df$age_rec * 0.500) + (df$age_cent2 * 0.016) + (df$sex_num * 3.010) + (df$education_low * 8.505) + (df$education_high * -2.092) + ((df$age_cent * df$education_low)*0.167) + ((df$age_cent * df$education_high)*0.167))
       df$priority_executive_stroop_interf <- (df$priority_executive_stroop_3 -((df$attention_test_stroop_1 - df$attention_test_stroop_2)/2))
       if (df$priority_executive_interf_stroop_pred_score <= 34.845) {
           df$priority_executive_stroop_interf_z <- ((df$priority_executive_stroop_interf - df$priority_executive_stroop_3_pred_score)/11.037)
         } else if (df$priority_executive_interf_stroop_pred_score <= 34.846 & df$priority_executive_interf_stroop_pred_score >= 41.636) {
           df$priority_executive_stroop_interf_z <- ((df$priority_executive_stroop_interf - df$priority_executive_stroop_3_pred_score)/12.667)
         } else if (df$priority_executive_interf_stroop_pred_score <= 41.637 & df$priority_executive_interf_stroop_pred_score >= 54.849) {
           df$priority_executive_stroop_interf_z <- ((df$priority_executive_stroop_interf - df$priority_executive_stroop_3_pred_score)/15.856)
         } else if (df$priority_executive_interf_stroop_pred_score  >= 54.850) {
           df$priority_executive_stroop_interf_z <- ((df$priority_executive_stroop_interf - df$priority_executive_stroop_3_pred_score)/22.472)
         } else {
           return(list(
             "error_message" = paste("executive function test not found, no z-score transformation possible")
           ))
         }

    df$education_low <- as.factor(df$education_low)
    df$education_high <- as.factor(df$education_high)

#This makes a table with means and standard deviations for the following variables per days since baseline
    descriptives_per_year_NPA_table <- df %>%
      dplyr::group_by(years_since_baseline) %>%
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
        mean_priority_processing_speed_ldst_z = mean(priority_processing_speed_ldst_z, na.rm = TRUE),
        sd_priority_processing_speed_ldst_z = sd(priority_processing_speed_ldst_z, na.rm = TRUE),
        mean_priority_attention_stroop_1_z = mean(priority_attention_stroop_1_z, na.rm = TRUE),
        sd_priority_attention_stroop_1_z = sd(priority_attention_stroop_1_z, na.rm = TRUE),
        mean_priority_attention_stroop_2_z = mean(priority_attention_stroop_2_z, na.rm = TRUE),
        sd_priority_attention_stroop_2_z = sd(priority_attention_stroop_2_z, na.rm = TRUE),
        mean_priority_attention_stroop_average_z = mean(priority_attention_stroop_average_z, na.rm = TRUE),
        sd_priority_attention_stroop_average_z = sd(priority_attention_stroop_average_z, na.rm = TRUE),
        mean_priority_executive_stroop_3_z = mean(priority_executive_stroop_3_z, na.rm = TRUE),
        sd_priority_executive_stroop_3_z = sd(priority_executive_stroop_3_z, na.rm = TRUE),
        mean_priority_executive_stroop_interf_z = mean(priority_executive_stroop_interf_z, na.rm = TRUE),
        sd_priority_executive_stroop_interf_z = sd(priority_executive_stroop_interf_z, na.rm = TRUE),
        mean_priority_attention_tmt_a_z = mean(priority_attention_tmt_a_z, na.rm = TRUE),
        sd_priority_attention_tmt_a_z = sd(priority_attention_tmt_a_z, na.rm = TRUE),
        mean_priority_executive_tmt_z = mean(priority_executive_tmt_z, na.rm = TRUE),
        sd_priority_executive_tmt_z = sd(priority_executive_tmt_z, na.rm = TRUE),
        mean_mmse = mean(mmse_total, na.rm = TRUE),
        sd_mmse = sd(mmse_total, na.rm = TRUE),
        mean_apoe = mean(apoe_carrier, na.rm = TRUE),
        sd_apoe = sd(apoe_carrier, na.rm = TRUE)
    )

    #same as above but here the table sorted by sex
    descriptives_by_sex_NPA_table <- df %>%
      dplyr::group_by(sex) %>%
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
        mean_priority_processing_speed_ldst_z = mean(priority_processing_speed_ldst_z, na.rm = TRUE),
        sd_priority_processing_speed_ldst_z = sd(priority_processing_speed_ldst_z, na.rm = TRUE),
        mean_priority_attention_stroop_1_z = mean(priority_attention_stroop_1_z, na.rm = TRUE),
        sd_priority_attention_stroop_1_z = sd(priority_attention_stroop_1_z, na.rm = TRUE),
        mean_priority_attention_stroop_2_z = mean(priority_attention_stroop_2_z, na.rm = TRUE),
        sd_priority_attention_stroop_2_z = sd(priority_attention_stroop_2_z, na.rm = TRUE),
        mean_priority_attention_stroop_average_z = mean(priority_attention_stroop_average_z, na.rm = TRUE),
        sd_priority_attention_stroop_average_z = sd(priority_attention_stroop_average_z, na.rm = TRUE),
        mean_priority_executive_stroop_3_z = mean(priority_executive_stroop_3_z, na.rm = TRUE),
        sd_priority_executive_stroop_3_z = sd(priority_executive_stroop_3_z, na.rm = TRUE),
        mean_priority_executive_stroop_interf_z = mean(priority_executive_stroop_interf_z, na.rm = TRUE),
        sd_priority_executive_stroop_interf_z = sd(priority_executive_stroop_interf_z, na.rm = TRUE),
        mean_priority_attention_tmt_a_z = mean(priority_attention_tmt_a_z, na.rm = TRUE),
        sd_priority_attention_tmt_a_z = sd(priority_attention_tmt_a_z, na.rm = TRUE),
        mean_priority_executive_tmt_z = mean(priority_executive_tmt_z, na.rm = TRUE),
        sd_priority_executive_tmt_z = sd(priority_executive_tmt_z, na.rm = TRUE),
        mean_mmse = mean(mmse_total, na.rm = TRUE),
        sd_mmse = sd(mmse_total, na.rm = TRUE),
        mean_apoe = mean(apoe_carrier, na.rm = TRUE),
        sd_apoe = sd(apoe_carrier, na.rm = TRUE)
      )

    #same as above but here the table sorted by years since baseline and sex
   descriptives_by_sex_and_FU_NPA_table <- df %>%
     dplyr::group_by(years_since_baseline, sex) %>%
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
      mean_priority_processing_speed_ldst_z = mean(priority_processing_speed_ldst_z, na.rm = TRUE),
      sd_priority_processing_speed_ldst_z = sd(priority_processing_speed_ldst_z, na.rm = TRUE),
      mean_priority_attention_stroop_1_z = mean(priority_attention_stroop_1_z, na.rm = TRUE),
      sd_priority_attention_stroop_1_z = sd(priority_attention_stroop_1_z, na.rm = TRUE),
      mean_priority_attention_stroop_2_z = mean(priority_attention_stroop_2_z, na.rm = TRUE),
      sd_priority_attention_stroop_2_z = sd(priority_attention_stroop_2_z, na.rm = TRUE),
      mean_priority_attention_stroop_average_z = mean(priority_attention_stroop_average_z, na.rm = TRUE),
      sd_priority_attention_stroop_average_z = sd(priority_attention_stroop_average_z, na.rm = TRUE),
      mean_priority_executive_stroop_3_z = mean(priority_executive_stroop_3_z, na.rm = TRUE),
      sd_priority_executive_stroop_3_z = sd(priority_executive_stroop_3_z, na.rm = TRUE),
      mean_priority_executive_stroop_interf_z = mean(priority_executive_stroop_interf_z, na.rm = TRUE),
      sd_priority_executive_stroop_interf_z = sd(priority_executive_stroop_interf_z, na.rm = TRUE),
      mean_priority_attention_tmt_a_z = mean(priority_attention_tmt_a_z, na.rm = TRUE),
      sd_priority_attention_tmt_a_z = sd(priority_attention_tmt_a_z, na.rm = TRUE),
      mean_priority_executive_tmt_z = mean(priority_executive_tmt_z, na.rm = TRUE),
      sd_priority_executive_tmt_z = sd(priority_executive_tmt_z, na.rm = TRUE),
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
    RIRS_processing_speed_p_tau <- nlme::lme(priority_processing_speed_ldst_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + p_tau + p_tau * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    summary_processing_speed_p_tau <- tab_model(RIRS_processing_speed_p_tau, p.val = "kr")

    vtg::log$info("RIRS_processing_speed_gfap")
    RIRS_processing_speed_gfap <- nlme::lme(priority_processing_speed_ldst_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + gfap + gfap * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    summary_processing_speed_gfap <- tab_model(RIRS_processing_speed_gfap, p.val = "kr")

    vtg::log$info("RIRS_processing_speed_nfl")
    RIRS_processing_speed_nfl <- nlme::lme(priority_processing_speed_ldst_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + nfl + nfl * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    summary_processing_speed_nfl <- tab_model(RIRS_processing_speed_nfl, p.val = "kr")

    vtg::log$info("RIRS_processing_speed_amyloid_b_ratio")
    RIRS_processing_speed_amyloid_b_ratio <- nlme::lme(priority_processing_speed_ldst_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + amyloid_b_ratio_42_40 + amyloid_b_ratio_42_40 * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    summary_processing_speed_amyloid_b_ratio <- tab_model(RIRS_processing_speed_amyloid_b_ratio, p.val = "kr")


    #Attention
    vtg::log$info("RIRS_attention_stroop_average_p_tau")
    RIRS_attention_stroop_average_p_tau <- nlme::lme(priority_attention_stroop_average_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + p_tau + p_tau * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))

    summary_attention_stroop_average_p_tau <- tab_model(RIRS_attention_stroop_average_p_tau)

    vtg::log$info("RIRS_attention_stroop_average_gfap")
    RIRS_attention_stroop_average_gfap <- nlme::lme(priority_attention_stroop_average_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + gfap + gfap * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))
    summary_attention_stroop_average_gfap <- tab_model(RIRS_attention_stroop_average_gfap)

    vtg::log$info("RIRS_attention_stroop_average_nfl")
    RIRS_attention_stroop_average_nfl <- nlme::lme(priority_attention_stroop_average_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + nfl + nfl * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))
    summary_attention_stroop_average_nfl <- tab_model(RIRS_attention_stroop_average_nfl)

    vtg::log$info("RIRS_attention_stroop_average_amyloid_b_ratio")
    RIRS_attention_stroop_average_amyloid_b_ratio <- nlme::lme(priority_attention_stroop_average_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + amyloid_b_ratio + amyloid_b_ratio * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))

    summary_attention_stroop_average_amyloid_b_ratio <- tab_model(RIRS_attention_stroop_average_amyloid_b_ratio)

    vtg::log$info("RIRS_attention_tmt_a_p_tau")
    RIRS_attention_tmt_a_p_tau <- nlme::lme(priority_attention_tmt_a_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + p_tau + p_tau * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))
    summary_attention_tmt_a_p_tau <- tab_model(RIRS_attention_tmt_a_p_tau)

    vtg::log$info("RIRS_attention_tmt_a_gfap")
    RIRS_attention_tmt_a_gfap <- nlme::lme(priority_attention_tmt_a_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + gfap + gfap * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))
    summary_attention_tmt_a_gfap <- tab_model(RIRS_attention_tmt_a_gfap)

    vtg::log$info("RIRS_attention_tmt_a_nfl")
    RIRS_attention_tmt_a_nfl <- nlme::lme(priority_attention_tmt_a_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + nfl + nfl * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))
    summary_attention_tmt_a_nfl <- tab_model(RIRS_attention_tmt_a_nfl)

    vtg::log$info("RIRS_attention_tmt_a_amyloid_b_ratio")
    RIRS_attention_tmt_a_amyloid_b_ratio <- nlme::lme(priority_attention_tmt_a_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + amyloid_b_ratio + amyloid_b_ratio * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))
    summary_attention_tmt_a_amyloid_b_ratio <- tab_model(RIRS_attention_tmt_a_amyloid_b_ratio)

    #Executive function
    vtg::log$info("RIRS_priority_executive_tmt_b_time_p_tau")
    RIRS_priority_executive_tmt_b_time_p_tau <- nlme::lme(priority_priority_executive_tmt_b_time_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + p_tau + p_tau * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))

    summary_priority_executive_tmt_b_time_p_tau <- tab_model(RIRS_priority_executive_tmt_b_time_p_tau)

    vtg::log$info("RIRS_priority_executive_tmt_b_time_gfap")
    RIRS_priority_executive_tmt_b_time_gfap <- nlme::lme(priority_priority_executive_tmt_b_time_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + gfap + gfap * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))
    summary_priority_executive_tmt_b_time_gfap <- tab_model(RIRS_priority_executive_tmt_b_time_gfap)

    vtg::log$info("RIRS_priority_executive_tmt_b_time_nfl")
    RIRS_priority_executive_tmt_b_time_nfl <- nlme::lme(priority_priority_executive_tmt_b_time_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + nfl + nfl * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))

    summary_priority_executive_tmt_b_time_nfl <- tab_model(RIRS_priority_executive_tmt_b_time_nfl)

    vtg::log$info("RIRS_priority_executive_tmt_b_time_amyloid_b_ratio")
    RIRS_priority_executive_tmt_b_time_amyloid_b_ratio <- nlme::lme(priority_priority_executive_tmt_b_time_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + amyloid_b_ratio + amyloid_b_ratio * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))
    summary_priority_executive_tmt_b_time_amyloid_b_ratio <- tab_model(RIRS_priority_executive_tmt_b_time_amyloid_b_ratio)

    vtg::log$info("RIRS_executive_stroop_3_p_tau")
    RIRS_executive_stroop_3_p_tau <- nlme::lme(priority_executive_stroop_3_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + p_tau + p_tau * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))
    summary_attention_stroop_3_p_tau <- tab_model(RIRS_attention_stroop_3_p_tau)

    vtg::log$info("RIRS_executive_stroop_3_gfap")
    RIRS_executive_stroop_3_gfap <- nlme::lme(priority_executive_stroop_3_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + gfap + gfap * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))
    summary_attention_stroop_3_gfap <- tab_model(RIRS_attention_stroop_3_gfap)

    vtg::log$info("RIRS_executive_stroop_3_nfl")
    RIRS_executive_stroop_3_nfl <- nlme::lme(priority_executive_stroop_3_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + nfl + nfl * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))
    summary_attention_stroop_3_nfl <- tab_model(RIRS_attention_stroop_3_nfl)

    vtg::log$info("RIRS_executive_stroop_3_amyloid_b_ratio")
    RIRS_executive_stroop_3_amyloid_b_ratio <- nlme::lme(priority_executive_stroop_3_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + amyloid_b_ratio + amyloid_b_ratio * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))
    summary_attention_stroop_3_amyloid_b_ratio <- tab_model(RIRS_attention_stroop_3_amyloid_b_ratio)

    # Executive function (Interference)
    vtg::log$info("RIRS_executive_stroop_interf_p_tau")
    RIRS_executive_stroop_interf_p_tau <- nlme::lme(priority_executive_stroop_interf_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + p_tau + p_tau * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))
    summary_attention_stroop_interf_p_tau <- tab_model(RIRS_attention_stroop_interf_p_tau)

    vtg::log$info("RIRS_executive_stroop_interf_gfap")
    RIRS_executive_stroop_interf_gfap <- nlme::lme(priority_executive_stroop_interf_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + gfap + gfap * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))
    summary_attention_stroop_interf_gfap <- tab_model(RIRS_attention_stroop_interf_gfap)

    vtg::log$info("RIRS_executive_stroop_interf_nfl")
    RIRS_executive_stroop_interf_nfl <- nlme::lme(priority_executive_stroop_interf_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + nfl + nfl * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))
    summary_attention_stroop_interf_nfl <- tab_model(RIRS_attention_stroop_interf_nfl)

    vtg::log$info("RIRS_executive_stroop_interf_amyloid_b_ratio")
    RIRS_executive_stroop_interf_amyloid_b_ratio <- nlme::lme(priority_executive_stroop_interf_z ~ years_since_baseline + age_rec + sex + education_low + education_high + apoe_carrier + amyloid_b_ratio + amyloid_b_ratio * years_since_baseline,
                            data = df,
                            random = ~ years_since_baseline | id,
                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
                            correlation = nlme::corSymm(form = ~1 | id),
                            method = "REML",
                            na.action = na.exclude,
                            control = nlme::lmeControl(opt='optim'))
    summary_attention_stroop_interf_amyloid_b_ratio <- tab_model(RIRS_attention_stroop_interf_amyloid_b_ratio)

    print(names(RIRS_memory_p_tau_im))
    # model_summary can't extract from lme models
    results <- list(
      #"model_memory_p_tau_im" = model_summary(RIRS_memory_p_tau_im),
      #"model_memory_gfap_im" = model_summary(RIRS_memory_gfap_im),
      #"model_memory_nfl_im" = model_summary(RIRS_memory_nfl_im),
      #"model_memory_amyloid_b_ratio_im" = model_summary(RIRS_memory_amyloid_b_ratio_im),
      #"model_memory_p_tau_dr" = model_summary(RIRS_memory_p_tau_dr),
      #"model_memory_gfap_dr" = model_summary(RIRS_memory_gfap_dr),
      #"model_memory_nfl_dr" = model_summary(RIRS_memory_nfl_dr),
      #"model_memory_amyloid_b_ratio_dr" = model_summary(RIRS_memory_amyloid_b_ratio_dr),

      "summary_memory_p_tau_im" = summary_memory_p_tau_im,
      "summary_memory_gfap_im" = summary_memory_gfap_im,
      "summary_memory_nfl_im" = summary_memory_nfl_im,
      "summary_memory_amyloid_b_ratio_im" = summary_memory_amyloid_b_ratio_im,
      "summary_memory_p_tau_dr" = summary_memory_p_tau_dr,
      "summary_memory_gfap_dr" = summary_memory_gfap_dr,
      "summary_memory_nfl_dr" = summary_memory_nfl_dr,
      "summary_memory_amyloid_b_ratio_dr" = summary_memory_amyloid_b_ratio_dr,

      # "model_mmse_p_tau" = RIRS_mmse_p_tau[model_info],
      # "model_mmse_gfap" = RIRS_mmse_gfap[model_info],
      # "model_mmse_nfl" = RIRS_mmse_nfl_dr[model_info],
      # "model_mmse_amyloid_b_ratio" = RIRS_mmse_amyloid_b_ratio_dr[model_info],

      #"model_language_p_tau" = model_summary(RIRS_language_p_tau),
      #"model_language_gfap" = model_summary(RIRS_language_gfap),
      #"model_language_nfl" = model_summary(RIRS_language_nfl),
      #"model_language_amyloid_b_ratio" = model_summary(RIRS_language_amyloid_b_ratio),

      "summary_language_p_tau" = summary_language_p_tau,
      "summary_language_gfap" = summary_language_gfap,
      "summary_language_nfl" = summary_language_nfl,
      "summary_language_amyloid_b_ratio" = summary_language_amyloid_b_ratio,

      "summary_processing_speed_p_tau" = summary_processing_speed_p_tau,
      "summary_processing_speed_gfap" = summary_processing_speed_gfap,
      "summary_processing_speed_nfl" = summary_processing_speed_nfl,
      "summary_processing_speed_amyloid_b_ratio" = summary_processing_speed_amyloid_b_ratio,

      # "model_attention_stroop_average_p_tau" = RIRS_attention_stroop_average_p_tau[model_info],
      # "model_attention_stroop_average_gfap" = RIRS_attention_stroop_average_gfap[model_info],
      # "model_attention_stroop_average_nfl" = RIRS_attention_stroop_average_nfl[model_info],
      # "model_attention_stroop_average_amyloid_b_ratio" = RIRS_attention_stroop_average_amyloid_b_ratio_dr[model_info],
      # "model_executive_stroop_3_p_tau" = RIRS_executive_stroop_3_p_tau[model_info],
      # "model_executive_stroop_3_gfap" = RIRS_executive_stroop_3_gfap[model_info],
      # "model_executive_stroop_3_nfl" = RIRS_executive_stroop_3_nfl_dr[model_info],
      # "model_executive_stroop_3_amyloid_b_ratio" = RIRS_executive_stroop_3_amyloid_b_ratio_dr[model_info],
      # "model_executive_stroop_interf_p_tau" = RIRS_executive_stroop_interf_p_tau[model_info],
      # "model_executive_stroop_interf_gfap" = RIRS_executive_stroop_interf_gfap[model_info],
      # "model_executive_stroop_interf_nfl" = RIRS_executive_stroop_interf_nfl_dr[model_info],
      # "model_executive_stroop_interf_amyloid_b_ratio" = RIRS_executive_stroop_interf_amyloid_b_ratio_dr[model_info],

      "summary_attention_stroop_average_p_tau" = summary_attention_stroop_average_p_tau,
      "summary_attention_stroop_average_gfap" = summary_attention_stroop_average_gfap,
      "summary_attention_stroop_average_nfl" = summary_attention_stroop_average_nfl,
      "summary_attention_stroop_average_amyloid_b_ratio" = summary_attention_stroop_average_amyloid_b_ratio,

      "summary_attention_stroop_3_p_tau" = summary_attention_stroop_3_p_tau,
      "summary_attention_stroop_3_gfap" = summary_attention_stroop_3_gfap,
      "summary_attention_stroop_3_nfl" = summary_attention_stroop_3_nfl,
      "summary_attention_stroop_3_amyloid_b_ratio" = summary_attention_stroop_3_amyloid_b_ratio,

      "summary_attention_stroop_interf_p_tau" = summary_attention_stroop_interf_p_tau,
      "summary_attention_stroop_interf_gfap" = summary_attention_stroop_interf_gfap,
      "summary_attention_stroop_interf_nfl" = summary_attention_stroop_interf_nfl,
      "summary_attention_stroop_interf_amyloid_b_ratio" = summary_attention_stroop_interf_amyloid_b_ratio,

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
