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
    df_cogn_test <- df[!is.na(df[[memory_dr_test_name]]) & df$id %in% df_plasma$id & df$id %in% df_baseline$id,]
      # dplyr::group_by(id, date) %>%
      # dplyr::filter(abs(difftime(date, df_plasma[df_plasma$id == id,]$date_plasma)) == min(abs(difftime(date, df_plasma[df_plasma$id == id,]$date_plasma))))
    # df_amyloid <- df[!is.na(df$amyloid_b_ratio_42_40),]

    # education_years - not available in most cohort (included here for now
    # to be available for the summarise function)
    df_grouped <- merge(
      x = df_baseline[c("id", "age", "sex", "birth_year", "education_category_3", "education_years")],
      y = df_plasma[c("id", "date_plasma", "p_tau", "gfap", "nfl", "amyloid_b_42", "amyloid_b_40")],
      by = "id"
    )
    df_grouped <- df_grouped[! duplicated(df_grouped$id),]
    df <- merge(
      x = df_cogn_test[c("id", "date", memory_dr_test_name)],
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
    print(sum(!is.na(df$priority_memory_dr_ravlt)))
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

    # Sex
    df$sex_num <- as.numeric(df$sex) + 1
    df$sex <- factor(df$sex, levels = c(0, 1), labels = c("male", "female"))

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

    print(names(df))
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
        sd_age = sd(age_rec, na.rm = TRUE)
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
        sd_years_since_baseline = sd(years_since_baseline, na.rm = TRUE)
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
      sd_age = sd(age_rec, na.rm = TRUE)
    )

    #Z-score transformations
    #Z-score: Memory immediate recall
    #used van der Elst for RAVLT
    #used norm scores from ADC for logical memory
    if (c("priority_memory_im_ravlt") %in% colnames(df)) {
      df$priority_memory_im_z <- 
        ((df$priority_memory_im_ravlt - (49.672+ (df$age_cent * -0.247) + (df$age_cent2 * -0.0033) + (df$sex * -4.227) + (df$education_low * -3.055) + (df$education_high * 2.496))) / 7.826)
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
    #Van der Elst, et al. norms for animal fluency
     if (c("priority_language_animal_fluency") %in% colnames(df)) {
    df$priority_language_z <- 
      (df$priority_language_animal_fluency - (24.777 +(df$age_cent * -0.097) + (df$education_low * -2.790) + (df$education_high * 1.586)) / 5.797)
    } else {
      return(list(
        "error_message" = paste("language test not found, no z-score transformation possible")
      ))
    }    

    #Z-score: attention
    #Van der Elst norms
    if (c("attention_test_stroop_1_time") | c("attention_test_stroop_2_time") %in% colnames(df)) {
      if(c("attention_test_stroop_1") %in% colnames(df)) {
        df$priority_attention_stroop_1_pred_score <- (41.517 + (df$age_cent * 0.131) + (df$age_cent2 * 0.003) + (df$education_low * 3.595) + (df$education_high * -1.507))
        df$priority_attention_stroop_1 <- df$attention_test_stroop_1_time
        if df$priority_attention_stroop_1_pred_score <= 40.209 {
          df$priority_attention_stroop_1_z <- ((df$attention_test_stroop_1_time - df$priority_attention_stroop_1_pred_score)/5.961)
        } else if df$priority_attention_stroop_1_pred_score <= 40.210 & >= 43.353 {
          df$priority_attention_stroop_1_z <- ((df$attention_test_stroop_1_time - df$priority_attention_stroop_1_pred_score)/6.400)
        } else if df$priority_attention_stroop_1_pred_score <= 43.354 & >= 46.059 {
          df$priority_attention_stroop_1_z <- ((df$attention_test_stroop_1_time - df$priority_attention_stroop_1_pred_score)/7.217)
        } else df$priority_attention_stroop_1_pred_score  >= 46.060 {
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
        if df$priority_attention_stroop_2_pred_score <= 51.661 {
          df$priority_attention_stroop_2_z <- ((df$attention_test_stroop_2_time - df$priority_attention_stroop_2_pred_score)/7.988)
        } else if df$priority_attention_stroop_2_pred_score <= 51.662 & >= 55.861 {
          df$priority_attention_stroop_2_z <- ((df$attention_test_stroop_2_time - df$priority_attention_stroop_2_pred_score)/8.459)
        } else if df$priority_attention_stroop_2_pred_score <= 55.862 & >= 60.713 {
          df$priority_attention_stroop_2_z <- ((df$attention_test_stroop_2_time - df$priority_attention_stroop_2_pred_score)/9.419)
        } else df$priority_attention_stroop_2_pred_score  >= 60.714 {
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

    #Z-score: executive functioning
    #van der Elst norms
      df$priority_executive_stroop_3_pred_score <- (82.601 + (df$age_rec * 0.714) + (df$age_cent2 * 0.023) + (df$sex * 4.470) + (df$education_low * 13.285) + (df$education_high * -3.873))
      df$priority_executive_stroop_3 <- df$priority_executive_stroop_3_time
        if df$priority_executive_stroop_3_pred_score <= 79.988 {
          df$priority_executive_stroop_3_z <- ((df$priority_executive_stroop_3 - df$priority_executive_stroop_3_pred_score)/13.963)
        } else if df$priority_executive_stroop_3_pred_score <= 79.989 & >= 92.862 {
          df$priority_executive_stroop_3_z <- ((df$priority_executive_stroop_3 - df$priority_executive_stroop_3_pred_score)/16.367)
        } else if df$priority_executive_stroop_3_pred_score <= 92.863 & >= 108.585 {
          df$priority_executive_stroop_3_z <- ((df$priority_executive_stroop_3 - df$priority_executive_stroop_3_pred_score)/19.506)
        } else df$priority_executive_stroop_3_pred_score  >= 108.586 {
          df$priority_executive_stroop_3_z <- ((df$priority_executive_stroop_3 - df$priority_executive_stroop_3_pred_score)/25.936)
          }

    #Z-score: executive functioning - interference 
    #stroop interference score, van der Elst norms
      df$priority_executive_interf_stroop_pred_score <- (36.066 + (df$age_rec * 0.500) + (df$age_cent2 * 0.016) + (df$sex * 3.010) + (df$education_low * 8.505) + (df$education_high * -2.092) + ((df$age_cent * df$education_low)*0.167) + ((df$age_cent * df$education_high)*0.167))
      df$priority_executive_stroop_interf <- (df$priority_executive_stroop_3 -((df$attention_test_stroop_1 - df$attention_test_stroop_2)/2))
      if df$priority_executive_interf_stroop_pred_score <= 34.845 {
          df$priority_executive_stroop_interf_z <- ((df$priority_executive_stroop_interf - df$priority_executive_stroop_3_pred_score)/11.037)
        } else if df$priority_executive_interf_stroop_pred_score <= 34.846 & >= 41.636 {
          df$priority_executive_stroop_interf_z <- ((df$priority_executive_stroop_interf - df$priority_executive_stroop_3_pred_score)/12.667)
        } else if df$priority_executive_interf_stroop_pred_score <= 41.637 & >= 54.849 {
          df$priority_executive_stroop_interf_z <- ((df$priority_executive_stroop_interf - df$priority_executive_stroop_3_pred_score)/15.856)
        } else if df$priority_executive_interf_stroop_pred_score  >= 54.850 {
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
        mean_mmse = mean(mmse, na.rm = TRUE),
        sd_mmse = sd(mmse, na.rm = TRUE)
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
        mean_mmse = mean(mmse, na.rm = TRUE),
        sd_mmse = sd(mmse, na.rm = TRUE)
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
      mean_mmse = mean(mmse, na.rm = TRUE),
      sd_mmse = sd(mmse, na.rm = TRUE)
    )
    
    
    summary_post <- summary_stats(
      df,
      c(
        "p_tau", "amyloid_b_ratio_42_40", "gfap", "nfl", "priority_memory_dr",
        "priority_memory_dr_z", "age_rec", "age_cent", "years_since_baseline"
      )
    )

    if (nrow(df) == 0) {
      return(list(
        "error_message" = "Empty dataset: no participants selected"
      ))
    }
    # RIRS model with unstructured covariance structure (add model for every biomarker x cognitive measure)
    #MMSE
    vtg::log$info("RIRS_mmse_p_tau")
    RIRS_mmse_p_tau <- nlme::lme(mmse ~ years_since_baseline + age_cent + sex + education_low + education_high + p_tau + p_tau * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_mmse_gfap")
    RIRS_mmse_gfap <- nlme::lme(mmse ~ years_since_baseline + age_cent + sex + education_low + education_high + gfap + gfap * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_mmse_nfl")
    RIRS_mmse_nfl <- nlme::lme(mmse ~ years_since_baseline + age_cent + sex + education_low + education_high + nfl + nfl * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_mmse_amyloid_b_ratio")
    RIRS_mmse_amyloid_b_ratio <- nlme::lme(mmse ~ years_since_baseline + age_cent + sex + education_low + education_high + amyloid_b_ratio + amyloid_b_ratio * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    
    #Immediate recall
    vtg::log$info("RIRS_memory_p_tau_im")
    RIRS_memory_p_tau_im <- nlme::lme(priority_memory_im_z ~ years_since_baseline + age_cent + sex + education_low + education_high + p_tau + p_tau * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_memory_gfap_im")
    RIRS_memory_gfap_im <- nlme::lme(priority_memory_im_z ~ years_since_baseline + age_cent + sex + education_low + education_high + gfap + gfap * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_memory_nfl_im")
    RIRS_memory_nfl_im <- nlme::lme(priority_memory_im_z ~ years_since_baseline + age_cent + sex + education_low + education_high + nfl + nfl * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_memory_amyloid_b_ratio_im")
    RIRS_memory_amyloid_b_ratio_im <- nlme::lme(priority_memory_im_z ~ years_since_baseline + age_cent + sex + education_low + education_high + amyloid_b_ratio + amyloid_b_ratio * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    
    #Delayed recall
    vtg::log$info("RIRS_memory_p_tau_dr")
    RIRS_memory_p_tau_dr <- nlme::lme(priority_memory_dr_z ~ years_since_baseline + age_cent + sex + education_low + education_high + p_tau + p_tau * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_memory_gfap_dr")
    RIRS_memory_gfap_dr <- nlme::lme(priority_memory_dr_z ~ years_since_baseline + age_cent + sex + education_low + education_high + gfap + gfap * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_memory_nfl_dr")
    RIRS_memory_nfl_dr <- nlme::lme(priority_memory_dr_z ~ years_since_baseline + age_cent + sex + education_low + education_high + nfl + nfl * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_memory_amyloid_b_ratio_dr")
    RIRS_memory_amyloid_b_ratio_dr <- nlme::lme(priority_memory_dr_z ~ years_since_baseline + age_cent + sex + education_low + education_high + amyloid_b_ratio + amyloid_b_ratio * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))

    #Language
    vtg::log$info("RIRS_language_p_tau")
    RIRS_language_p_tau <- nlme::lme(priority_language_z ~ years_since_baseline + age_cent + sex + education_low + education_high + p_tau + p_tau * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_language_gfap")
    RIRS_language_gfap <- nlme::lme(priority_language_z ~ years_since_baseline + age_cent + sex + education_low + education_high + gfap + gfap * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_language_nfl")
    RIRS_language_nfl <- nlme::lme(priority_language_z ~ years_since_baseline + age_cent + sex + education_low + education_high + nfl + nfl * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_language_amyloid_b_ratio")
    RIRS_language_amyloid_b_ratio <- nlme::lme(priority_language_z ~ years_since_baseline + age_cent + sex + education_low + education_high + amyloid_b_ratio + amyloid_b_ratio * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    #Attention
    vtg::log$info("RIRS_attention_stroop_average_p_tau")
    RIRS_attention_stroop_average_p_tau <- nlme::lme(priority_attention_stroop_average_z ~ years_since_baseline + age_cent + sex + education_low + education_high + p_tau + p_tau * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_attention_stroop_average_gfap")
    RIRS_attention_stroop_average_gfap <- nlme::lme(priority_attention_stroop_average_z ~ years_since_baseline + age_cent + sex + education_low + education_high + gfap + gfap * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_attention_stroop_average_nfl")
    RIRS_attention_stroop_average_nfl <- nlme::lme(priority_attention_stroop_average_z ~ years_since_baseline + age_cent + sex + education_low + education_high + nfl + nfl * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_attention_stroop_average_amyloid_b_ratio")
    RIRS_attention_stroop_average_amyloid_b_ratio <- nlme::lme(priority_attention_stroop_average_z ~ years_since_baseline + age_cent + sex + education_low + education_high + amyloid_b_ratio + amyloid_b_ratio * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    #Executive function
    vtg::log$info("RIRS_executive_stroop_3_p_tau")
    RIRS_executive_stroop_3_p_tau <- nlme::lme(priority_executive_stroop_3_z ~ years_since_baseline + age_cent + sex + education_low + education_high + p_tau + p_tau * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_executive_stroop_3_gfap")
    RIRS_executive_stroop_3_gfap <- nlme::lme(priority_executive_stroop_3_z ~ years_since_baseline + age_cent + sex + education_low + education_high + gfap + gfap * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_executive_stroop_3_nfl")
    RIRS_executive_stroop_3_nfl <- nlme::lme(priority_executive_stroop_3_z ~ years_since_baseline + age_cent + sex + education_low + education_high + nfl + nfl * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_executive_stroop_3_amyloid_b_ratio")
    RIRS_executive_stroop_3_amyloid_b_ratio <- nlme::lme(priority_executive_stroop_3_z ~ years_since_baseline + age_cent + sex + education_low + education_high + amyloid_b_ratio + amyloid_b_ratio * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    
    #Executive function (Interference)
    vtg::log$info("RIRS_executive_stroop_interf_p_tau")
    RIRS_executive_stroop_interf_p_tau <- nlme::lme(priority_executive_stroop_interf_z ~ years_since_baseline + age_cent + sex + education_low + education_high + p_tau + p_tau * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_executive_stroop_interf_gfap")
    RIRS_executive_stroop_interf_gfap <- nlme::lme(priority_executive_stroop_interf_z ~ years_since_baseline + age_cent + sex + education_low + education_high + gfap + gfap * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_executive_stroop_interf_nfl")
    RIRS_executive_stroop_interf_nfl <- nlme::lme(priority_executive_stroop_interf_z ~ years_since_baseline + age_cent + sex + education_low + education_high + nfl + nfl * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))
    vtg::log$info("RIRS_executive_stroop_interf_amyloid_b_ratio")
    RIRS_executive_stroop_interf_amyloid_b_ratio <- nlme::lme(priority_executive_stroop_interf_z ~ years_since_baseline + age_cent + sex + education_low + education_high + amyloid_b_ratio + amyloid_b_ratio * years_since_baseline,
                           data = df,
                           random = ~ years_since_baseline | id,
                           weights = nlme::varIdent(form= ~1 | years_since_baseline),
                           correlation = nlme::corSymm(form = ~1 | id),
                           method = "REML",
                           na.action = na.exclude,
                           control = nlme::lmeControl(opt='optim'))

    model_info <- c("modelStruct", "dims", "contrasts", "coefficients", "fitted", "residuals", "numIter")
    results <- list(
      "model_memory_p_tau_im" = RIRS_memory_p_tau_im[model_info],
      "model_memory_p_tau_im_summary" = summary(RIRS_memory_p_tau_im),
      "model_memory_gfap_im" = RIRS_memory_gfap_im[model_info],
      "model_memory_gfap_im_summary" = summary(RIRS_memory_gfap_im),
      "model_memory_nfl_im" = RIRS_memory_nfl_im[model_info],
      "model_memory_nfl_im_summary" = summary(RIRS_memory_nfl_im),
      "model_memory_amyloid_b_ratio_im" = RIRS_memory_amyloid_b_ratio_im[model_info],
      "model_memory_amyloid-b_ratio_im_summary" = summary(RIRS_memory_amyloid_b_ratio_im),
      "model_memory_p_tau_dr" = RIRS_memory_p_tau_dr[model_info],
      "model_memory_p_tau_dr_summary" = summary(RIRS_memory_p_tau_dr),
      "model_memory_gfap_dr" = RIRS_memory_gfap_dr[model_info],
      "model_memory_gfap_dr_summary" = summary(RIRS_memory_gfap_dr),
      "model_memory_nfl_dr" = RIRS_memory_nfl_dr[model_info],
      "model_memory_nfl_dr_summary" = summary(RIRS_memory_nfl_dr),
      "model_memory_amyloid_b_ratio_dr" = RIRS_memory_amyloid_b_ratio_dr[model_info],
      "model_memory_amyloid-b_ratio_dr_summary" = summary(RIRS_memory_amyloid_b_ratio_dr),
      "model_mmse_p_tau" = RIRS_mmse_p_tau[model_info],
      "model_mmse_p_tau_summary" = summary(RIRS_mmse_p_tau),
      "model_mmse_gfap" = RIRS_mmse_gfap[model_info],
      "model_mmse_gfap_summary" = summary(RIRS_mmse_gfap),
      "model_mmse_nfl" = RIRS_mmse_nfl_dr[model_info],
      "model_mmse_nfl_summary" = summary(RIRS_mmse_nfl),
      "model_mmse_amyloid_b_ratio" = RIRS_mmse_amyloid_b_ratio_dr[model_info],
      "model_mmse_amyloid-b_ratio_summary" = summary(RIRS_mmse_amyloid_b_ratio),
      "model_language_p_tau" = RIRS_language_p_tau[model_info],
      "model_language_p_tau_summary" = summary(RIRS_language_p_tau),
      "model_language_gfap" = RIRS_language_gfap[model_info],
      "model_language_gfap_summary" = summary(RIRS_language_gfap),
      "model_language_nfl" = RIRS_language_nfl_dr[model_info],
      "model_language_nfl_summary" = summary(RIRS_language_nfl),
      "model_language_amyloid_b_ratio" = RIRS_language_amyloid_b_ratio_dr[model_info],
      "model_language_amyloid-b_ratio_summary" = summary(RIRS_language_amyloid_b_ratio),
      "model_attention_stroop_average_p_tau" = RIRS_attention_stroop_average_p_tau[model_info],
      "model_attention_stroop_average_p_tau_summary" = summary(RIRS_attention_stroop_average_p_tau),
      "model_attention_stroop_average_gfap" = RIRS_attention_stroop_average_gfap[model_info],
      "model_attention_stroop_average_gfap_summary" = summary(RIRS_attention_stroop_average_gfap),
      "model_attention_stroop_average_nfl" = RIRS_attention_stroop_average_nfl_dr[model_info],
      "model_attention_stroop_average_nfl_summary" = summary(RIRS_attention_stroop_average_nfl),
      "model_attention_stroop_average_amyloid_b_ratio" = RIRS_attention_stroop_average_amyloid_b_ratio_dr[model_info],
      "model_attention_stroop_average_amyloid-b_ratio_summary" = summary(RIRS_attention_stroop_average_amyloid_b_ratio),
      "model_executive_stroop_3_p_tau" = RIRS_executive_stroop_3_p_tau[model_info],
      "model_executive_stroop_3_p_tau_summary" = summary(RIRS_executive_stroop_3_p_tau),
      "model_executive_stroop_3_gfap" = RIRS_executive_stroop_3_gfap[model_info],
      "model_executive_stroop_3_gfap_summary" = summary(RIRS_executive_stroop_3_gfap),
      "model_executive_stroop_3_nfl" = RIRS_executive_stroop_3_nfl_dr[model_info],
      "model_executive_stroop_3_nfl_summary" = summary(RIRS_executive_stroop_3_nfl),
      "model_executive_stroop_3_amyloid_b_ratio" = RIRS_executive_stroop_3_amyloid_b_ratio_dr[model_info],
      "model_executive_stroop_3_amyloid-b_ratio_summary" = summary(RIRS_executive_stroop_3_amyloid_b_ratio),
      "model_executive_stroop_interf_p_tau" = RIRS_executive_stroop_interf_p_tau[model_info],
      "model_executive_stroop_interf_p_tau_summary" = summary(RIRS_executive_stroop_interf_p_tau),
      "model_executive_stroop_interf_gfap" = RIRS_executive_stroop_interf_gfap[model_info],
      "model_executive_stroop_interf_gfap_summary" = summary(RIRS_executive_stroop_interf_gfap),
      "model_executive_stroop_interf_nfl" = RIRS_executive_stroop_interf_nfl_dr[model_info],
      "model_executive_stroop_interf_nfl_summary" = summary(RIRS_executive_stroop_interf_nfl),
      "model_executive_stroop_interf_amyloid_b_ratio" = RIRS_executive_stroop_interf_amyloid_b_ratio_dr[model_info],
      "model_executive_stroop_interf_amyloid-b_ratio_summary" = summary(RIRS_executive_stroop_interf_amyloid_b_ratio),
      "average_FU_time_table" = average_FU_time_table,
      "count_men_and_women_table" = count_men_and_women_table,
      # "count_id" = count_id,
      "descriptives_per_year_table" = descriptives_per_year_table,
      "descriptives_by_sex_table" = descriptives_by_sex_table,
      "descriptives_by_sex_and_FU_table" = descriptives_by_sex_and_FU_table,
      "descriptives_by_sex_NPA_table" = descriptives_by_sex_NPA_table,
      "descriptives_per_year_NPA_table" = descriptives_per_year_NPA_table,
      "descriptives_by_sex_and_FU_NPA_table" = descriptives_by_sex_and_FU_NPA_table,
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
