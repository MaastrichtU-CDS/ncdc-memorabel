RPC_models_EMIF_AD_mmse_sex <- function(df, config, model = "memory", exclude=c()) {
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
      by = "id",
      all.x = T
    )
    df_cogn_test <- df[df$id %in% df_grouped$id & (!is.na(df[["priority_memory_dr_15_word_list_correct"]]) | !is.na(df[["priority_executive_stroop_3_time"]]) | !is.na(df[["mmse_total"]])
      | !is.na(df[["attention_test_sdst_60_correct"]]) | !is.na(df[["attention_test_tmt_a_time"]]) | !is.na(df[["priority_executive_tmt_b_time"]])
      | !is.na(df[["priority_memory_im_15_word_list_correct"]]) | !is.na(df[["priority_language_animal_fluency_60_correct"]])
      | !is.na(df[["attention_test_stroop_1_time"]]) | !is.na(df[["attention_test_stroop_2_time"]])),]
    df <- merge(
      x = df_cogn_test[c("id", "date", "priority_memory_dr_15_word_list_correct", "mmse_total",
        "attention_test_sdst_60_correct", "attention_test_tmt_a_time", "priority_executive_tmt_b_time",
        "priority_memory_im_15_word_list_correct", "priority_language_animal_fluency_60_correct",
        "priority_executive_stroop_3_time", "attention_test_stroop_1_time", "attention_test_stroop_2_time")],
      y = df_grouped,
      by = "id"
      # all.x = T
    )
    # attention_test_sdst_60_correct should be attention_test_sdst_60_ts but there was an error in the DB
    df$attention_test_sdst_60 <- df$attention_test_sdst_60_correct
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

    #df$years_since_baseline <- as.integer(df$days_since_baseline/365.25, 0)
    df$years_since_baseline <- as.numeric(floor(df$days_since_baseline/365.25))

    df <- subset(df, years_since_baseline >= 0)

    #Create variable for number of follow-ups
    df <- df %>%
      dplyr::arrange(id, years_since_baseline) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(num_prior_visit = dplyr::row_number()-1) %>%
      dplyr::ungroup()

    df$sqrt_prior_visit <- sqrt(df$num_prior_visit)

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
    df$log_amyloid_b_ratio_42_40 <- log(df$amyloid_b_ratio_42_40)

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
      )

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
        sd_age = sd(age_rec, na.rm = TRUE)
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
        sd_years_since_baseline = sd(years_since_baseline, na.rm = TRUE)
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
        sd_age = sd(age_rec, na.rm = TRUE)
      )

    if (nrow(df) == 0) {
      return(list(
        "error_message" = "Empty dataset: no participants selected"
      ))
    }


    # RIRS model with unstructured covariance structure (add model for every biomarker x cognitive measure)
    #Overall models
    vtg::log$info("summary_mmse_p_tau")
    summary_mmse_p_tau <- safe_lme_summary(mmse_total ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + p_tau + p_tau * years_since_baseline,
                                 data = df,
                                 random = ~ years_since_baseline | id,
                                 weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                 correlation = nlme::corSymm(form = ~1 | id),
                                 method = "REML",
                                 na.action = na.exclude,
                                 control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    # summary_mmse_p_tau <- sjPlot::tab_model(summary_mmse_p_tau, digits = 10)

    vtg::log$info("summary_mmse_gfap")
    summary_mmse_gfap <- safe_lme_summary(mmse_total ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + gfap + gfap * years_since_baseline,
                                data = df,
                                random = ~ years_since_baseline | id,
                                weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                correlation = nlme::corSymm(form = ~1 | id),
                                method = "REML",
                                na.action = na.exclude,
                                control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    # summary_mmse_gfap <- sjPlot::tab_model(summary_mmse_gfap, digits = 10)

    # vtg::log$info("summary_mmse_nfl")
    # summary_mmse_nfl <- safe_lme_summary(mmse_total ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + nfl + nfl * years_since_baseline,
    #                            data = df,
    #                            random = ~ years_since_baseline | id,
    #                            weights = nlme::varIdent(form= ~1 | years_since_baseline),
    #                            correlation = nlme::corSymm(form = ~1 | id),
    #                            method = "REML",
    #                            na.action = na.exclude,
    #                            # control = nlme::lmeControl(opt='optim'))
    #                            control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    # # summary_mmse_nfl <- sjPlot::tab_model(summary_mmse_nfl, digits = 10)

    vtg::log$info("summary_mmse_amyloid_b_ratio")
    summary_mmse_amyloid_b_ratio_log <- safe_lme_summary(mmse_total ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + log_amyloid_b_ratio_42_40 + log_amyloid_b_ratio_42_40 * years_since_baseline,
                                               data = df,
                                               random = ~ years_since_baseline | id,
                                               weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                               correlation = nlme::corSymm(form = ~1 | id),
                                               method = "REML",
                                               na.action = na.exclude,
                                               # model doesn't converge with the default options
                                               control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    # summary_mmse_amyloid_b_ratio_log <- sjPlot::tab_model(summary_mmse_amyloid_b_ratio_log, digits = 10)

    #3 way interaction models for sex
    vtg::log$info("summary_mmse_p_tau_3w")
    summary_mmse_p_tau_3w <- safe_lme_summary(mmse_total ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + p_tau + p_tau * years_since_baseline
                                    + sex * p_tau + sex * years_since_baseline + sex * p_tau * years_since_baseline,
                                    data = df,
                                    random = ~ years_since_baseline | id,
                                    weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                    correlation = nlme::corSymm(form = ~1 | id),
                                    method = "REML",
                                    na.action = na.exclude,
                                    control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    # summary_mmse_p_tau_3w <- sjPlot::tab_model(summary_mmse_p_tau_3w, digits = 10)

    vtg::log$info("summary_mmse_gfap_3w")
    summary_mmse_gfap_3w <- safe_lme_summary(mmse_total ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + gfap + gfap * years_since_baseline
                                   + sex * gfap + sex * years_since_baseline + sex * gfap * years_since_baseline,
                                   data = df,
                                   random = ~ years_since_baseline | id,
                                   weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                   correlation = nlme::corSymm(form = ~1 | id),
                                   method = "REML",
                                   na.action = na.exclude,
                                   control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    # summary_mmse_gfap_3w <- sjPlot::tab_model(summary_mmse_gfap_3w, digits = 10)

    # vtg::log$info("summary_mmse_nfl_3w")
    # summary_mmse_nfl_3w <- safe_lme_summary(mmse_total ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + nfl + nfl * years_since_baseline
    #                               + sex * nfl + sex * years_since_baseline + sex * nfl * years_since_baseline,
    #                               data = df,
    #                               random = ~ years_since_baseline | id,
    #                               weights = nlme::varIdent(form= ~1 | years_since_baseline),
    #                               correlation = nlme::corSymm(form = ~1 | id),
    #                               method = "REML",
    #                               na.action = na.exclude,
    #                               control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    # # summary_mmse_nfl_3w <- sjPlot::tab_model(summary_mmse_nfl_3w, digits = 10)

    vtg::log$info("summary_mmse_amyloid_b_ratio_3w")
    summary_mmse_amyloid_b_ratio_log_3w <- safe_lme_summary(mmse_total ~ years_since_baseline + age_rec + sex + sqrt_prior_visit + education_low + education_high + log_amyloid_b_ratio_42_40 + log_amyloid_b_ratio_42_40 * years_since_baseline
                                                  + sex * log_amyloid_b_ratio_42_40 + sex * years_since_baseline + sex * log_amyloid_b_ratio_42_40 * years_since_baseline,
                                                  data = df,
                                                  random = ~ years_since_baseline | id,
                                                  weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                                  correlation = nlme::corSymm(form = ~1 | id),
                                                  method = "REML",
                                                  na.action = na.exclude,
                                                  control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    # summary_mmse_amyloid_b_ratio_log_3w <- sjPlot::tab_model(summary_mmse_amyloid_b_ratio_log_3w, digits = 10)

    #models stratified for apoe
    ##man = 0, female = 1
    vtg::log$info("summary_mmse_p_tau_male")
    summary_mmse_p_tau_male <- safe_lme_summary(mmse_total ~ years_since_baseline + sqrt_prior_visit + age_rec + education_low + education_high + p_tau + p_tau * years_since_baseline,
                                      data = subset(df, sex_num == 0),
                                      random = ~ years_since_baseline | id,
                                      weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                      correlation = nlme::corSymm(form = ~1 | id),
                                      method = "REML",
                                      na.action = na.exclude,
                                      control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    # summary_mmse_p_tau_male <- sjPlot::tab_model(summary_mmse_p_tau_male, digits = 10)

    vtg::log$info("summary_mmse_p_tau_female")
    summary_mmse_p_tau_female <- safe_lme_summary(mmse_total ~ years_since_baseline + sqrt_prior_visit + age_rec + education_low + education_high + p_tau + p_tau * years_since_baseline,
                                        data = subset(df, sex_num == 1),
                                        random = ~ years_since_baseline | id,
                                        weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                        correlation = nlme::corSymm(form = ~1 | id),
                                        method = "REML",
                                        na.action = na.exclude,
                                        control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    # summary_mmse_p_tau_female <- sjPlot::tab_model(summary_mmse_p_tau_female, digits = 10)

    vtg::log$info("summary_mmse_gfap_male")
    summary_mmse_gfap_male <- safe_lme_summary(mmse_total ~ years_since_baseline + sqrt_prior_visit + age_rec + education_low + education_high + gfap + gfap * years_since_baseline,
                                     data = subset(df, sex_num == 0),
                                     random = ~ years_since_baseline | id,
                                     weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                     correlation = nlme::corSymm(form = ~1 | id),
                                     method = "REML",
                                     na.action = na.exclude,
                                     control = nlme::lmeControl(opt='optim'))
    # summary_mmse_gfap_male <- sjPlot::tab_model(summary_mmse_gfap_male, digits = 10)

    # vtg::log$info("summary_mmse_gfap_female")
    summary_mmse_gfap_female <- safe_lme_summary(mmse_total ~ years_since_baseline + sqrt_prior_visit + age_rec + education_low + education_high + gfap + gfap * years_since_baseline,
                                       data = subset(df, sex_num == 1),
                                       random = ~ years_since_baseline | id,
                                       weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                       correlation = nlme::corSymm(form = ~1 | id),
                                       method = "REML",
                                       na.action = na.exclude,
                                       control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    # summary_mmse_gfap_female <- sjPlot::tab_model(summary_mmse_gfap_female, digits = 10)

    # vtg::log$info("summary_mmse_nfl_male")
    # summary_mmse_nfl_male <- safe_lme_summary(mmse_total ~ years_since_baseline + sqrt_prior_visit + age_rec + education_low + education_high + nfl + nfl * years_since_baseline,
    #                                 data = subset(df, sex_num == 0),
    #                                 random = ~ years_since_baseline | id,
    #                                 weights = nlme::varIdent(form= ~1 | years_since_baseline),
    #                                 correlation = nlme::corSymm(form = ~1 | id),
    #                                 method = "REML",
    #                                 na.action = na.exclude,
    #                                 control = nlme::lmeControl(opt='optim'))
    # # summary_mmse_nfl_male <- sjPlot::tab_model(summary_mmse_nfl_male, digits = 10)

    # vtg::log$info("summary_mmse_nfl_female")
    # summary_mmse_nfl_female <- safe_lme_summary(mmse_total ~ years_since_baseline + sqrt_prior_visit + age_rec + education_low + education_high + nfl + nfl * years_since_baseline,
    #                                   data = subset(df, sex_num == 1),
    #                                   random = ~ years_since_baseline | id,
    #                                   weights = nlme::varIdent(form= ~1 | years_since_baseline),
    #                                   correlation = nlme::corSymm(form = ~1 | id),
    #                                   method = "REML",
    #                                   na.action = na.exclude,
    #                                   control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    # # summary_mmse_nfl_female <- sjPlot::tab_model(summary_mmse_nfl_female, digits = 10)

    vtg::log$info("summary_mmse_amyloid_b_ratio_log_male")
    summary_mmse_amyloid_b_ratio_log_male <- safe_lme_summary(mmse_total ~ years_since_baseline + sqrt_prior_visit + age_rec + education_low + education_high + log_amyloid_b_ratio_42_40 + log_amyloid_b_ratio_42_40 * years_since_baseline,
                                                    data = subset(df, sex_num == 0),
                                                    random = ~ years_since_baseline | id,
                                                    weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                                    correlation = nlme::corSymm(form = ~1 | id),
                                                    method = "REML",
                                                    na.action = na.exclude,
                                                    control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    # summary_mmse_amyloid_b_ratio_log_male <- sjPlot::tab_model(summary_mmse_amyloid_b_ratio_log_male, digits = 10)

    vtg::log$info("summary_mmse_amyloid_b_ratio_log_female")
    summary_mmse_amyloid_b_ratio_log_female <- safe_lme_summary(mmse_total ~ years_since_baseline + sqrt_prior_visit + age_rec + education_low + education_high + log_amyloid_b_ratio_42_40 + log_amyloid_b_ratio_42_40 * years_since_baseline,
                                                      data = subset(df, sex_num == 1),
                                                      random = ~ years_since_baseline | id,
                                                      weights = nlme::varIdent(form= ~1 | years_since_baseline),
                                                      correlation = nlme::corSymm(form = ~1 | id),
                                                      method = "REML",
                                                      na.action = na.exclude,
                                                      control = nlme::lmeControl(opt='optim', maxIter = 500, msMaxIter = 500, msMaxEval = 500, msVerbose = TRUE))
    # summary_mmse_amyloid_b_ratio_log_female <- sjPlot::tab_model(summary_mmse_amyloid_b_ratio_log_female, digits = 10)



    # model_summary can't extract from lme models
    results <- list(
      "summary_mmse_p_tau" = summary_mmse_p_tau,
      "summary_mmse_gfap" = summary_mmse_gfap,
      # "summary_mmse_nfl" = summary_mmse_nfl,
      "summary_mmse_amyloid_b_ratio_log" = summary_mmse_amyloid_b_ratio_log,

      "summary_mmse_p_tau_3w" = summary_mmse_p_tau_3w,
      "summary_mmse_gfap_3w" = summary_mmse_gfap_3w,
      # "summary_mmse_nfl_3w" = summary_mmse_nfl_3w,
      "summary_mmse_amyloid_b_ratio_log_3w" = summary_mmse_amyloid_b_ratio_log_3w,

      "summary_mmse_p_tau_male" = summary_mmse_p_tau_male,
      "summary_mmse_gfap_male" = summary_mmse_gfap_male,
      # "summary_mmse_nfl_male" = summary_mmse_nfl_male,
      "summary_mmse_amyloid_b_ratio_log_male" = summary_mmse_amyloid_b_ratio_log_male,
      "summary_mmse_p_tau_female" = summary_mmse_p_tau_female,
      "summary_mmse_gfap_female" = summary_mmse_gfap_female,
      # "summary_mmse_nfl_female" = summary_mmse_nfl_female,
      "summary_mmse_amyloid_b_ratio_log_female" = summary_mmse_amyloid_b_ratio_log_female,

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
