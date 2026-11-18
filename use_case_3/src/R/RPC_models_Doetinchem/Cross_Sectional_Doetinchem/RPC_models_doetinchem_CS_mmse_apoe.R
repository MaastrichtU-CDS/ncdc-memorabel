RPC_models_DC <- function(df, config, model = "memory", exclude=c()) {
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
    # A specific date is not available.
    df_plasma$date_plasma <- df_plasma$date
    df_baseline <- df[!is.na(df$age) & !is.na(df$sex),]
    df_baseline_education <- df[!is.na(df$education_category_3),]
    df_baseline_education <- df_baseline_education[! duplicated(df_baseline_education$id),]
    df_apoe <- df[!is.na(df$apoe_carrier),]
    df_grouped <- merge(
      x = df_baseline[c("id", "age", "sex")],
      y = df_baseline_education[c("id", "education_category_3", "education_years")],
      by = "id"
    )
    df_grouped <- df_grouped[! duplicated(df_grouped$id),]
    df_grouped <- merge(
      x = df_grouped[c("id", "age", "sex", "education_category_3", "education_years")],
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
    df_cogn_test <- df[df$id %in% df_grouped$id & (!is.na(df[["priority_memory_im_ravlt"]])
      | !is.na(df[["priority_memory_dr_ravlt"]])
      | !is.na(df[["priority_language_animal_fluency_60_correct"]])
      | !is.na(df[["attention_test_ldst_60_correct"]])
      | !is.na(df[["attention_test_stroop_1_time"]])
      | !is.na(df[["attention_test_stroop_2_time"]])
      | !is.na(df[["priority_executive_stroop_3_time"]])
      | !is.na(df[["mmse_total"]])),]
    df <- merge(
      x = df_cogn_test[c("id", "date", "priority_memory_im_ravlt", "priority_memory_dr_ravlt",
        "priority_language_animal_fluency_60_correct", "attention_test_ldst_60_correct",
        "mmse_total", "attention_test_stroop_1_time", "attention_test_stroop_2_time",
        "priority_executive_stroop_3_time")],
      y = df_grouped,
      by = "id"
      # all.x = T
    )
    excluded <- unique(df$id[is.na(df$age) | is.na(df$sex)])

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

    # Age of participant:
    # current_year <- format(Sys.Date(), "%Y")
    # Year of birth will always be available (mandatory in OMOP), age is not guarantee
    # DCS: use age, error with the birth year
    # df$age_rec <- as.numeric(format(df$date, "%Y")) - df$birth_year
    df$age_rec <- as.numeric(df$age)

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
    # math operations are not meaningful with factors
    # df$sex_num <- df$sex_num, levels = c(0, 1), labels = c("female", "male")
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

    # df$education_category_3 <- dplyr::recode(df$education_category_verhage, "1"=0, "2"=0, "3"=0, "4"=1, "5"=1, "6"=2, "7"=2)
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
    df$log_amyloid_b_ratio_42_40 <- log(df$amyloid_b_ratio_42_40)


    df$id <- as.factor(as.character(df$id))
    # df %>% dplyr::mutate_if(is.character, as.factor)

    

    # CS model with unstructured covariance structure (add model for every biomarker x cognitive measure)
    #2-way interactions for apoe
    vtg::log$info("CS_mmse_p_tau_2w")
    summary_CS_mmse_p_tau_2w <- safe_lme_summary(mmse_total ~ age_rec + sex + education_low + education_high + apoe_carrier + p_tau + apoe_carrier * p_tau,
                                    data = df,
                                    na.action = na.exclude)
    # summary_CS_mmse_p_tau_2w <- sjPlot::tab_model(CS_mmse_p_tau_2w, digits = 10)

    vtg::log$info("CS_mmse_gfap_2w")
    summary_CS_mmse_gfap_2w <- safe_lme_summary(mmse_total ~ age_rec + sex + education_low + education_high + apoe_carrier + gfap + apoe_carrier * gfap,
                                   data = df,
                                   na.action = na.exclude)
    #summary_CS_mmse_gfap_2w <- sjPlot::tab_model(CS_mmse_gfap_2w, digits = 10)

    vtg::log$info("CS_mmse_nfl_2w")
    summary_CS_mmse_nfl_2w <- safe_lme_summary(mmse_total ~ age_rec + sex + education_low + education_high + apoe_carrier + nfl + apoe_carrier * nfl,
                                  data = df,
                                  na.action = na.exclude)
    #summary_CS_mmse_nfl_2w <- sjPlot::tab_model(CS_mmse_nfl_2w, digits = 10)

    vtg::log$info("CS_mmse_p_tau_2w")
    summary_CS_mmse_p_tau_2w <- safe_lme_summary(mmse_total ~ age_rec + sex + education_low + education_high + apoe_carrier + ptau + apoe_carrier * nfl,
                                               data = df,
                                               na.action = na.exclude)

    vtg::log$info("CS_mmse_amyloid_b_ratio_2w")
    summary_CS_mmse_amyloid_b_ratio_log_2w <- safe_lme_summary(mmse_total ~ age_rec + sex + education_low + education_high + apoe_carrier + log_amyloid_b_ratio_42_40
                                                  + apoe_carrier * log_amyloid_b_ratio_42_40,
                                                  data = df,
                                                  na.action = na.exclude)
    #summary_CS_mmse_amyloid_b_ratio_log_2w <- sjPlot::tab_model(CS_mmse_amyloid_b_ratio_log_2w, digits = 10)

    # Apoe stratified models
    ##APOE4_carrier no = F, APOE4_carrier yes = T
    summary_CS_mmse_p_tau_apoe_neg <- safe_lme_summary(mmse_total ~ age_rec + sex + education_low + education_high + p_tau,
                                          data = subset(df, apoe_carrier == "no"),
                                          na.action = na.exclude)
    #summary_CS_mmse_p_tau_apoe_neg <- sjPlot::tab_model(CS_mmse_p_tau_apoe_neg, digits = 10)

    summary_CS_mmse_p_tau_apoe_pos <- safe_lme_summary(mmse_total ~ age_rec + sex + education_low + education_high + p_tau,
                                          data = subset(df, apoe_carrier == "yes"),
                                          na.action = na.exclude)
    #summary_CS_mmse_p_tau_apoe_pos <- sjPlot::tab_model(CS_mmse_p_tau_apoe_pos, digits = 10)

    vtg::log$info("CS_mmse_gfap")
    summary_CS_mmse_gfap_apoe_neg <- safe_lme_summary(mmse_total ~ age_rec + sex + education_low + education_high + gfap,
                                         data = subset(df, apoe_carrier == "no"),
                                         na.action = na.exclude)
    #summary_CS_mmse_gfap_apoe_neg <- sjPlot::tab_model(CS_mmse_gfap_apoe_neg, digits = 10)

    summary_CS_mmse_gfap_apoe_pos <- safe_lme_summary(mmse_total ~ age_rec + sex + education_low + education_high + gfap,
                                         data = subset(df, apoe_carrier == "yes"),
                                         na.action = na.exclude)
    #summary_CS_mmse_gfap_apoe_pos <- sjPlot::tab_model(CS_mmse_gfap_apoe_pos, digits = 10)

    vtg::log$info("CS_mmse_nfl")
    summary_CS_mmse_nfl_apoe_neg <- safe_lme_summary(mmse_total ~ age_rec + sex + education_low + education_high + nfl,
                                        data = subset(df, apoe_carrier == "no"),
                                        na.action = na.exclude)
    #summary_CS_mmse_nfl_apoe_neg <- sjPlot::tab_model(CS_mmse_nfl_apoe_neg, digits = 10)

    summary_CS_mmse_nfl_apoe_pos <- safe_lme_summary(mmse_total ~ age_rec + sex + education_low + education_high + nfl,
                                        data = subset(df, apoe_carrier == "yes"),
                                        na.action = na.exclude)
    #summary_CS_mmse_nfl_apoe_pos <- sjPlot::tab_model(CS_mmse_nfl_apoe_pos, digits = 10)

    vtg::log$info("CS_mmse_amyloid_b_ratio")
    summary_CS_mmse_amyloid_b_ratio_apoe_neg <- safe_lme_summary(mmse_total ~ age_rec + sex + education_low + education_high + log_amyloid_b_ratio_42_40,
                                                    data = subset(df, apoe_carrier == "no"),
                                                    na.action = na.exclude)
    #summary_CS_mmse_amyloid_b_ratio_apoe_neg <- sjPlot::tab_model(CS_mmse_amyloid_b_ratio_apoe_neg, digits = 10)

    vtg::log$info("CS_mmse_amyloid_b_ratio")
    summary_CS_mmse_amyloid_b_ratio_apoe_pos <- safe_lme_summary(mmse_total ~ age_rec + sex + education_low + education_high + log_amyloid_b_ratio_42_40,
                                                    data = subset(df, apoe_carrier == "yes"),
                                                    na.action = na.exclude)
    #summary_CS_mmse_amyloid_b_ratio_apoe_pos <- sjPlot::tab_model(CS_mmse_amyloid_b_ratio_apoe_pos, digits = 10)

    # model_summary can't extract from safe_lme_summarye models
    results <- list(
      "summary_CS_mmse_p_tau_2w" = summary_CS_mmse_p_tau_2w,
      "summary_CS_mmse_gfap_2w" = summary_CS_mmse_gfap_2w,
      "summary_CS_mmse_nfl_2w" = summary_CS_mmse_nfl_2w,
      "summary_CS_mmse_amyloid_b_ratio_log_2w" = summary_CS_mmse_amyloid_b_ratio_log_2w,

      "summary_CS_mmse_p_tau_apoe_neg" = summary_CS_mmse_p_tau_apoe_neg,
      "summary_CS_mmse_gfap_apoe_neg" = summary_CS_mmse_gfap_apoe_neg,
      "summary_CS_mmse_nfl_apoe_neg" = summary_CS_mmse_nfl_apoe_neg,
      "summary_CS_mmse_amyloid_b_ratio_apoe_neg" = summary_CS_mmse_amyloid_b_ratio_apoe_neg,

      "summary_CS_mmse_p_tau_apoe_pos" = summary_CS_mmse_p_tau_apoe_pos,
      "summary_CS_mmse_gfap_apoe_pos" = summary_CS_mmse_gfap_apoe_pos,
      "summary_CS_mmse_nfl_apoe_pos" = summary_CS_mmse_nfl_apoe_pos,
      "summary_CS_mmse_amyloid_b_ratio_apoe_pos" = summary_CS_mmse_amyloid_b_ratio_apoe_pos,

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
