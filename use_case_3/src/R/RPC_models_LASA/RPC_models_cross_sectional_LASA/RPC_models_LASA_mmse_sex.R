#here all the sex analyses and the overall model will be added
RPC_models_mmse_sex <- function(df, config, model = "memory", exclude=c()) {
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
    pre_summary <- summary_CS_stats(df)
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

    df_plasma <- df[!is.na(df$p_tau),]
    df_baseline <- df[!is.na(df$education_category_3) | !is.na(df$education_category_verhage),]

    # df_cogn_test <- df[!is.na(df[[memory_dr_test_name]]) & df$id %in% df_plasma$id & df$id %in% df_baseline$id,]
    df_mmse <- df[!is.na(df[["mmse_total"]]) & df$id %in% df_plasma$id & df$id %in% df_baseline$id,]

    df_grouped <- merge(
      x = df_baseline[c("id", "age", "sex", "birth_year", "education_category_3", "education_years", "education_category_verhage")],
      y = df_plasma[c("id", "date_plasma", "p_tau", "gfap", "nfl", "amyloid_b_42", "amyloid_b_40", "amyloid_b_ratio_42_40")],
      by = "id"
    )
    df_grouped <- df_grouped[! duplicated(df_grouped$id),]

    df <- merge(
      x = df_mmse[c("id", "date", "date_mmse", "mmse_total")],
      y = df_grouped,
      by = "id",
      all.x = T,
    )
    excluded <- unique(df$id[is.na(df$birth_year) | is.na(df$sex)])

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

    #df$years_since_baseline <- as.integer(df$days_since_baseline/365.25, 0)
    df$years_since_baseline <- as.numeric(floor(df$days_since_baseline / 365.25))
    vtg::log$info("Filter the dataset")
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

    # Education levels
    df$education_category_3 <- ifelse(
      is.na(df$education_category_3),
      dplyr::recode(df$education_category_verhage, "1"=0, "2"=1, "3"=1, "4"=1, "5"=1, "6"=1, "7"=2),
      df$education_category_3
    )
    df$education <- factor(df$education_category_3, levels = c(0, 1, 2), labels = c("low", "medium", "high"))
    # dummy variables:
    df$education_low <- ifelse(df$education == 'low', 1, 0)
    df$education_high <- ifelse(df$education == 'high', 1, 0)

    # In the original dataset, this variable may not
    # be associated with the plasma data but only with the visit date
    # May be necessary to first check if amyloid_b_42 and amyloid_b_40 are
    # available. If not available, use amyloid_b_ratio_42_40 directly from
    # the database.
    # )
    df$amyloid_b_ratio_42_40 <- ifelse(
      is.na(df$amyloid_b_42) | is.na(df$amyloid_b_40) | df$amyloid_b_40 == 0,
      df$amyloid_b_ratio_42_40,
      df$amyloid_b_42 / df$amyloid_b_40
    )
    df$id <- as.factor(as.character(df$id))

    #log transformation for amyloid ratio
    df$log_amyloid_b_ratio_42_40 <- log(df$amyloid_b_ratio_42_40)

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
        sd_age = sd(age_rec, na.rm = TRUE)
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
        sd_years_since_baseline = sd(years_since_baseline, na.rm = TRUE)
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
        sd_age = sd(age_rec, na.rm = TRUE)
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
        mean_edu_years = mean(education_years, na.rm = TRUE),
        sd_edu_years = sd(education_years, na.rm = TRUE),
        mean_age = mean(age_rec, na.rm = TRUE),
        sd_age = sd(age_rec, na.rm = TRUE),
        mean_mmse = mean(mmse_total, na.rm = TRUE),
        sd_mmse = sd(mmse_total, na.rm = TRUE)
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
        mean_mmse = mean(mmse_total, na.rm = TRUE),
        sd_mmse = sd(mmse_total, na.rm = TRUE)
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
        mean_mmse = mean(mmse_total, na.rm = TRUE),
        sd_mmse = sd(mmse_total, na.rm = TRUE)
      )

    if (nrow(df) == 0) {
      return(list(
        "error_message" = "Empty dataset: no participants selected"
      ))
    }

    # CS model with unstructured covariance structure (add model for every biomarker x cognitive measure)
    #Overall models
    vtg::log$info("CS_mmse_p_tau")
    CS_mmse_p_tau <- lm(mmse_total ~ age_rec + sex + education_low + education_high + p_tau,
                                 data = df,
                                 na.action = na.exclude
    summary_CS_mmse_p_tau <- sjPlot::tab_model(CS_mmse_p_tau)

    vtg::log$info("CS_mmse_gfap")
    CS_mmse_gfap <- lm(mmse_total ~ age_rec + sex + education_low + education_high + gfap,
                                data = df,
                                na.action = na.exclude,
    summary_CS_mmse_gfap <- sjPlot::tab_model(CS_mmse_gfap)

    vtg::log$info("CS_mmse_nfl")
    CS_mmse_nfl <- lm(mmse_total ~ age_rec + sex + education_low + education_high + nfl,
                               data = df,
                               na.action = na.exclude,
    summary_CS_mmse_nfl <- sjPlot::tab_model(CS_mmse_nfl)

    vtg::log$info("CS_mmse_amyloid_b_ratio")
    CS_mmse_amyloid_b_ratio_log <- lm(mmse_total ~ age_rec + sex + education_low + education_high + log_amyloid_b_ratio_42_40,
                                               data = df,
                                               na.action = na.exclude
    summary_CS_mmse_amyloid_b_ratio_log <- sjPlot::tab_model(CS_mmse_amyloid_b_ratio_log)

    
    #2 way interaction models for sex
    vtg::log$info("CS_mmse_p_tau_2w")
    CS_mmse_p_tau_2w <- lm(mmse_total ~  age_rec + sex + education_low + education_high + p_tau + sex * p_tau,
                                    data = df,
                                    na.action = na.exclude
    summary_CS_mmse_p_tau_2w <- sjPlot::tab_model(CS_mmse_p_tau_2w)

    vtg::log$info("CS_mmse_gfap_2w")
    CS_mmse_gfap_2w <- lm(mmse_total ~  age_rec + sex + education_low + education_high + gfap + sex * gfap,
                                   data = df,                                   
                                   na.action = na.exclude
    summary_CS_mmse_gfap_2w <- sjPlot::tab_model(CS_mmse_gfap_2w)

    vtg::log$info("CS_mmse_nfl_2w")
    CS_mmse_nfl_2w <- lm(mmse_total ~  age_rec + sex + education_low + education_high + nfl + sex * nfl,
                                  data = df,
                                  na.action = na.exclude
    summary_CS_mmse_nfl_2w <- sjPlot::tab_model(CS_mmse_nfl_2w)

    vtg::log$info("CS_mmse_amyloid_b_ratio_2w")
    CS_mmse_amyloid_b_ratio_log_2w <- lm(mmse_total ~  age_rec + sex + education_low + education_high + log_amyloid_b_ratio_42_40 + sex * log_amyloid_b_ratio_42_40,
                                                  data = df,
                                                  na.action = na.exclude
    summary_CS_mmse_amyloid_b_ratio_log_2w <- sjPlot::tab_model(CS_mmse_amyloid_b_ratio_log_2w)

    #models stratified for apoe
    ##man = 0, female = 1
    vtg::log$info("CS_mmse_p_tau_male")
    CS_mmse_p_tau_male <- lm(mmse_total ~  age_rec + education_low + education_high + p_tau,
                                      data = subset(df, sex_num == 0),
                                      na.action = na.exclude
    summary_CS_mmse_p_tau_male <- sjPlot::tab_model(CS_mmse_p_tau_male)

    vtg::log$info("CS_mmse_p_tau_female")
    CS_mmse_p_tau_female <- lm(mmse_total ~  age_rec + education_low + education_high + p_tau,
                                        data = subset(df, sex_num == 1),
                                        na.action = na.exclude
    summary_CS_mmse_p_tau_female <- sjPlot::tab_model(CS_mmse_p_tau_female)

    vtg::log$info("CS_mmse_gfap_male")
    CS_mmse_gfap_male <- lm(mmse_total ~  age_rec + education_low + education_high + gfap,
                                     data = subset(df, sex_num == 0),
                                     na.action = na.exclude
    summary_CS_mmse_gfap_male <- sjPlot::tab_model(CS_mmse_gfap_male)

    vtg::log$info("CS_mmse_gfap_female")
    CS_mmse_gfap_female <- lm(mmse_total ~  age_rec + education_low + education_high+ gfap,
                                       data = subset(df, sex_num == 1),
                                       na.action = na.exclude
    summary_CS_mmse_gfap_female <- sjPlot::tab_model(CS_mmse_gfap_female)

    vtg::log$info("CS_mmse_nfl_male")
    CS_mmse_nfl_male <- lm(mmse_total ~  age_rec + education_low + education_high + nfl,
                                    data = subset(df, sex_num == 0),
                                    na.action = na.exclude
    summary_CS_mmse_nfl_male <- sjPlot::tab_model(CS_mmse_nfl_male)

    vtg::log$info("CS_mmse_nfl_female")
    CS_mmse_nfl_female <- lm(mmse_total ~  age_rec + education_low + education_high + nfl,
                                      data = subset(df, sex_num == 1),
                                      na.action = na.exclude
    summary_CS_mmse_nfl_female <- sjPlot::tab_model(CS_mmse_nfl_female)

    vtg::log$info("CS_mmse_amyloid_b_ratio_log_male")
    CS_mmse_amyloid_b_ratio_log_male <- lm(mmse_total ~  age_rec + education_low + education_high + log_amyloid_b_ratio_42_40,
                                                    data = subset(df, sex_num == 0),
                                                    na.action = na.exclude
    summary_CS_mmse_amyloid_b_ratio_log_male <- sjPlot::tab_model(CS_mmse_amyloid_b_ratio_log_male)

    vtg::log$info("CS_mmse_amyloid_b_ratio_log_female")
    CS_mmse_amyloid_b_ratio_log_female <- lm(mmse_total ~  age_rec + education_low + education_high + log_amyloid_b_ratio_42_40,
                                                      data = subset(df, sex_num == 1),
                                                      na.action = na.exclude
    summary_CS_mmse_amyloid_b_ratio_log_female <- sjPlot::tab_model(CS_mmse_amyloid_b_ratio_log_female)



    # model_summary can't extract from lme models
    results <- list(
      "summary_CS_mmse_p_tau" = summary_CS_mmse_p_tau,
      "summary_CS_mmse_gfap" = summary_CS_mmse_gfap,
      "summary_CS_mmse_nfl" = summary_CS_mmse_nfl,
      "summary_CS_mmse_amyloid_b_ratio_log" = summary_CS_mmse_amyloid_b_ratio_log,

      "summary_CS_mmse_p_tau_2w" = summary_CS_mmse_p_tau_2w,
      "summary_CS_mmse_gfap_2w" = summary_CS_mmse_gfap_2w,
      "summary_CS_mmse_nfl_2w" = summary_CS_mmse_nfl_2w,
      "summary_CS_mmse_amyloid_b_ratio_log_2w" = summary_CS_mmse_amyloid_b_ratio_log_2w,

      "summary_CS_mmse_p_tau_male" = summary_CS_mmse_p_tau_male,
      "summary_CS_mmse_gfap_male" = summary_CS_mmse_gfap_male,
      "summary_CS_mmse_nfl_male" = summary_CS_mmse_nfl_male,
      "summary_CS_mmse_amyloid_b_ratio_log_male" = summary_CS_mmse_amyloid_b_ratio_log_male,
      "summary_CS_mmse_p_tau_female" = summary_CS_mmse_p_tau_female,
      "summary_CS_mmse_gfap_female" = summary_CS_mmse_gfap_female,
      "summary_CS_mmse_nfl_female" = summary_CS_mmse_nfl_female,
      "summary_CS_mmse_amyloid_b_ratio_log_female" = summary_CS_mmse_amyloid_b_ratio_log_female,

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
