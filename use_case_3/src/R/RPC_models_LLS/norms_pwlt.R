RPC_models_lls <- function(df, config, model = "memory", exclude=c()) {
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

#Create the dataset needed
    # cognitive variables
cog_vars <- c(
  "priority_memory_im_pwlt",
  "priority_memory_de_pwlt"
)

df_grouped <- df_baseline %>%
  dplyr::select(id, age, sex, birth_year) %>%
  dplyr::left_join(
    df_baseline_education %>% dplyr::select(id, education_category_3),
    by = "id"
  ) %>%
  dplyr::distinct(id, .keep_all = TRUE)

# Cognitive test filter
df_cogn_test <- df[rowSums(!is.na(df[cog_vars])) > 0, ]

df_final <- df_cogn_test %>%
  dplyr::select(
    id, date,
    priority_memory_im_pwlt, priority_memory_de_pwlt,
    priority_executive_stroop_3_time, mmse_total,
    attention_test_stroop_1_time, attention_test_stroop_2_time,
    attention_test_sdst_60_correct, attention_test_sdst_90_correct
  ) %>%
  dplyr::left_join(df_grouped, by = "id")

excluded <- df_final %>%
  dplyr::filter(is.na(birth_year) | is.na(sex)) %>%
  dplyr::pull(id) %>%
  unique()

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
    df$sex_num <- ifelse(df$sex == 0, 1, 0)
    df$sex <- factor(df$sex, levels = c(0, 1), labels = c("male", "female"))

    # Education levels
    df$education <- factor(df$education_category_3, levels = c(0, 1, 2), labels = c("low", "medium", "high"))

    # dummy variables:
    df$education_low <- ifelse(df$education == 'low', 1, 0)
    df$education_high <- ifelse(df$education == 'high', 1, 0)

    df$id <- as.factor(as.character(df$id))

# Descriptive statistics --------------------------------------------------
    #Count of participants
    dplyr::n_distinct(df$id)

    #Count of women and men (0 = women, 1 = men)
    count_men_and_women_table <- df %>%
      dplyr::group_by(sex) %>%
      dplyr::summarise(
        count_sex = dplyr::n_distinct(id)
      )

##Descriptives delayed recall logical memory task, immediate recall logical memory task.
descriptives_by_sex_table <- df %>%
  group_by(sex) %>%
  summarise(
    nr_participants = n_distinct(id),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    mean_dr_lm = mean(priority_memory_dr_lm, na.rm = TRUE),
    sd_dr_lm = sd(priority_memory_dr_lm, na.rm = TRUE),
    mean_im_lm = mean(priority_memory_im_lm, na.rm = TRUE),
    sd_im_lm = sd(priority_memory_im_lm, na.rm = TRUE)
  )

descriptives_by_edu_table <- df %>%
  group_by(education_category_3) %>%
  summarise(
    nr_participants = n_distinct(id),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    mean_dr_lm = mean(priority_memory_dr_lm, na.rm = TRUE),
    sd_dr_lm = sd(priority_memory_dr_lm, na.rm = TRUE),
    mean_im_lm = mean(priority_memory_im_lm, na.rm = TRUE),
    sd_im_lm = sd(priority_memory_im_lm, na.rm = TRUE)
  )

descriptives_by_sex_and_edu_table <- df %>%
  group_by(sex, education_category_3) %>%
  summarise(nr_participants = n_distinct(id),
            mean_age = mean(age, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE),
            mean_dr_lm = mean(priority_memory_dr_lm, na.rm = TRUE),
            sd_dr_lm = sd(priority_memory_dr_lm, na.rm = TRUE),
            mean_im_lm = mean(priority_memory_im_lm, na.rm = TRUE),
            sd_im_lm = sd(priority_memory_im_lm, na.rm = TRUE)
  )

descriptives_im_lm <- describe(df$priority_memory_im_lm)
descriptives_im_lm_stats <- df(
  mean = descriptives_im_lm$mean,
  sd = descriptives_im_lm$sd,
  min = descriptives_im_lm$min,
  max = descriptives_im_lm$max
)

descriptives_dr_lm <- describe(df$priority_memory_dr_lm)
descriptives_dr_lm_stats <- df(
  mean = descriptives_dr_lm$mean,
  sd = descriptives_dr_lm$sd,
  min = descriptives_dr_lm$min,
  max = descriptives_dr_lm$max
)

# Regression models -------------------------------------------------------
##Syntax regression model including age, sex, and education for immediate recall of logical memory
model_im_lm <- lm(df$priority_memory_im_lm ~ age_cent + sex + education_low + education_high, data = df)
    summary_model_im_lm <- sjPlot::tab_model(model_im_lm)


##Syntax regression model including age, sex, and education for delayed recall of logical memory
model_dr_lm <- lm(df$priority_memory_dr_lm ~ age_cent + sex + education_low + education_high, data = df)
    summary_model_dr_lm <- sjPlot::tab_model(model_dr_lm)


#immediate recall logical memory
df$ZPRED_im_lm <- predict(model_im_lm) #predicted z-score
df$ZRESID_im_lm <- resid(model_im_lm) #residuals of the model

##Homoscedasticity
###Scatterplot of residuals - immediate recall logical memory
scatter_res_im_lm <- plot(df$ZPRED_im_lm, df$ZRESID_im_lm,
                          xlab = "Predicted Mean Test Score",
                          ylab = "Residuals",
                          main = "Scatterplot of immediate recall logical memory",
                          pch = 19,  # point type
                          col = ifelse(df$sex == 1, "blue", "red"))
#Add a horizontal line at y = 0 to show where residuals = 0
abline(h = 0, col = "black", lty = 2)

##Noramlity of the errors - immediate recall logical memory
histo_im_lm <- hist(df$ZRESID_im_lm, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm_im_lm <- qqnorm(df$ZRESID_im_lm)
qq_line_im_lm <- qqline(df$ZRESID_im_lm, col = "red")

z_pred_resid_descriptives_im_lm <- describe(df %>% select(ZPRED_im_lm, ZRESID_im_lm))
print(z_pred_resid_descriptives_im_lm)

#delayed recall logical memory
df$ZPRED_dr_lm <- predict(model_dr_lm) #predicted z-score
df$ZRESID_dr_lm <- resid(model_dr_lm) #residuals of the model

##Homoscedasticity
###Scatterplot of residuals - delayed recall logical memory
scatter_res_dr_lm <- plot(df$ZPRED_dr_lm, df$ZRESID_dr_lm,
                          xlab = "Predicted Mean Test Score",
                          ylab = "Residuals",
                          main = "Scatterplot of delayed recall logical memory",
                          pch = 19,  # point type
                          col = ifelse(df$sex == 1, "blue", "red"))
#Add a horizontal line at y = 0 to show where residuals = 0
abline(h = 0, col = "black", lty = 2)

##Noramlity of the errors - delayed recall logical memory
histo_dr_lm <- hist(df$ZRESID_dr_lm, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm_dr_lm <- qqnorm(df$ZRESID_dr_lm)
qq_line_dr_lm <- qqline(df$ZRESID_dr_lm, col = "red")

z_pred_resid_descriptives_dr_lm <- describe(df %>% select(ZPRED_dr_lm, ZRESID_dr_lm))
print(z_pred_resid_descriptives_dr_lm)

# The output --------------------------------------------------------------
results <- list(
  "descriptives_by_sex" = descriptives_by_sex_table,
  "descriptives_by_edu" = descriptives_by_edu_table,
  "descriptives_by_sex_and_edu" = descriptives_by_sex_and_edu_table,
  "descriptives_by_diagnosis" = descriptives_by_diagnosis_table,
  "summary_model_im_lm" = summary_model_im_lm,
  "summary_model_dr_lm" = summary_model_dr_lm,
  "scatter_res_im_lm" = scatter_res_im_lm,
  "histo_im_lm" = histo_im_lm,
  "qqnorm_im_lm" = qqnorm_im_lm,
  "qq_line_im_lm" = qq_line_im_lm,
  "z_pred_resid_descriptives_im_lm" = z_pred_resid_descriptives_im_lm,
  "scatter_res_dr_lm" = scatter_res_dr_lm,
  "histo_dr_lm" = histo_dr_lm,
  "qqnorm_dr_lm" = qqnorm_dr_lm,
  "qq_line_dr_lm" = qq_line_dr_lm,
  "z_pred_resid_descriptives_dr_lm" = z_pred_resid_descriptives_dr_lm,

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
