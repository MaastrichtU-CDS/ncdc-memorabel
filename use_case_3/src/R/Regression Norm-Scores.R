# Libraries ---------------------------------------------------------------
library(haven)
library(tidyverse)
#library(plyr)
library(ggplot2)
# library(gtsummary)
# library(flextable)
library(modelsummary)
library(broom)
# Postgres DB
library(DBI)
library(RPostgres)
#new libraries
library(dplyr)
library(psych)

RPC_mixed_models <- function(df) {
  vtg::log$info("Starting: Linear models")
  result = tryCatch({
    con <- RPostgres::dbConnect(
      RPostgres::Postgres(),
      dbname = Sys.getenv("PGDATABASE"),
      host = Sys.getenv("PGHOST"),
      port = Sys.getenv("PGPORT"),
      password = Sys.getenv("PGPASSWORD"),
      user = Sys.getenv("PGUSER"),
    )
    df <- RPostgres::dbGetQuery(con, 'SELECT * FROM records')

  # Inclusion and exclusion - creating a dataframe for use ------------------

    df <- RPostgres::dbGetQuery(con, 'SELECT * FROM records') %>% #I'm assuming this is the name of the dataframe from where we are retrieving the data, otherwise we need to change this
  dplyr::select(id,
                sex,
                birth_year,
                education_category_3,
                priority_memory_im_lm,
                priority_memory_dr_lm,
                priority_attention_test_tmt_a_time,
                priority_executive_tmt_b_time,
                scd_diagnosis,
                normal_cognition_diagnosis,
                date) %>%  # Selecting all the variables needed
  dplyr::filter(!is.na(education_category_3)
                & !is.na(sex)
                & !is.na(birth_year)
                & !is.na(priority_memory_im_lm)
                & !is.na(priority_memory_dr_lm)
                & !is.na(priority_attention_test_tmt_a_time)
                & !is.na(priority_executive_tmt_b_time)) %>%  # Remove rows with missing values for any of the columns
  dplyr::filter(scd_diagnosis == "yes" | normal_cognition_diagnosis == "yes") %>% #excludes everyone who isn't either healthy or has scd
  group_by(id) %>%
  slice_min(order_by = date) %>% #selects the earliest date available per participant

# Validation of available columns
check_names <- c("birth_year", "sex", "education_category_3","date", "scd_diagnosis", "normal_cognition_diagnosis", "priority_attention_test_tmt_a_time","priority_executive_tmt_b_time")
missing_variables <- c()
for (name in check_names) {
  if (!name %in% colnames(df)) {
    missing_variables <- c(name, missing_variables)
  }
}

# Transformations of variables --------------------------------------------
##calculating the age of the participant
df$age <- ifelse(is.na(df$age), as.numeric(format(df$date, "%Y")) - df$birth_year, df$age)

##Transforming the categories of education to the appropriate values + the dummy variables
df$education_category_3 <- factor(df$education_category_3, levels = c(0, 1, 2), labels = c("low", "medium", "high"))
df$education_low <- ifelse(df$education == 'low', 1, 0)
df$education_high <- ifelse(df$education == 'high', 1, 0)

##recoding sex - this is to make sure females = 0
df$sex <- dplyr::recode_factor(df$sex, "male" = 1, "female" = 0)

##calculating the interference score of the TMT
df$tmt_interference <- (df$priority_executive_tmt_b_time - df$priority_attention_test_tmt_a_time)

# Descriptive statistics --------------------------------------------------
##Descriptives delayed recall logical memory task, immediate recall logical memory task, and TMT A & TMT B
descriptives_by_sex_table <- df %>%
  group_by(sex) %>%
  summarise(
    nr_participants = n_distinct(id),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    mean_dr_lm = mean(priority_memory_dr_lm, na.rm = TRUE),
    sd_dr_lm = sd(priority_memory_dr_lm, na.rm = TRUE),
    mean_im_lm = mean(priority_memory_im_lm, na.rm = TRUE),
    sd_im_lm = sd(priority_memory_im_lm, na.rm = TRUE),
    mean_tmt_a = mean(priority_attention_test_tmt_a_time, na.rm = TRUE),
    sd_tmt_a = sd(priority_attention_test_tmt_a_time, na.rm = TRUE),
    mean_tmt_b = mean(priority_executive_tmt_b_time, na.rm = TRUE),
    sd_tmt_b = sd(priority_executive_tmt_b_time, na.rm = TRUE)
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
    sd_im_lm = sd(priority_memory_im_lm, na.rm = TRUE),
    mean_tmt_a = mean(priority_attention_test_tmt_a_time, na.rm = TRUE),
    sd_tmt_a = sd(priority_attention_test_tmt_a_time, na.rm = TRUE),
    mean_tmt_b = mean(priority_attention_test_tmt_b_time, na.rm = TRUE),
    sd_tmt_b = sd(priority_attention_test_tmt_b_time, na.rm = TRUE)
  )

descriptives_by_sex_and_edu_table <- df %>%
  group_by(sex, education_category_3) %>%
  summarise(nr_participants = n_distinct(id),
            mean_age = mean(age, na.rm = TRUE),
            sd_age = sd(age, na.rm = TRUE),
            mean_dr_lm = mean(priority_memory_dr_lm, na.rm = TRUE),
            sd_dr_lm = sd(priority_memory_dr_lm, na.rm = TRUE),
            mean_im_lm = mean(priority_memory_im_lm, na.rm = TRUE),
            sd_im_lm = sd(priority_memory_im_lm, na.rm = TRUE),
            mean_tmt_a = mean(priority_attention_test_tmt_a_time, na.rm = TRUE),
            sd_tmt_a = sd(priority_attention_test_tmt_a_time, na.rm = TRUE),
            mean_tmt_b = mean(priority_attention_test_tmt_b_time, na.rm = TRUE),
            sd_tmt_b = sd(priority_attention_test_tmt_b_time, na.rm = TRUE)
  )

descriptives_by_diagnosis_table <- df %>%
  group_by(scd_diagnosis, normal_cognition_diagnosis) %>%
  summarise(
    nr_participants = n_distinct(id),
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    mean_dr_lm = mean(priority_memory_dr_lm, na.rm = TRUE),
    sd_dr_lm = sd(priority_memory_dr_lm, na.rm = TRUE),
    mean_im_lm = mean(priority_memory_im_lm, na.rm = TRUE),
    sd_im_lm = sd(priority_memory_im_lm, na.rm = TRUE),
    mean_tmt_a = mean(priority_attention_test_tmt_a_time, na.rm = TRUE),
    sd_tmt_a = sd(priority_attention_test_tmt_a_time, na.rm = TRUE),
    mean_tmt_b = mean(priority_attention_test_tmt_b_time, na.rm = TRUE),
    sd_tmt_b = sd(priority_attention_test_tmt_b_time, na.rm = TRUE)
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

descriptives_tmt_a <- describe(df$priority_attention_test_tmt_a_time)
descriptives_tmt_a_stats <- df(
  mean = descriptives_tmt_a$mean,
  sd = descriptives_tmt_a$sd,
  min = descriptives_tmt_a$min,
  max = descriptives_tmt_a$max
)

descriptives_tmt_b <- describe(df$priority_executive_tmt_b_time)
descriptives_tmt_b_stats <- df(
  mean = descriptives_tmt_b$mean,
  sd = descriptives_tmt_b$sd,
  min = descriptives_tmt_b$min,
  max = descriptives_tmt_b$max
)

descriptives_tmt_interference <- describe(df$tmt_interference)
descriptives_tmt_interference_stats <- df(
  mean = descriptives_tmt_interference$mean,
  sd = descriptives_tmt_interference$sd,
  min = descriptives_tmt_interference$min,
  max = descriptives_tmt_interference$max
)

count_sex <- table(df$sex)
count_education <- table(df$education_category_3)
count_scd <- table(df$scd_diagnosis)
count_normal_cognition <- table(df$normal_cognition_diagnosis)

# Regression models -------------------------------------------------------
##Syntax regression model including age, sex, and education for immediate recall of logical memory
model_im_lm <- lm(df$priority_memory_im_lm ~ age + sex + education_low + education_high, data = df)

##Syntax regression model including age, sex, and education for delayed recall of logical memory
model_dr_lm <- lm(df$priority_memory_dr_lm ~ age + sex + education_low + education_high, data = df)

##Syntax regression model that includes age, sex, and education for TMT A
model_tmt_a <- lm(df$priority_attention_test_tmt_a_time ~ age + sex + education_low + education_high, data = df)

##Syntax regression model that includes age, sex, and education for TMT A
model_tmt_b <- lm(df$priority_executive_tmt_b_time ~ age + sex + education_low + education_high, data = df)

##Syntax regression model interference score TMT B - TMT A correcting for age, sex, and education
model_tmt_interference <- lm(df$interference ~ age + sex + education_low + education_high, data = df)

# Assumptions checks ------------------------------------------------------
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

#TMT A
df$ZPRED_tmt_a <- predict(model_tmt_a) #predicted z-score
df$ZRESID_tmt_a <- resid(model_tmt_a) #residuals of the

##Homoscedasticity
###Scatterplot of residuals - TMT A
scatter_res_tmt_a <- plot(df$ZPRED_tmt_a, df$ZRESID_tmt_a,
                          xlab = "Predicted Mean Test Score",
                          ylab = "Residuals",
                          main = "Scatterplot of TMT A",
                          pch = 19,  # point type
                          col = ifelse(df$sex == 1, "blue", "red"))
#Add a horizontal line at y = 0 to show where residuals = 0
abline(h = 0, col = "black", lty = 2)

##Noramlity of the errors - TMT A
histo_tmt_a <- hist(df$ZRESID_tmt_a, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm_tmt_a <- qqnorm(df$ZRESID_tmt_a)
qq_line_tmt_a <- qqline(df$ZRESID_tmt_a, col = "red")

z_pred_resid_descriptives_tmt_a <- describe(df %>% select(ZPRED_tmt_a, ZRESID_tmt_a))
print(z_pred_resid_descriptives_tmt_a)

#TMT B
df$ZPRED_tmt_b <- predict(model_tmt_b) #predicted z-score
df$ZRESID_tmt_b <- resid(model_tmt_b) #residuals of the model

##Homoscedasticity
###Scatterplot of residuals - TMT B
scatter_res_tmt_b <- plot(df$ZPRED_tmt_b, df$ZRESID_tmt_b,
                          xlab = "Predicted Mean Test Score",
                          ylab = "Residuals",
                          main = "Scatterplot of TMT B",
                          pch = 19,  # point type
                          col = ifelse(df$sex == 1, "blue", "red"))
#Add a horizontal line at y = 0 to show where residuals = 0
abline(h = 0, col = "black", lty = 2)

##Noramlity of the errors - TMT B
histo_tmt_b <- hist(df$ZRESID_tmt_b, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm_tmt_b <- qqnorm(df$ZRESID_tmt_b)
qq_line_tmt_b <- qqline(df$ZRESID_tmt_b, col = "red")

z_pred_resid_descriptives_tmt_b <- describe(df %>% select(ZPRED_tmt_b, ZRESID_tmt_b))
print(z_pred_resid_descriptives_tmt_b)

#TMT interference
df$ZPRED_interference <- predict(model_tmt_interference) #predicted z-score
df$ZRESID_interference <- resid(model_tmt_interference) #residuals of the model

##Homoscedasticity
###Scatterplot of residuals - TMT Interference
scatter_res_tmt_interference <- plot(df$ZPRED_tmt_interference, df$ZRESID_tmt_interference,
                          xlab = "Predicted Mean Test Score",
                          ylab = "Residuals",
                          main = "Scatterplot of TMT interference score",
                          pch = 19,  # point type
                          col = ifelse(df$sex == 1, "blue", "red"))
#Add a horizontal line at y = 0 to show where residuals = 0
abline(h = 0, col = "black", lty = 2)

##Noramlity of the errors - TMT Interference
histo_tmt_interference <- hist(df$ZRESID_tmt_interference, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm_tmt_interference <- qqnorm(df$ZRESID_tmt_interference)
qq_line_tmt_interference <- qqline(df$ZRESID_tmt_interference, col = "red")

z_pred_resid_descriptives_tmt_interference <- describe(df %>% select(ZPRED_tmt_interference, ZRESID_tmt_interference))
print(z_pred_resid_descriptives_tmt_interference)

# Sensitivity analysis ----------------------------------------------------
#Remove outliers (+/- 5 SD)
#To be continued when we know the rest of the syntax works (it's practically the same as the previous code).


# The output --------------------------------------------------------------
results <- list(
  "descriptives_by_sex" = descriptives_by_sex_table,
  "descriptives_by_edu" = descriptives_by_edu_table,
  "descriptives_by_sex_and_edu" = descriptives_by_sex_and_edu_table,
  "descriptives_by_diagnosis" = descriptives_by_diagnosis_table,
  "descriptives_im_lm" = descriptives_im_lm_stats,
  "descriptives_dr_lm" = descriptives_dr_lm_stats,
  "descriptives_tmt_a" = descriptives_tmt_a_stats,
  "descriptives_tmt_b" = descriptives_tmt_b_stats,
  "descriptives_tmt_interference" = descriptives_tmt_interference_stats,
  "count_sex" = count_sex,
  "count_education" = count_education,
  "count_scd" = count_scd,
  "count_normal_cognition" = count_normal_cognition,
  "model_im_lm" = model_im_lm,
  "model_dr_lm" = model_dr_lm,
  "model_tmt_a" = model_tmt_a,
  "model_tmt_b" = model_tmt_b,
  "model_tmt_interference" = model_tmt_interference,
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
  "scatter_res_tmt_a" = scatter_res_tmt_a,
  "histo_tmt_a" = histo_tmt_a,
  "qqnorm_tmt_a" = qqnorm_tmt_a,
  "qq_line_tmt_a" = qq_line_tmt_a,
  "z_pred_resid_descriptives_tmt_a" = z_pred_resid_descriptives_tmt_a,
  "scatter_res_tmt_b" = scatter_res_tmt_b,
  "histo_tmt_b" = histo_tmt_b,
  "qqnorm_tmt_b" = qqnorm_tmt_b,
  "qq_line_tmt_b" = qq_line_tmt_b,
  "z_pred_resid_descriptives_tmt_b" = z_pred_resid_descriptives_tmt_b,
  "scatter_res_tmt_interference" = scatter_res_tmt_interference,
  "histo_tmt_interference" = histo_tmt_interference,
  "qqnorm_tmt_interference" = qqnorm_tmt_interference,
  "qq_line_tmt_interference" = qq_line_tmt_interference,
  "z_pred_resid_descriptives_tmt_interference" = z_pred_resid_descriptives_tmt_interference,
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
