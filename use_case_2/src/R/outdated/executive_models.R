executive_models <- function(df) {
  ### Attention
  executive <- list()
  # Model 0
  res0 <- lm(Priority_executive_z ~ ZEDcomp, data = df)
  # tbl_regression(res0)
  executive[["m0"]] <- summary(res0)

  # Model 1
  res1 <- lm(Priority_executive_z ~ ZEDcomp + Age_cent + sex + Education_low + Education_high, data = df)
  executive[["m1"]] <- summary(res1)

  # Model 2
  res2 <- lm(Priority_executive_z ~ ZEDcomp + Age_cent + sex + Education_low + Education_high +
               bmi + hypertension + drugs_hypertension + hdl_ratio + drugs_hypercholesterolemia +
               cardiovascular_disease + dm_2,
             data = df)
  executive[["m2"]] <- summary(res2)

  # Model 3
  res3 <- lm(Priority_executive_z ~ ZEDcomp + Age_cent + sex + Education_low + Education_high +
               bmi + hypertension + drugs_hypertension + hdl_ratio + drugs_hypercholesterolemia +
               cardiovascular_disease + dm_2 + smoking_behavior + alcohol, data = df)
  executive[["m3"]] <- summary(res3)

  # Model 4
  # Drugs_depression was included in the model but it's not available in the current
  # dataset
  res4 <- lm(Priority_executive_z ~ ZEDcomp + Age_cent + sex + Education_low + Education_high +
               bmi + hypertension + drugs_hypertension + hdl_ratio + drugs_hypercholesterolemia +
               cardiovascular_disease + dm_2 + smoking_behavior + alcohol + depression,
             data = df)
  executive[["m4"]] <- summary(res4)
  return(executive)
}
