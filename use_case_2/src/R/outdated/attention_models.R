attention_models <- function(df) {
  ### Attention
  attention <- list()
  # Model 0
  res0 <- lm(Priority_attention_z ~ ZEDcomp, data = df)
  # tbl_regression(res0)
  attention[["m0"]] <- summary(res0)

  # Model 1
  res1 <- lm(Priority_attention_z ~ ZEDcomp + Age_cent + sex + Education_low + Education_high, data = df)
  attention[["m1"]] <- summary(res1)

  # Model 2
  res2 <- lm(Priority_attention_z ~ ZEDcomp + Age_cent + sex + Education_low + Education_high +
               bmi + hypertension + drugs_hypertension + hdl_ratio + drugs_hypercholesterolemia +
               cardiovascular_disease + dm_2,
             data = df)
  attention[["m2"]] <- summary(res2)

  # Model 3
  res3 <- lm(Priority_attention_z ~ ZEDcomp + Age_cent + sex + Education_low + Education_high +
               bmi + hypertension + drugs_hypertension + hdl_ratio + drugs_hypercholesterolemia +
               cardiovascular_disease + dm_2 + smoking_behavior + alcohol, data = df)
  attention[["m3"]] <- summary(res3)

  # Model 4
  # Drugs_depression was included in the model but it's not available in the current
  # dataset
  res4 <- lm(Priority_attention_z ~ ZEDcomp + Age_cent + sex + Education_low + Education_high +
               bmi + hypertension + drugs_hypertension + hdl_ratio + drugs_hypercholesterolemia +
               cardiovascular_disease + dm_2 + smoking_behavior + alcohol + depression,
             data = df)
  attention[["m4"]] <- summary(res4)
  return(attention)
}
