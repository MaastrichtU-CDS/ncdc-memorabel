attention_models <- function(df) {
  ### Attention
  attention <- list()
  # Model 0
  res0 <- lm(df$Priority_attention_z ~ ZEDcomp, data = df)
  # tbl_regression(res0)
  attention[["m0"]] <- summary(res0)

  # Model 1
  res1 <- lm(df$Priority_attention_z ~ ZEDcomp + Age_cent + sex + Education_low + Education_high, data = df)
  attention[["m1"]] <- summary(res1)

  # # Model 2
  # res2 <- lm(Priority_attention_z ~ ZEDcomp + Age_cent + Sex + Education_low + Education_high +
  #              BMI + Hypertension + Drugs_hypertension + N_Chol_ratio + Drugs_hypercholesterolemia +
  #              N_CVD + DM_2,
  #            data = df)
  # attention[["m2"]] <- summary(res2)
  #
  # # Model 3
  # res3 <- lm(Priority_attention_z ~ ZEDcomp + Age_cent + Sex + Education_low + Education_high +
  #              BMI + Hypertension + Drugs_hypertension + N_Chol_ratio + Drugs_hypercholesterolemia +
  #              N_CVD + DM_2 + Smoking + Alcohol, data = df)
  # attention[["m3"]] <- summary(res3)
  #
  # # Model 4
  # res4 <- lm(Priority_attention_z ~ ZEDcomp + Age_cent + Sex + Education_low + Education_high +
  #              BMI + Hypertension + Drugs_hypertension + N_Chol_ratio + Drugs_hypercholesterolemia +
  #              N_CVD + DM_2 + Smoking + Alcohol + Depression + Drugs_depression,
  #            data = df)
  # attention[["m4"]] <- summary(res4)
  return(attention)
}
