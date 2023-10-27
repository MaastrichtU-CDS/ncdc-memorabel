memory_dr_models <- function(df) {
  ### Memory - Delayed recall
  memory <- list()
  # Model 0
  res0 <- lm(Priority_memory_dr_z ~ ZEDcomp, data = df)
  memory[["m0"]] <- summary(res0)

  # # Model 1
  res1 <- lm(Priority_memory_dr_z ~ ZEDcomp + Age_cent + sex + Education_low + Education_high, data = df)
  memory[["m1"]] <- summary(res1)


  # Model 2
  res2 <- lm(Priority_memory_dr_z ~ ZEDcomp + Age_cent + sex + Education_low + Education_high +
               bmi + hypertension + drugs_hypertension + hdl_ratio + drugs_hypercholesterolemia +
               cardiovascular_disease + dm_2,
             data = df)
  memory[["m2"]] <- summary(res2)

  # # Model 3
  res3 <- lm(Priority_memory_dr_z ~ ZEDcomp + Age_cent + sex + Education_low + Education_high +
               bmi + hypertension + drugs_hypertension + hdl_ratio + drugs_hypercholesterolemia +
               cardiovascular_disease + dm_2 + smoking_behavior + alcohol, data = df)
  memory[["m3"]] <- summary(res3)


  # # Model 4
  # # Removed Drugs_depression
  res4 <- lm(Priority_memory_dr_z ~ ZEDcomp + Age_cent + sex + Education_low + Education_high +
               bmi + hypertension + drugs_hypertension + hdl_ratio + drugs_hypercholesterolemia +
               cardiovascular_disease + dm_2 + smoking_behavior + alcohol + depression,
             data = df)
  memory[["m4"]] <- summary(res4)
  return(memory)
}
