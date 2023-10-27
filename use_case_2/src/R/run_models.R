formula_builder <- function(outcome, variables) {
  formula <- as.formula(
    paste(
      outcome,
      paste(variables, collapse=" + "),
      sep = " ~ "
    )
  )
  return(formula)
}

run_models <- function(df, outcome="Priority_attention_z") {
  results <- list()
  # Model 0
  variables <- c("ZEDcomp")
  res0 <- lm(formula_builder(outcome, variables), data = df)
  # tbl_regression(res0)
  results[["m0"]] <- summary(res0)

  # Model 1
  variables <- c("ZEDcomp", "Age_cent", "sex", "Education_low", "Education_high")
  res1 <- lm(formula_builder(outcome, variables), data = df)
  results[["m1"]] <- summary(res1)

  # Model 2
  variables <- c(variables, c("bmi", "hypertension", "drugs_hypertension",
                              "hdl_ratio", "drugs_hypercholesterolemia",
                              "cardiovascular_disease", "dm_2"
                              )
                )
  res2 <- lm(formula_builder(outcome, variables), data = df)
  results[["m2"]] <- summary(res2)

  # Model 3
  variables <- c(variables, c("smoking_behavior", "alcohol"))
  res3 <- lm(formula_builder(outcome, variables), data = df)
  results[["m3"]] <- summary(res3)

  # Model 4
  # Drugs_depression was included in the model but it's not available in the
  # current dataset
  variables <- c(variables, c("depression"))
  res4 <- lm(formula_builder(outcome, variables), data = df)
  results[["m4"]] <- summary(res4)
  return(results)
}
