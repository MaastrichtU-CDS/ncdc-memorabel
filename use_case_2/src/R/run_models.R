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

run_models <- function(df, outcome="Priority_attention_z", exclude=c()) {
  print("Run linear models")
  results <- list()
  # Model 0
  if (!("m0" %in% exclude)) {
    print("Model 0")
    variables <- c("ZEDcomp")
    res0 <- lm(formula_builder(outcome, setdiff(variables, exclude)), data = df)
    results[["m0"]] <- summary(res0)
  }

  # Model 1
  if (!("m1" %in% exclude)) {
    print("Model 1")
    variables <- c(variables, c("Age_cent", "sex", "Education_low", "Education_high"))
    res1 <- lm(formula_builder(outcome, setdiff(variables, exclude)), data = df)
    results[["m1"]] <- summary(res1)
  }

  # Model 2
  if (!("m2" %in% exclude)) {
    print("Model 2")
    variables <- c(variables, c("bmi", "hypertension", "drugs_hypertension",
                                "hdl_ratio", "drugs_hypercholesterolemia",
                                "cardiovascular_disease", "dm_type2")
                  )
    res2 <- lm(formula_builder(outcome, setdiff(variables, exclude)), data = df)
    results[["m2"]] <- summary(res2)
  }

  # Model 3
  if (!("m3" %in% exclude)) {
    print("Model 3")
    variables <- c(variables, c("smoking_behavior", "alcohol"))
    res3 <- lm(formula_builder(outcome, setdiff(variables, exclude)), data = df)
    results[["m3"]] <- summary(res3)
  }

  # Model 4
  # Drugs_depression not available in the current data set
  if (!("m4" %in% exclude)) {
    print("Model 4")
    variables <- c(variables, c("depression"))
    res4 <- lm(formula_builder(outcome, setdiff(variables, exclude)), data = df)
    results[["m4"]] <- summary(res4)
  }

  return(results)
}
