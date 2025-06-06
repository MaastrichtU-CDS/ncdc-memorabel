set_seed_config <- function(config) {
  if ("seed" %in% names(config)) {
    set.seed(config[["seed"]])
  }
}

check_responses <- function(responses) {
  if (length(responses) == 0) {
    return(c("No responses obtained from the node, please check the logs"))
  }
  error_message = list()
  for (i in 1:length(responses)) {
    if ("error_message" %in% names(responses[[i]])) {
      error_message <- c(error_message, responses[[i]][["error_message"]])
    }
  }
  if (length(error_message) > 0) {
    return(error_message)
  }
}

get_freq <- function(col) {
  min <- as.integer(Sys.getenv("MINIMUM_FREQ", unset=5))
  as.data.frame(table(col)) %>%
    dplyr::mutate(Freq = replace(Freq, Freq < min, min))
}

summary_stats <- function(df, model_variables=c()) {
  metrics = list(
    "n" = nrow(df),
    "sex" = get_freq(df$sex),
    "age" = summary(df$age),
    "education" = get_freq(df$education_category_3),
    "education_low" = get_freq(df$education_low),
    "education_high" = get_freq(df$education_high)
  )
  for (variable in model_variables) {
    metrics[[variable]] <- list(
      "summary" = summary(df[[variable]]),
      "sd" = sd(df[[variable]]),
      "na" = sum(is.na(df[[variable]])),
      "min" = min(is.na(df[[variable]])),
      "max" = min(is.na(df[[variable]])),
      "inf" = sum(is.finite(df[[variable]]))
    )
  }
  return(metrics)
}

model_summary <- function(model) {
  ms <- summary(model)
  return(list(
    "coefficients" = ms[["coefficients"]][["fixed"]],
    "fixDF" = model[["fixDF"]],
    "logLik" = model[["logLik"]],
    "varFix" = model[["varFix"]],
    "sigma" = model[["sigma"]],
    "contrasts" = model[["contrasts"]]
  ))
}
