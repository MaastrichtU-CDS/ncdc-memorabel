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

summary_stats <- function(df) {
  model_variables <- c(
    "ZEDcomp", "Age_cent", "Education_low", "Education_high", "bmi",
    "hypertension", "drugs_hypertension", "hdl_ratio",
    "drugs_hypercholesterolemia", "cardiovascular_disease",
    "myocardial_infarction", "stroke", "dm_2", "dm", "dm_type2",
    "hdl_ratio", "hdl", "ldl",
    "smoking_behavior", "alcohol", "depression", "Priority_memory_im_z",
    "Priority_memory_dr_z", "Priority_attention_z", "Priority_executive_z",
    "Priority_speed_z", "Priority_language_z"
  )
  metrics = list(
    "n" = nrow(df),
    "sex" = get_freq(df$sex),
    "age" = summary(df$age),
    "education" = get_freq(df$education_category_3)
  )
  for (variable in model_variables) {
    metrics[[variable]] <- sum(is.na(df[[variable]]))
  }
  return(metrics)
}
