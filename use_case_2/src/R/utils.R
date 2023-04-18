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
