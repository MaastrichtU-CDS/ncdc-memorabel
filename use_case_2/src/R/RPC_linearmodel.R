RPC_linearmodel <- function(df, config, model = "memory", exclude=c()) {
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
    # Sub-cohort if selected
    cohort <- c()
    if ("subcohort" %in% names(config)) {
      cohort <- config[["subcohort"]]
    }

    query <- 'SELECT * FROM ncdc'
    # To select a specific cohort in the organization
    if (!(is.na(cohort)) && length(cohort) > 0) {
      cohorts_parsed <- paste(cohort, collapse=",")
      vtg::log$info(cohorts_parsed)
      query <- paste(
        "SELECT ROW_NUMBER() OVER() AS row, n.* FROM ncdc AS n LEFT JOIN PERSON AS p ON n.id = p.person_id WHERE p.care_site_id IN ('",
        cohorts_parsed,
        "')"
      )
    }
    df <- RPostgres::dbGetQuery(con, query)
    pre_summary <- summary_stats(df)
    # The dataframe will contain all the data harmonized for the cohort. The
    # variable names will be the same in all cohorts.
    # In any case, it's a best practice to validate that all columns are available
    check_names <- c("age", "sex", "bmi")
    missing_variables <- c()
    for (name in check_names) {
      if (!name %in% colnames(df)) {
        missing_variables <- c(name, missing_variables)
      }
    }

    if (length(missing_variables) > 0) {
      return(list(
        "error_message" = paste("Missing the following variables: ", paste(missing_variables, collapse=", "))
      ))
    }

    # Pre-processing the data
    df <- preprocessing(df, model, config)
    #data <- preprocessing(df, local_std)
    #df <- data["data"]
    if (nrow(df) == 0) {
      return(list(
        "error_message" = "Empty dataset: no participants selected"
      ))
    }
    outcomes = list(
      "memory" = "Priority_memory_im_z",
      "memory_dr" = "Priority_memory_dr_z",
      "attention" = "Priority_attention_z",
      "executive" = "Priority_executive_z",
      "processing_speed" = "Priority_speed_z",
      "language" = "Priority_language_z"
    )
    # Filter the data set based on the specific test
    # Otherwise, it'll be excluded by the model
    if ("model_only" %in% names(config) & config[["model_only"]] == TRUE) {
      cog_test <- outcomes[[model]]
      df <- df[!is.na(df[[cog_test]]), ]
    }

    # Sending all models results in an error:
    # 413 Request Entity Too Large
    results <- list(
      "models" = run_models(df, outcome=outcomes[[model]], exclude=exclude),
      "n" = nrow(df),
      "summary" = summary_stats(df),
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
