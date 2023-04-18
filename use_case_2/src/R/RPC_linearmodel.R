RPC_linearmodel <- function(df) {
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
    df <- RPostgres::dbGetQuery(con, 'SELECT * FROM records')

    # The dataframe will contain all the data harmonized for the cohort. The
    # variable names will be the same in all cohorts.
    # In any case, it's a best practice to validate that all columns are available
    check_names <- c("birth_year", "age", "sex", "education_category", "hypertension", "smoking_behavior", "dementia_diagnosis")
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

    # Identifying the participants that need to be excluded can be done by
    # checking if the variable value is NULL.
    excluded <- unique(
      df$id[is.na(df$birth_year) |is.na(df$sex)]
    )

    # Selected participants
    included <- unique(df$id[! df$id %in% excluded])
    df <- df[df$id %in% included,]

    # Create the necessary transformations
    # current_year <- format(Sys.Date(), "%Y")
    # Year of birth will always be available (mandatory in OMOP), age is not guaranteed
    df <- transform(df, age_rec=ifelse(is.na(age), as.numeric(format(df$date, "%Y")) - df$birth_year, age))
    df$age2 <- df$age_rec^2

    # Model 0
    res0 <- lm(dementia_diagnosis ~ age2 + sex + hypertension, data = df)

    return(
      list(
        model0 <- res0
      )
    )
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
