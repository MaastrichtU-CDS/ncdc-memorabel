# Analysis
library(haven)
library(tidyverse)
#library(plyr)
library(ggplot2)
library(gtsummary)
library(flextable)
library(modelsummary)
library(broom)
# Postgres DB
library(DBI)
library(RPostgres)

# Client for the database
password <- ""
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "",
  host = "",
  port = ,
  password = password,
  user = ""
)

# The most straightforward approach is to load all data from the NCDC table.
# If you already have a script, it can be adapted by changing the variable names
# to the ones used in the PHT (check ./destination_mapping.csv).
df <- dbGetQuery(con, 'SELECT * FROM ncdc')

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

stopifnot(length(missing_variables) == 0)

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
