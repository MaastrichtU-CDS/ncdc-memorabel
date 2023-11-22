setup.client <- function() {
  # Define parameters
  username <- "researcher"
  password <- "password"
  host <- 'http://localhost:5000'
  api_path <- '/api'

  # Create the client
  client <- vtg::Client$new(host, api_path=api_path)
  client$authenticate(username, password)

  return(client)
}

# Create a client
client <- setup.client()

# Get a list of available collaborations
print( client$getCollaborations() )

# Select a collaboration
client$setCollaborationId(1)

# Select the algorithm
client$set.task.image(
  'pmateus/usecase2:2.6.1',
  task.name="analysis"
)

config = list(
  "org_ids"= c('2', '3')
  # Run only for a sub-cohort (by ID):
  # subcohort=c(5)
  # If true, uses the variable for diabetes type 1 or 2 (if available)
  # Otherwise, uses the variable for diabetes type 2 only (dm_2)
  # dm_all=FALSE,
  # Apply the logarithmic function to the biomarker measurements
  # log_bio=FALSE,
  # Specific for Maastricht to reproduce previous results
  # model_only_ms=TRUE,
  # Exclude participants without the cognitive test prior
  # to running the models
  # model_only=FALSE
)

# Run the analysis
client$use.master.container <- TRUE
client$data_format <- "RDS"
# main function name (don't change) - "analysis"
# cognitive domain - "memory", "memory_dr", "attention", "executive", "language"
# configurations (list) - config
# Variables or models to exclude from the analysis - "m0", "m1", "age", ...
result <- client$call('analysis', 'memory_dr', config, exclude=c())
