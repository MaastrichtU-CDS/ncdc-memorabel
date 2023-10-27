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
  'pmateus/usecase2:1.0.0',
  task.name="analysis"
)

config = list(
  "org_ids"= c('2', '3')
  # Run only for a sub-cohort (by ID):
  # subcohort=c(5)
)

# Run the analysis
client$use.master.container <- TRUE
client$data_format <- "RDS"
result <- client$call('analysis', 'memory_dr', config)
