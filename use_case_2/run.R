setup.client <- function() {
  # Define parameters
  username <- ""
  password <- ""
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

client$set.task.image(
  'pmateus/usecase2:1.0.0',
  task.name="analysis"
)

# Run the bayesian network algorithm
client$use.master.container <- TRUE
client$data_format <- "RDS"
result <- client$call('analysis')
