#install.packages('devtools')
# This also installs the package vtg
install.packages("C:\\Users\\inigo.bermejo\\Documents\\Src\\vtg.glm\\src", repos = NULL, type="source")
#devtools::install_github('iknl/vtg.glm', subdir='src')

setup.client <- function() {
  # Define parameters
  username <- "inigo"
  password <- "xxxxxxx"
  host <- 'https://dev.v6.personalhealthtrain.net'
  api_path <- '/api'
  
  # Create the client
  client <- vtg::Client$new(host, api_path=api_path)
  client$authenticate(username, password)
  
  return(client)
}

# Create a client
client <- setup.client()

# Set collaboration to Memorabel
collaborations <- client$getCollaborations()
collaborationId <- as.numeric(collaborations[collaborations$name == 'X-omics demo',]$id)
client$setCollaborationId(4)

# vtg.dglm contains the function `dglm`.
model_hdlchol <- vtg.glm::dglm(client, formula = hdlchol ~ bmi + sex + age, family="gaussian",tol= 1e-08)

model_try <- vtg.glm::dglm(client, formula = triglycerides ~ bmi + sex + age, family="gaussian",tol= 1e-08)

model_totchol <- vtg.glm::dglm(client, formula = totchol ~ bmi + sex + age, family="gaussian",tol= 1e-08)

summary(model_hdlchol)
summary(model_try)
summary(model_totchol)

model_hdlchol$coefficients
model_try$coefficients
model_totchol$coefficients
