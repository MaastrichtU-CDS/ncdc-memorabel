analysis <- function(client, config=list()) {
    vtg::log$info("Running UC2 analysis")
    analysis_result = tryCatch({
        pkg.name <- getPackageName()
        # sd <- names(Sys.getenv())
        # vtg::log$info(paste(sd, collapse=" - "))
        image.name <- Sys.getenv("IMAGE_NAME")
        # To run a specific docker image, you must specify it here due
        # to a problem with the R version of vtg
        image.name <- 'pmateus/usecase2:1.0.0'

        client$set.task.image(
            image.name,
            task.name="analysis"
        )

        # Send off a new container if necessary
        if (client$use.master.container) {
            vtg::log$info(
                "Running `analysis` in master container using image '{image.name}'")
            result <- client$call("analysis")
            return(result)
        }
        vtg::log$info("Running `analysis` locally")

        # Update the client organizations according to the ones selected
        orgs <- list()
        collaboration_org_ids = client$collaboration$organizations
        selected_orgs <- list()
        if ("org_ids" %in% names(config)) {
            vtg::log$info("Setting up the organizations for the analysis")
            client$collaboration$organizations <- list()
            for (collaboration in collaboration_org_ids) {
                if (collaboration$id %in% config[["org_ids"]]) {
                    selected_orgs <- append(selected_orgs, list(collaboration))
                }
            }
        }
        if (length(selected_orgs) > 0) {
            client$collaboration$organizations <- selected_orgs
        }

        # Initialize the seed in case it isn't provided
        seed <- sample(1:10000, 1)
        if ("seed" %in% names(config)) {
            seed <- config[["seed"]]
        } else {
            vtg::log$info("Using a random seed '{seed}'")
            config[["seed"]] <- seed
        }
        set.seed(seed)

        # Run the linear models
        responses <- client$call(
            "linearmodel"
        )
        error_check = check_responses(responses)
        if (!is.null(error_check)) {
            return(error_check)
        }
        results <- responses
        return(results)
    }, error = function(e) {
        vtg::log$info("Error while running the master")
        vtg::log$info(e)
        return(list(
            "error_message" = paste("Error running the master:", e, sep=" ")
        ))
    })
    return(analysis_result)
}
