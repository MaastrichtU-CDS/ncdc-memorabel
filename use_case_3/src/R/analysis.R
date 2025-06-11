analysis <- function(client, model, config=list(), exclude=c()) {
    vtg::log$info("Running UC2 analysis")
    analysis_result = tryCatch({
        pkg.name <- getPackageName()
        # sd <- names(Sys.getenv())
        # vtg::log$info(paste(sd, collapse=" - "))
        image.name <- Sys.getenv("IMAGE_NAME")

        image.name <- 'pmateus/usecase3:1.0.0'

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
                    vtg::log$info("Organization '{collaboration$id}' included.")
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
        # Single script
        # responses <- client$call(
        #     "models_mmse_apoe",
        #     config=config,
        #     model=model,
        #     exclude=exclude
        # )
        #
        # Multiple scripts
        responses <- list()
        models_to_run <- c("models_LASA_stratified_apoe", "models_mmse_apoe", "models_apoe_2_w_interaction",
                           "models_sex_3_w_interaction", "models_stratified_sex", "models_overall_model",
                           "models_mmse_sex", "models_apoe_3_w_interaction")
        # models_to_run <- c("models_apoe_2_w_interaction", "models_mmse_apoe", "models_LASA_stratified_apoe",
        #                   "models_CS_overall_model", "models_stratified_sex", "models_sex_2_w_interaction")
        for (model_id in models_to_run) {
          responses[model_id] <- client$call(
            model_id,
            config=config,
            model=model,
            exclude=exclude
          )
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
