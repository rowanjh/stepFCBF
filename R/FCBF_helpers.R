# Helper functions
discretize_var <- function(vector){
    ### Helper function for FCBF.
    if(!is.numeric(vector)){stop("Feature must be numeric to discretize")}
    results <- rep('l', length(vector))
    results[vector > median(vector)] <- 'h'
    return(results)
}

FCBF_helper <- function(preds, outcome, min_su){
    ### Takes a set of predictors, does FCBF for feature selection, and
    ### returns the names of the features to keep.

    preds <- preds %>% purrr::map_if(is.numeric, discretize_var) %>% as.data.frame

    res <- FCBF::fcbf(feature_table = preds, target_vector = outcome, minimum_su = min_su,
                verbose = FALSE, samples_in_rows = TRUE)
    return(res)
}
