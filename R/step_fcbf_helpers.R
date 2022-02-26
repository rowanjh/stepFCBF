# Helper functions
discretize_var <- function(vector, cutpoint){
    ### Helper function for FCBF.
    if(!is.numeric(vector)){
        rlang::abort("Feature must be numeric to discretize")
    }
    if(cutpoint <=0 | cutpoint >=1){
        rlang::abort("cutpoint must be between 0 and 1")
    }
    results <- rep('l', length(vector))
    cut <- quantile(vector, cutpoint)
    results[vector > cut] <- 'h'
    return(results)
}

FCBF_helper <- function(preds, outcome, min_su, cutpoint){
    ### Takes a set of predictors, does FCBF for feature selection, and
    ### returns the names of the features to keep.

    preds <- preds %>%
        purrr::map_if(is.numeric, ~discretize_var(.x, cutpoint = cutpoint)) %>%
        as_tibble()
    res <- FCBF::fcbf(feature_table = preds, target_vector = outcome, minimum_su = min_su,
                verbose = FALSE, samples_in_rows = TRUE)
    return(res)
}
