# Helper functions
# discretize_var <- function(vector, quantile){
#     ### Helper function for FCBF.
#     if(!is.numeric(vector)){stop("Feature must be numeric to discretize")}
#     results <- rep('l', length(vector))
#     results[vector > median(vector)] <- 'h'
#     return(results)
# }

FCBF_helper <- function(preds, outcome, min_su, n_cuts){
    ### Takes a set of predictors, does FCBF for feature selection, and
    ### returns the names of the features to keep.

    preds <- preds %>%
        purrr::map_if(is.numeric, ~recipes::discretize(.x, n_cuts))

    res <- FCBF::fcbf(feature_table = preds, target_vector = outcome, minimum_su = min_su,
                verbose = FALSE, samples_in_rows = TRUE)
    return(res)
}
