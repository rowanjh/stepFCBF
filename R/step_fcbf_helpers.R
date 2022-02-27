# Helper functions
discretize_var <- function(numeric_feat, cutpoint){
    ### Helper function for FCBF.
    # For an odd-length vector, median gets included in 'l' (low) group
    # NAs are ignored and will remain as NA in the discretized variable
    if(!is.numeric(numeric_feat)){
        rlang::abort("Feature must be numeric to discretize")
    }
    cut <- quantile(numeric_feat, cutpoint, na.rm = TRUE)
    results <- rep(NA, length(numeric_feat)) # initialize all as NA
    results[numeric_feat <= cut] <- 'l' # below cut as 'low'
    results[numeric_feat > cut] <- 'h' # set values above cut as 'high'
    return(as.factor(results))
}

FCBF_helper <- function(preds, outcome, min_su, cutpoint){
    ### Takes a set of predictors, does FCBF for feature selection, and
    ### returns the names of the features to keep.

    preds <- preds %>%
        purrr::map_if(is.numeric, ~discretize_var(.x, cutpoint = cutpoint)) %>%
        purrr::map_if(function(x){!is.factor(x)}, ~as.factor(.x)) %>%
        as.data.frame()

    res <- FCBF::fcbf(feature_table = preds, target_vector = outcome, minimum_su = min_su,
                verbose = FALSE, samples_in_rows = TRUE)
    return(res)
}


remove_NA_cols <- function(pred_colnames, df){
    ### Takes a df and character vector of columns names. Columns full of NA are
    ### removed from the character vector
    NAcols <- purrr::map_lgl(df[, pred_colnames], ~all(is.na(.x)))
    if(sum(NAcols) > 0){
        rlang::warn(paste(sum(NAcols), "features were full of NAs and removed prior to FCBF"))
    }
    return(pred_colnames[!NAcols])
}
