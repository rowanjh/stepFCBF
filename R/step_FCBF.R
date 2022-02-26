# User-facing function
#' Fast Correlation Based Filter for Feature Selection
#'
#' step_FCBF takes a set of features and performs a fast correlation based filter, resulting in a smaller subset of features being selected. The number of features selected depends on the min_su threshold parameter (a lower threshold selects more features).
#'
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param ... Selector functions that specify which features should be considered by the FCBF.  e.g. all_numeric_predictors(), all_predictors()
#' @param min_su Minimum threshold for symmetrical uncertainty. Lower values allow more features to be selected.
#' @param outcome Outcome variable used for filter selection. If there is only one outcome variable in the recipe, it will automatically be detected. If multiple outcome variables exist, the user should specify it.
#' @param features_retained Internal object that gives a record of which features were retained after FCBF. Should not be specified by the user.
#' @param role Not used for this step since new variables are not created
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param removals Feature columns that will be removed. Used internally and should not be set by the user.
#' @param skip A logical. Should the step be skipped when the recipe is baked by bake()? While all operations are baked when prep() is run, some operations may not be able to be conducted on new data (e.g. processing the outcome variable(s)). Care should be taken when using skip = TRUE as it may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @details step_FCBF takes a range of features (e.g. the full feature set) and selects a subset of features using the FCBF algorithm as described in Yu, L. and Liu, H. (2003).
#'
#' FCBF selects features to simultaneously minimize correlation between features and maximise correlations between the features and the target. FCBF only works with categorical features, so continuous features must first be discretized. By default this is based on a median split (i.e. splitting continuous variables into 'high' versus 'low'), but the method may be customized in the internal function 'discretize_var'.
#'
#'#' Code to implement the FCBF algorithm is driven by Bioconductor package FCBF. step_FCBF provides wrappers that allow it to be used within the tidymodels framework

#' @return Returns the recipe object, with step_FCBF added to the sequence of operations for this recipe.
#' @references Yu, L. and Liu, H. (2003); Feature Selection for High-Dimensional Data A Fast Correlation Based Filter Solution, Proc. 20th Intl. Conf. Mach. Learn. (ICML-2003), Washington DC, 2003.
#'
#'
#' @examples test
#'
#' @export
step_FCBF <- function (recipe, ..., min_su = 0.025, outcome = NA, cutpoint = 0.5,
                       features_retained = NA, role = NA, trained = FALSE,
                       removals = NULL, skip = FALSE, id = rand_id("FCBF")) {
    # Check arguments

    recipes::recipes_pkg_check(required_pkgs.step_FCBF())

    add_step(recipe, step_FCBF_new(terms = enquos(...), min_su = min_su,
                                   outcome = outcome, cutpoint = cutpoint,
                                   features_retained = features_retained,
                                   role = role, trained = trained,
                                   removals = removals, skip = skip, id = id))
}

# Constructor (boilerplate)
step_FCBF_new <- function (terms, min_su, outcome, cutpoint, features_retained,
                           role, trained, removals, skip, id) {
    step(subclass = "FCBF", terms = terms, min_su = min_su, outcome = outcome,
         cutpoint = cutpoint, features_retained = features_retained,
         role = role, trained = trained, removals = removals, skip = skip, id = id)
}

#' @importFrom recipes prep
#' @export
#'
prep.step_FCBF <- function (x, training, info = NULL, ...){
    # Find outcome column
    if(!is.na(x$outcome)){
        if(class(x$outcome) != "character"){
            rlang::abort("Outcome variable for step_FCBF must be supplied as a character string")
        }
        if(length(x$outcome) > 1){
            rlang::abort("Only a single outcome variable can be supplied for step_FCBF")
        }
        outcome_col <- x$outcome
    } else{
        outcome_col <- info %>% dplyr::filter(role == 'outcome') %>% dplyr::pull(variable)
        if(length(outcome_col)>1){
            rlang::abort(paste0("step_FCBF found more than one outcome variable.",
                                "Only a single outcome variable can be accepted by FCBF. Please",
                                "supply the outcome variable using the outcome argument in step_FCBF"))
        }
    }
    if(length(outcome_col)<1|is.na(outcome_col)){
        rlang::abort(paste("An outcome varaible was not found by step_FCBF. Please",
                           "ensure an outcome variable is specified."))
    }
    # Get predictor columns to be used in FCBF
    pred_cols <- recipes_eval_select(x$terms, training, info)

    if (length(pred_cols) <= 1) {
        # this message is not given by FCBF::fcbf when only 1 predictor is selected by the filter
        rlang::warn("Fewer than two predictors were supplied to step_FCBF, FCBF will not be conducted")
    }
    fcbf_out <- FCBF_helper(preds = training[, pred_cols],
                            outcome = training[, outcome_col, drop = TRUE],
                            min_su = x$min_su,
                            cutpoint = x$cutpoint)
    cols_selected <- pred_cols[fcbf_out$index]
    if(length(cols_selected) == 1) print("Number of features selected =  1")

    # Specify which cols to remove from the training set
    remove_cols <- pred_cols[!pred_cols %in% cols_selected]

    # Keep a record of which predictors were retained (potentially useful after fitting resamples etc.)
    vars_retained <-
        info %>% dplyr::filter(role == 'predictor', !variable %in% remove_cols)

    # keep_cols <- c(cols_selected, outcome_col)
    step_FCBF_new(terms = x$terms, min_su = x$min_su, outcome = x$outcome,
                  cutpoint = x$cutpoint,
                  features_retained = vars_retained, role = x$role, trained = TRUE,
                  removals = remove_cols, skip = x$skip, id = x$id)
}

#' @importFrom recipes bake
#' @export
bake.step_FCBF <- function (object, new_data, ...) {
    if (length(object$removals) > 0)
        new_data <- new_data[, !colnames(new_data) %in% object$removals]
    as_tibble(new_data)

}

#' @export
print.step_FCBF <- function (x, width = max(20, options()$width - 36), ...){
    title <- "FCBF retained : "
    print_step(x$features_retained$variable, x$terms, x$trained, title, width)
    title <- "FCBF removed: "
    print_step(x$removals, x$terms, x$trained, title, width)

    invisible(x)
}

#' S3 methods for tracking which additional packages are needed for steps.
#'
#' Recipe-adjacent packages always list themselves as a required package so that
#' the steps can function properly within parallel processing schemes.
#' @param x A recipe step
#' @return A character vector
#' @rdname required_pkgs.embed
#' @keywords internal
#' @export
required_pkgs.step_FCBF <- function(x, ...) {
    c("FCBF")
}
