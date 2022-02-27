# User-facing function
#' Fast Correlation Based Filter for Feature Selection
#'
#' step_fcbf takes a set of features and performs a fast correlation based filter, resulting in a smaller subset of features being selected. The number of features selected depends on the min_su threshold parameter (a lower threshold selects more features).
#'
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe.
#' @param ... Selector functions that specify which features should be considered by the FCBF.  e.g. all_numeric_predictors(), all_predictors()
#' @param min_su Minimum threshold for symmetrical uncertainty. Lower values allow more features to be selected.
#' @param outcome Outcome variable used for filter selection. If there is only one outcome variable in the recipe, it will automatically be detected. If multiple outcome variables exist, the user should specify it.
#' @param cutpoint Quantile value (0-1) describing how to split numeric features into binary nominal features. e.g. 0.5 = median split
#' @param features_retained Internal object that gives a record of which features were retained after FCBF. Should not be specified by the user.
#' @param role Not used for this step since new variables are not created
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param removals Feature columns that will be removed. Used internally and should not be set by the user.
#' @param skip A logical. Should the step be skipped when the recipe is baked by bake()? While all operations are baked when prep() is run, some operations may not be able to be conducted on new data (e.g. processing the outcome variable(s)). Care should be taken when using skip = TRUE as it may affect the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#'
#' @details step_fcbf takes a range of features (e.g. the full feature set) and selects a subset of features using the FCBF algorithm as described in Yu, L. and Liu, H. (2003).
#'
#' FCBF selects features to simultaneously minimize correlation between features and maximise correlations between the features and the target. FCBF only works with categorical features, so continuous features must first be discretized. By default this is based on a median split (i.e. splitting continuous variables into 'high' versus 'low'), but the method may be customized in the internal function 'discretize_var'.
#'
#'#' Code to implement the FCBF algorithm is driven by Bioconductor package FCBF. step_fcbf provides wrappers that allow it to be used within the tidymodels framework

#' @return Returns the recipe object, with step_fcbf added to the sequence of operations for this recipe.
#' @references Yu, L. and Liu, H. (2003); Feature Selection for High-Dimensional Data A Fast Correlation Based Filter Solution, Proc. 20th Intl. Conf. Mach. Learn. (ICML-2003), Washington DC, 2003.
#'
#' @export
#' @importFrom recipes rand_id add_step recipes_pkg_check
#' @importFrom rlang enquos .data
step_fcbf <- function (recipe, ..., min_su = 0.025, outcome = NA, cutpoint = 0.5,
                       features_retained = NA, role = NA, trained = FALSE,
                       removals = NULL, skip = FALSE, id = rand_id("FCBF")) {
    # Check for packages
    recipes_pkg_check(required_pkgs.step_fcbf())

    # Check arguments
    if(is.na(min_su)) rlang::abort("min_su must be a number between 0-1")
    if(is.na(cutpoint)) rlang::abort("cutpoint must be a number between 0-1")
    if(!is.numeric(min_su) | min_su >= 1 | min_su <= 0)
        rlang::abort("min_su must be a number between 0-1")
    if(!is.numeric(cutpoint) | cutpoint >= 1 | cutpoint <= 0)
        rlang::abort("cutpoint must be a number between 0-1")

    add_step(recipe, step_fcbf_new(terms = enquos(...), min_su = min_su,
                                   outcome = outcome, cutpoint = cutpoint,
                                   features_retained = features_retained,
                                   role = role, trained = trained,
                                   removals = removals, skip = skip, id = id))
}

# Constructor (boilerplate)
#' @importFrom recipes step
step_fcbf_new <- function (terms, min_su, outcome, cutpoint, features_retained,
                           role, trained, removals, skip, id) {
    step(subclass = "fcbf", terms = terms, min_su = min_su, outcome = outcome,
                  cutpoint = cutpoint, features_retained = features_retained,
                  role = role, trained = trained, removals = removals, skip = skip, id = id)
}

#' @importFrom recipes prep recipes_eval_select
#' @importFrom dplyr filter pull
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
#'
prep.step_fcbf <- function (x, training, info = NULL, ...){
    ## Find outcome column
    if(length(x$outcome) > 1){
        rlang::abort("Only a single outcome variable can be supplied for step_fcbf")
    }
    if(!is.na(x$outcome)){
        if(class(x$outcome) != "character"){
            rlang::abort("Outcome variable for step_fcbf must be supplied as a character string")
        }
        if(!x$outcome %in% names(training)){
            rlang::abort(paste0("Outcome variable '", x$outcome, "' not found"))
        }
        outcome_col <- x$outcome
        print(paste0("Outcome variable for FCBF set to: ", x$outcome))
    } else{
        print(paste0("Outcome not supplied in step_fcbf() argument. Automatically ",
                     "finding outcome from recipe specification"))
        outcome_col <- info %>% filter(role == 'outcome') %>% pull(variable)
        if(length(outcome_col)>1){
            rlang::abort(paste0("step_fcbf found more than one outcome variable.",
                                "Only a single outcome variable can be accepted by FCBF. Please",
                                "supply the outcome variable using the outcome argument in step_fcbf"))
        }
    }
    if(length(outcome_col)<1){
        rlang::abort(paste("An outcome variable was not found by step_fcbf. Please",
                           "ensure an outcome variable is specified."))
    }


    ## Get predictor columns to be provided to FCBF algorithm
    # Extract predictor columns selected by user or tidyselect functions
    preds_input <- recipes_eval_select(x$terms, training, info)

    # Exclude any columns full of NA values (can break FCBF function)
    preds_fcbf <- remove_NA_cols(preds_input, training)

    # Catch any issues that may break the FCBF code
    if (length(preds_fcbf) == 1) {
        # A message is not given by FCBF::fcbf when only 1 predictor is
        # provided to the filter.
        rlang::warn(paste0("Only one usable predictor was supplied to ",
                            "step_fcbf. FCBF may have unexpected results."))
    } else if (length(preds_fcbf) == 0){
        rlang::abort(paste0("No usable predictors were supplied to step_fcbf."))
    }

    ## Run FCBF
    fcbf_out <- FCBF_helper(preds = training[, preds_fcbf, drop = FALSE],
                            outcome = training[, outcome_col, drop = TRUE],
                            min_su = x$min_su,
                            cutpoint = x$cutpoint)

    cols_selected <- preds_fcbf[fcbf_out$index]

    # The "Number of features selected" message isn't given by FCBF when ncol = 1
    if(length(cols_selected) == 1) print("Number of features selected =  1")

    # Specify which cols to remove from the training set
    remove_cols <- preds_input[!preds_input %in% cols_selected]

    # Keep list of which predictors were retained (potentially useful for user)
    feats_retained <-
        info %>% filter(.data$role == 'predictor', !.data$variable %in% remove_cols)

    # keep_cols <- c(cols_selected, outcome_col)
    step_fcbf_new(terms = x$terms, min_su = x$min_su, outcome = x$outcome,
                  cutpoint = x$cutpoint,
                  features_retained = feats_retained, role = x$role, trained = TRUE,
                  removals = remove_cols, skip = x$skip, id = x$id)
}

#' @importFrom recipes bake
#' @importFrom tibble as_tibble
#' @export
bake.step_fcbf <- function (object, new_data, ...) {
    if (length(object$removals) > 0)
        new_data <- new_data[, !colnames(new_data) %in% object$removals]
    as_tibble(new_data)

}

#' @export
#' @importFrom recipes print_step
print.step_fcbf <- function (x, width = max(20, options()$width - 36), ...){
    if(x$trained){
        title <- "FCBF retained : "
        print_step(x$features_retained$variable, x$terms, x$trained, title, width)
        title <- "FCBF removed: "
        print_step(x$removals, x$terms, x$trained, title, width)
    } else {
        print_step(untr_obj = x$terms, title = "FCBF applied to features: ", width = width)
    }
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
required_pkgs.step_fcbf <- function(x, ...) {
    c("FCBF")
}
