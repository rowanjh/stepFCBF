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
step_FCBF <- function (recipe, ..., min_su = 0.025, outcome = NA, features_retained = NA,
                       role = NA, trained = FALSE, removals = NULL, skip = FALSE, id = rand_id("FCBF")) {

    add_step(recipe, step_FCBF_new(terms = enquos(...), min_su = min_su,
                                   outcome = outcome, features_retained = features_retained,
                                   role = role, trained = trained,
                                   removals = removals, skip = skip, id = id))
}

# Constructor (boilerplate code)
step_FCBF_new <- function (terms, min_su, outcome, features_retained,
                           role, trained, removals, skip, id) {
    step(subclass = "FCBF", terms = terms, min_su = min_su, outcome = outcome,
         features_retained = features_retained, role = role, trained = trained,
         removals = removals, skip = skip, id = id)
}


