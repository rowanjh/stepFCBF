#' Prep method
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
                            n_cuts = n_cuts)
    cols_selected <- pred_cols[fcbf_out$index]
    if(length(cols_selected) == 1) print("Number of features selected =  1")

    # Specify which cols to remove from the training set
    remove_cols <- pred_cols[!pred_cols %in% cols_selected]

    # Keep a record of which predictors were retained (potentially useful after fitting resamples etc.)
    vars_retained <-
        info %>% dplyr::filter(role == 'predictor', !variable %in% remove_cols)

    # keep_cols <- c(cols_selected, outcome_col)
    step_FCBF_new(terms = x$terms, min_su = x$min_su, outcome = x$outcome,
                  n_cuts = x$n_cuts,
                  features_retained = vars_retained, role = x$role, trained = TRUE,
                  removals = remove_cols, skip = x$skip, id = x$id)
}

#' Bake method
#' @importFrom recipes bake
#' @export
bake.step_FCBF <- function (object, new_data, ...) {
    if (length(object$removals) > 0)
        new_data <- new_data[, !colnames(new_data) %in% object$removals]
    as_tibble(new_data)
}

# print.step_FCBF <- function (x, width = max(20, options()$width - 36), ...){
#     cat("FCBF applied", sep = " ")
#     printer(
#         # Names before prep (could be selectors)
#         untr_obj = x$terms,
#         # Names after prep:
#         tr_obj = names(x$keep_cols),
#         # Has it been prepped?
#         trained = x$trained,
#         # An estimate of how many characters to print on a line:
#         width = width
#     )
# }
