# Prep method
prep.step_FCBF <- function (x, training, info = NULL, ...) {
    pred_cols <- info %>% dplyr::filter(role == 'predictor') %>% dplyr::pull(variable)
    outcome_col <- info %>% dplyr::filter(role == 'outcome') %>% dplyr::pull(variable)

    if(length(outcome_col)>1){
        rlang::abort("More than one outcome variable input to prep.step_FCBF")
    }
    if (length(pred_cols) > 1) {
        fcbf_out <- FCBF_helper(preds = training[, pred_cols],
                                outcome = training[, outcome_col, drop = TRUE],
                                min_su = x$min_su)
        cols_selected <- rownames(fcbf_out)
    }
    else {
        message("FCBF skipped: not enough predictors were detected (<2)")
        cols_selected <- pred_cols
    }
    keep_cols <- c(cols_selected, outcome_col)
    step_FCBF_new(terms = x$terms, role = x$role, trained = TRUE,
                  min_su = x$min_su,
                  keep_cols = keep_cols, skip = x$skip, id = x$id)
}

# Bake method
bake.step_FCBF <- function (object, new_data, ...) {
    if (length(object$keep_cols) > 0)
        new_data <- new_data[, object$keep_cols]
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
