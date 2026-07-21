# blackboost (mboost) fits gradient boosting with partykit regression trees as
# base learners. The prediction is the constant offset plus the learning rate
# (`nu`) times the sum of the per-tree terminal-node predictions, so it can be
# expressed as `offset + nu * (tree_1 + tree_2 + ...)`. Only the Gaussian
# (squared error) regression family is supported; other families apply a
# non-linear response function that cannot be expressed as a single formula.

mboost_check_regression <- function(model) {
  if (!identical(model$family@name, "Squared Error (Regression)")) {
    cli::cli_abort(
      c(
        "Only the {.code Gaussian()} family is supported for blackboost.",
        i = "Family {.val {model$family@name}} applies a non-linear response function that cannot be expressed as a single formula."
      )
    )
  }
  invisible(model)
}

# Extract the boosting components: the constant offset, the learning rate `nu`,
# and one nested-tree info structure per boosting iteration. Each base learner
# is a partykit `partynode` fit on the negative gradient; its terminal-node
# predictions live in the `coef` table of the learner's `predict` closure. We
# rebuild a `party` object per tree with those predictions recorded as the
# fitted response so the shared partykit parser recovers them directly.
mboost_components <- function(model) {
  e <- rlang::fn_env(model$predict)
  ens <- get("ens", envir = e)
  nu <- get("nu", envir = e)
  offset <- get("offset", envir = e)

  tree_info_list <- map(ens, function(bm) {
    te <- rlang::fn_env(bm$predict)
    tree <- get("tree", envir = te)
    mymf <- get("mymf", envir = te)
    coef <- get("coef", envir = te)
    where <- get("where", envir = te)

    node_pred <- coef[unclass(where), 1]
    fitted <- data.frame(
      "(fitted)" = as.integer(as.character(where)),
      "(response)" = node_pred,
      "(weights)" = rep(1, length(node_pred)),
      check.names = FALSE
    )
    pt <- partykit::party(tree, data = mymf, fitted = fitted)
    partykit_tree_info_full(pt)
  })

  list(tree_info_list = tree_info_list, nu = nu, offset = offset)
}

# Combine per-tree expressions into the full boosting prediction.
mboost_build_formula <- function(tree_info_list, nu, offset) {
  tree_exprs <- map(tree_info_list, generate_nested_case_when_tree)
  res <- reduce_addition(tree_exprs)
  expr(!!offset + !!nu * !!res)
}

# Model parser -------------------------------------

#' @export
parse_model.blackboost <- function(model) {
  mboost_check_regression(model)

  comps <- mboost_components(model)

  pm <- list()
  pm$general$model <- "blackboost"
  pm$general$type <- "tree"
  pm$general$version <- 3
  pm$general$nu <- comps$nu
  pm$general$offset <- comps$offset
  pm$tree_info_list <- comps$tree_info_list
  as_parsed_model(pm)
}

# Fit formula -----------------------------------

#' @export
tidypredict_fit.blackboost <- function(model, ...) {
  mboost_check_regression(model)

  comps <- mboost_components(model)
  mboost_build_formula(comps$tree_info_list, comps$nu, comps$offset)
}
