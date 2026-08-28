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

  # Subsetting a fitted model (`model[m]`, as the `cvrisk()` workflow does) sets
  # `mstop` in this same environment but leaves `ens` at its full length, so the
  # ensemble has to be truncated to the iterations the model actually uses.
  # Raising `mstop` grows `ens`, so truncation is always the right operation.
  ens <- ens[seq_len(get("mstop", envir = e))]

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
  tree_exprs <- map(
    tree_info_list,
    \(tree_info) generate_nested_case_when_tree(tree_info, missing = "na")
  )
  mboost_combine(tree_exprs, nu, offset)
}

mboost_combine <- function(tree_exprs, nu, offset) {
  expr(!!offset + !!nu * !!reduce_addition(tree_exprs))
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

  tidypredict_combine_trees(model, tidypredict_trees(model))
}

# Extractors --------------------------------------

#' @export
tidypredict_trees.blackboost <- function(x, ...) {
  rlang::check_dots_empty()
  mboost_check_regression(x)

  map(
    mboost_components(x)$tree_info_list,
    \(tree_info) generate_nested_case_when_tree(tree_info, missing = "na")
  )
}

#' @export
tidypredict_n_trees.blackboost <- function(x, ...) {
  rlang::check_dots_empty()

  # `mstop` rather than the raw ensemble length: subsetting a fitted model
  # leaves `ens` at full length, and `mboost_components()` truncates to the
  # iterations the model actually uses.
  length(mboost_components(x)$tree_info_list)
}

# Boosting, so the trees are summed rather than averaged, then shrunk by `nu`
# and offset. Summing them plainly, as a caller might assume, would be wrong on
# both counts.
#' @export
tidypredict_combine_trees.blackboost <- function(x, trees, ...) {
  rlang::check_dots_empty()
  check_trees_arg(trees)

  comps <- mboost_components(x)
  mboost_combine(trees, comps$nu, comps$offset)
}

#' @exportS3Method
build_tree_formula.pm_tree_blackboost <- function(model) {
  mboost_build_formula(
    model$tree_info_list,
    model$general$nu,
    model$general$offset
  )
}
