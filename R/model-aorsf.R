# aorsf fits oblique random forests where each split is a linear combination of
# (standardized) predictors rather than a single variable. Regression prediction
# is the mean across trees of each tree's terminal-node in-bag mean, so it can be
# expressed as the mean of the individual tree expressions. The per-forest
# standardization (using the stored means and standard deviations) is folded into
# the split coefficients so the generated formula operates on the raw predictor
# columns.

aorsf_check_supported <- function(model) {
  if (!inherits(model, "ObliqueForestRegression")) {
    abort_classification_unsupported("aorsf")
  }

  names_x <- model$get_names_x()
  classes <- vapply(
    model$data[names_x],
    function(x) class(x)[[1]],
    character(1)
  )
  if (!all(classes %in% c("numeric", "integer"))) {
    bad <- names_x[!classes %in% c("numeric", "integer")]
    cli::cli_abort(
      c(
        "Only numeric predictors are supported for aorsf.",
        i = "Oblique splits on non-numeric predictor(s) {.val {bad}} cannot be expressed as a single formula."
      )
    )
  }

  invisible(model)
}

# Convert an aorsf tree to the standard nested tree_info format. aorsf stores
# each tree as parallel arrays indexed by node: `child_left` holds the 0-indexed
# id of the left child (right child is `+ 1`), with `0` marking a leaf.
aorsf_tree_info_full <- function(model, tree_no) {
  forest <- model$forest
  child_left <- forest$child_left[[tree_no]]
  cutpoint <- forest$cutpoint[[tree_no]]
  coef_indices <- forest$coef_indices[[tree_no]]
  coef_values <- forest$coef_values[[tree_no]]
  leaf_summary <- forest$leaf_summary[[tree_no]]

  names_x <- model$get_names_x()
  means <- model$get_means()
  stdev <- model$get_stdev()

  n <- length(child_left)
  terminal <- child_left == 0

  node_splits <- vector("list", n)
  for (i in seq_len(n)) {
    if (!terminal[i]) {
      cols <- names_x[coef_indices[[i]] + 1L]
      cv <- coef_values[[i]]
      mu <- means[cols]
      sdv <- stdev[cols]
      # lincomb on standardized x is sum(cv * (x - mu) / sdv). Folding the
      # centering and scaling into the coefficients gives a linear expression in
      # the raw columns compared against an adjusted threshold.
      coefs <- as.numeric(cv / sdv)
      threshold <- cutpoint[i] + sum(cv * mu / sdv)
      node_splits[[i]] <- list(
        primary = list(
          is_oblique = TRUE,
          cols = cols,
          coefs = coefs,
          val = threshold
        )
      )
    }
  }

  list(
    nodeID = seq_len(n) - 1L,
    leftChild = ifelse(terminal, NA_integer_, child_left),
    rightChild = ifelse(terminal, NA_integer_, child_left + 1L),
    terminal = terminal,
    prediction = leaf_summary,
    node_splits = node_splits
  )
}

# Model parser -------------------------------------

#' @export
parse_model.ObliqueForest <- function(model) {
  aorsf_check_supported(model)

  pm <- list()
  pm$general$model <- "aorsf"
  pm$general$type <- "tree"
  pm$general$version <- 3
  n_trees <- length(model$forest$child_left)
  # Recorded so the parsed model can reproduce `predict()`'s refusal to score
  # an incomplete row.
  pm$general$predictors <- model$get_names_x()
  pm$tree_info_list <- map(
    seq_len(n_trees),
    function(tree_no) aorsf_tree_info_full(model, tree_no)
  )
  as_parsed_model(pm)
}

# Fit formula -----------------------------------

#' @export
tidypredict_fit.ObliqueForest <- function(model, ...) {
  aorsf_check_supported(model)

  tidypredict_combine_trees(model, tidypredict_trees(model))
}

# Extractors --------------------------------------

#' @export
tidypredict_trees.ObliqueForest <- function(x, ...) {
  rlang::check_dots_empty()
  aorsf_check_supported(x)

  map(
    seq_len(length(x$forest$child_left)),
    function(tree_no) {
      generate_nested_case_when_tree(aorsf_tree_info_full(x, tree_no))
    }
  )
}

#' @export
tidypredict_n_trees.ObliqueForest <- function(x, ...) {
  rlang::check_dots_empty()

  length(x$forest$child_left)
}

# The mean is only half of it. `aorsf` refuses to predict from an incomplete
# row ("Please remove missing values from new data, or impute them."), so the
# result is wrapped to return `NA` for such a row. A caller that averaged the
# trees itself would silently produce a number where the model produces
# nothing.
#' @export
tidypredict_combine_trees.ObliqueForest <- function(x, trees, ...) {
  rlang::check_dots_empty()
  check_trees_arg(trees)

  expr_na_if_incomplete(
    expr_mean(trees, length(trees)),
    x$get_names_x()
  )
}

build_tree_formula.pm_tree_aorsf <- function(model) {
  expr_na_if_incomplete(
    build_tree_formula_forest(model),
    model$general$predictors
  )
}
