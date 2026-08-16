# aorsf fits oblique random forests where each split is a linear combination of
# (standardized) predictors rather than a single variable. Regression prediction
# is the mean across trees of each tree's terminal-node in-bag mean, so it can be
# expressed as the mean of the individual tree expressions. The per-forest
# standardization (using the stored means and standard deviations) is folded into
# the split coefficients so the generated formula operates on the raw predictor
# columns.
#
# The split thresholds are then calibrated against the training data, because the
# arithmetic cannot be matched exactly. aorsf accumulates its linear combination
# in compiled code, `out += x * beta` term by term, which the compiler contracts
# into a fused multiply-add: the products are never rounded to a double. No R or
# SQL expression can do that, so the two values differ in the last bit. That is
# ordinarily harmless, but aorsf's cutpoints are *observed* linear-combination
# values, so every training row lands exactly on one of them and a last-bit
# difference sends it down the other branch. Rather than chase the arithmetic,
# `aorsf_calibration()` asks aorsf which branch it takes for each training row and
# moves the threshold into the gap between the two branches. See #351.

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
aorsf_tree_info_full <- function(model, tree_no, calibration = NULL) {
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

  node_splits <- aorsf_calibrate_tree(
    node_splits,
    child_left,
    calibration,
    tree_no
  )

  list(
    nodeID = seq_len(n) - 1L,
    leftChild = ifelse(terminal, NA_integer_, child_left),
    rightChild = ifelse(terminal, NA_integer_, child_left + 1L),
    terminal = terminal,
    prediction = leaf_summary,
    node_splits = node_splits
  )
}

# Threshold calibration ----------------------------

# The training rows, together with the terminal node aorsf itself assigns to each
# of them in each tree. `NULL` when that cannot be obtained, in which case the
# algebraic thresholds are kept.
aorsf_calibration <- function(model) {
  data <- model$data
  names_x <- model$get_names_x()
  if (is.null(data)) {
    return(NULL)
  }
  complete <- stats::complete.cases(data[names_x])
  if (!any(complete)) {
    return(NULL)
  }
  data <- data[complete, , drop = FALSE]
  leaves <- try(
    predict(model, new_data = data, pred_type = "leaf"),
    silent = TRUE
  )
  if (inherits(leaves, "try-error")) {
    return(NULL)
  }
  list(x = data[names_x], leaves = leaves)
}

# Terminal node ids (0-based) reachable from each node.
aorsf_subtree_leaves <- function(child_left) {
  n <- length(child_left)
  out <- vector("list", n)
  # A child is always stored after its parent, so one pass in reverse suffices.
  for (i in rev(seq_len(n))) {
    if (child_left[i] == 0) {
      out[[i]] <- i - 1
    } else {
      out[[i]] <- c(out[[child_left[i] + 1L]], out[[child_left[i] + 2L]])
    }
  }
  out
}

aorsf_calibrate_tree <- function(
  node_splits,
  child_left,
  calibration,
  tree_no
) {
  if (is.null(calibration)) {
    return(node_splits)
  }
  x <- calibration$x
  leaf_ids <- calibration$leaves[, tree_no]
  subtree <- aorsf_subtree_leaves(child_left)

  rows_at <- vector("list", length(child_left))
  rows_at[[1]] <- seq_len(nrow(x))
  for (i in seq_along(child_left)) {
    rows <- rows_at[[i]]
    if (child_left[i] == 0 || length(rows) == 0) {
      next
    }
    split <- node_splits[[i]]$primary
    # Taken apart rather than rebuilt, so the value calibrated against is the one
    # the generated expression actually computes.
    lincomb <- build_nested_split_condition(split)[[2]]
    values <- rlang::eval_tidy(lincomb, x[rows, , drop = FALSE])
    goes_left <- leaf_ids[rows] %in% subtree[[child_left[i] + 1L]]
    node_splits[[i]]$primary$val <- aorsf_separating_threshold(
      values,
      goes_left,
      split$val
    )
    rows_at[[child_left[i] + 1L]] <- rows[goes_left]
    rows_at[[child_left[i] + 2L]] <- rows[!goes_left]
  }
  node_splits
}

# Any threshold in `[left, right)` reproduces aorsf's branch assignment for every
# training row seen at this node, where `left` is the largest value aorsf sends
# left and `right` the smallest it sends right. The algebraic threshold is kept
# whenever it already lies there, so it only ever moves by the last bit or two.
# When the two branches overlap, or one of them is empty, there is nothing to
# separate and the algebraic threshold is kept.
aorsf_separating_threshold <- function(values, goes_left, algebraic) {
  if (all(goes_left) || !any(goes_left)) {
    return(algebraic)
  }
  left <- max(values[goes_left])
  right <- min(values[!goes_left])
  if (left >= right) {
    return(algebraic)
  }
  if (algebraic >= left && algebraic < right) {
    return(algebraic)
  }
  left
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
  calibration <- aorsf_calibration(model)
  pm$tree_info_list <- map(
    seq_len(n_trees),
    function(tree_no) aorsf_tree_info_full(model, tree_no, calibration)
  )
  as_parsed_model(pm)
}

# Fit formula -----------------------------------

#' @export
tidypredict_fit.ObliqueForest <- function(model, ...) {
  aorsf_check_supported(model)

  n_trees <- length(model$forest$child_left)
  calibration <- aorsf_calibration(model)
  tree_exprs <- map(
    seq_len(n_trees),
    function(tree_no) {
      generate_nested_case_when_tree(
        aorsf_tree_info_full(model, tree_no, calibration)
      )
    }
  )

  # `aorsf` refuses to predict from an incomplete row ("Please remove missing
  # values from new data, or impute them."), so there is no value to match.
  expr_na_if_incomplete(expr_mean(tree_exprs, n_trees), model$get_names_x())
}

build_tree_formula.pm_tree_aorsf <- function(model) {
  expr_na_if_incomplete(
    build_tree_formula_forest(model),
    model$general$predictors
  )
}
