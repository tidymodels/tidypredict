# Model parser -------------------------------------

#' @export
parse_model.randomForest <- function(model) {
  # Check if this is a classification model
  if (!is.null(model$classes)) {
    abort_classification_unsupported("randomForest")
  }

  pm <- list()
  pm$general$model <- "randomForest"
  pm$general$type <- "tree"
  pm$general$version <- 3
  term_labels <- names(model$forest$ncat)
  # Recorded so the parsed model can reproduce `predict()`'s refusal to score
  # an incomplete row, including for a predictor that no split happens to use.
  pm$general$predictors <- term_labels
  # `corr.bias = TRUE` stores the intercept and slope of a regression of the
  # observed values on the out-of-bag predictions, which `predict()` applies to
  # the forest average.
  pm$general$coefs <- unname(model$coefs)
  pm$tree_info_list <- map(
    seq_len(model$ntree),
    function(tree_no) rf_tree_info_full(model, tree_no, term_labels)
  )
  as_parsed_model(pm)
}

# The node table of a single tree.
#
# `randomForest::getTree()` subsets its assembled matrix without
# `drop = FALSE`, so a stump (a tree with a single root node and no split)
# collapses to a plain vector. `nrow()` on that is `NULL`, and the `1:nrow()`
# that follows aborts with "argument of length 0" before the tree is ever
# returned. A stump is common whenever the outcome is constant within the
# bootstrap sample, and `predict()` scores such a forest fine, so the table is
# assembled here for that case rather than letting the fit fail.
rf_get_tree <- function(model, tree_no) {
  n_nodes <- model$forest$ndbigtree[tree_no]
  if (n_nodes > 1) {
    return(randomForest::getTree(model, tree_no))
  }

  forest <- model$forest
  if (model$type == "regression") {
    daughters <- cbind(
      forest$leftDaughter[, tree_no],
      forest$rightDaughter[, tree_no]
    )
  } else {
    daughters <- forest$treemap[,, tree_no]
  }

  tree <- cbind(
    daughters,
    forest$bestvar[, tree_no],
    forest$xbestsplit[, tree_no],
    forest$nodestatus[, tree_no],
    forest$nodepred[, tree_no]
  )[seq_len(n_nodes), , drop = FALSE]

  dimnames(tree) <- list(
    seq_len(n_nodes),
    c(
      "left daughter",
      "right daughter",
      "split var",
      "split point",
      "status",
      "prediction"
    )
  )
  tree
}

# Convert randomForest getTree to standard tree_info format
rf_tree_info_full <- function(model, tree_no, term_labels) {
  tree <- rf_get_tree(model, tree_no)
  n_nodes <- nrow(tree)

  # randomForest uses 1-indexed nodes, convert to 0-indexed
  # Also convert child IDs to 0-indexed (or NA for leaves)
  # `getTree()` names its rows, and those names would otherwise ride along into
  # the generated leaf values.
  left_child <- unname(tree[, "left daughter"])
  right_child <- unname(tree[, "right daughter"])
  left_child <- ifelse(left_child == 0, NA_integer_, left_child - 1L)
  right_child <- ifelse(right_child == 0, NA_integer_, right_child - 1L)

  terminal <- unname(tree[, "status"]) == -1
  prediction <- ifelse(terminal, unname(tree[, "prediction"]), NA_real_)

  # Build split var names. Leaves report a split variable of 0, and indexing
  # `term_labels` with a 0 drops the element rather than yielding an NA, so the
  # subset has to be assigned by position: `ifelse()` over a shortened vector
  # silently misaligns every name after the first leaf.
  split_var_idx <- unname(tree[, "split var"])
  splitvarName <- rep(NA_character_, n_nodes)
  has_split <- split_var_idx != 0
  splitvarName[has_split] <- term_labels[split_var_idx[has_split]]

  levels <- rf_predictor_levels(model, term_labels)
  ncat <- model$forest$ncat

  # Build node_splits list
  node_splits <- vector("list", n_nodes)
  for (i in seq_len(n_nodes)) {
    if (!terminal[i]) {
      col <- splitvarName[i]
      node_splits[[i]] <- list(
        primary = rf_split_info(
          col,
          unname(tree[i, "split point"]),
          ncat[[col]],
          levels[[col]]
        )
      )
    }
  }

  list(
    nodeID = seq_len(n_nodes) - 1L,
    leftChild = left_child,
    rightChild = right_child,
    splitvarName = splitvarName,
    terminal = terminal,
    prediction = prediction,
    node_splits = node_splits
  )
}

# The levels of each factor predictor, and `NULL` for a numeric one.
#
# `randomForest` stores the levels of an unordered factor in `forest$xlevels`
# and writes a `0` there for everything else, so an ordered factor is only
# identifiable from the model's terms. Fits made through the `x`/`y` interface
# carry no terms, in which case an ordered factor is indistinguishable from a
# numeric predictor and is left alone.
rf_predictor_levels <- function(model, term_labels) {
  xlevels <- model$forest$xlevels
  classes <- attr(model$terms, "dataClasses")

  out <- lapply(term_labels, function(var) {
    lvls <- xlevels[[var]]
    if (is.character(lvls)) {
      return(lvls)
    }
    if (identical(unname(classes[var]), "ordered")) {
      return(NA_character_)
    }
    NULL
  })
  names(out) <- term_labels
  out
}

# `randomForest` encodes a split on a factor predictor in one of two ways, and
# neither is a threshold on the column itself.
#
# An unordered factor has `ncat > 1`, and the split point is an integer whose
# bits name the levels going left: a split point of 10 on levels a, b, c, d
# means `{b, d}` go left. An ordered factor has `ncat == 1` and an ordinary
# numeric split point, but it is compared against the level's integer code, so
# `<=` against the column itself returns `NA` rather than a branch.
#
# The unordered case is expressed as the set of levels going left, which the
# shared categorical split machinery already handles. The ordered case cannot
# be, because `randomForest` does not store the levels anywhere, so it stays a
# numeric comparison against the integer code.
rf_split_info <- function(col, split_point, ncat, levels) {
  if (is.null(levels)) {
    return(list(col = col, val = split_point, is_categorical = FALSE))
  }

  if (ncat == 1) {
    return(list(
      col = col,
      val = split_point,
      is_categorical = FALSE,
      as_integer = TRUE
    ))
  }

  bits <- as.integer(intToBits(split_point))[seq_along(levels)]
  list(
    col = col,
    vals = as.list(levels[bits == 1]),
    is_categorical = TRUE
  )
}

# Fit model (nested) -----------------------------------------------

#' @export
tidypredict_fit.randomForest <- function(model, ...) {
  tidypredict_fit_rf_nested(model)
}

# Nested formula builder for randomForest
tidypredict_fit_rf_nested <- function(model) {
  # Check if this is a classification model
  if (!is.null(model$classes)) {
    abort_classification_unsupported("randomForest")
  }

  n_trees <- model$ntree
  term_labels <- names(model$forest$ncat)

  tree_exprs <- map(seq_len(n_trees), function(tree_no) {
    build_nested_rf_tree(model, tree_no, term_labels)
  })

  # `randomForest::predict()` returns `NA` for a row with any missing
  # predictor, so the forest average is only defined on complete rows.
  expr_na_if_incomplete(
    rf_correct_bias(expr_mean(tree_exprs, n_trees), unname(model$coefs)),
    term_labels
  )
}

# `randomForest(corr.bias = TRUE)` fits `observed ~ predicted` on the
# out-of-bag predictions and keeps the two coefficients in `model$coefs`.
# `predict.randomForest()` then returns `coefs[1] + coefs[2] * yhat` for every
# complete row, so the correction is an affine rescale of the forest average.
rf_correct_bias <- function(f, coefs) {
  if (is.null(coefs)) {
    return(f)
  }
  expr(!!coefs[[1]] + !!coefs[[2]] * (!!f))
}

# Build nested case_when for a single randomForest tree
#
# `leaf_value` turns the tree's own prediction into the value the leaf should
# contribute, which is the only way a regression tree and a voting tree differ.
build_nested_rf_tree <- function(
  model,
  tree_no,
  term_labels,
  leaf_value = identity
) {
  tree <- rf_get_tree(model, tree_no)

  # Pre-extract columns as vectors for fast indexing (avoids slow row access)
  # Use unname() once here instead of on every recursive call
  status <- unname(tree[, "status"])
  prediction <- unname(tree[, "prediction"])
  left_daughter <- unname(tree[, "left daughter"])
  right_daughter <- unname(tree[, "right daughter"])
  split_var <- unname(tree[, "split var"])
  split_point <- unname(tree[, "split point"])

  levels <- rf_predictor_levels(model, term_labels)
  ncat <- model$forest$ncat

  build_node <- function(node_id) {
    # Check if terminal (leaf) node - status == -1
    if (status[node_id] == -1) {
      return(leaf_value(prediction[node_id]))
    }

    # Internal node - get split info
    left_id <- left_daughter[node_id]
    right_id <- right_daughter[node_id]
    var_idx <- split_var[node_id]
    split_val <- split_point[node_id]

    # Recurse
    left_subtree <- build_node(left_id)
    right_subtree <- build_node(right_id)

    col_name <- term_labels[var_idx]

    condition <- build_nested_split_condition(rf_split_info(
      col_name,
      split_val,
      ncat[[col_name]],
      levels[[col_name]]
    ))

    expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
  }

  build_node(1L)
}

# Legacy flat case_when (for v1/v2 parsed model compatibility) ----------------
# These functions are preserved for backwards compatibility when loading
# parsed models saved with version < 3.

# Used by tidypredict_fit.pm_tree() for v1/v2 randomForest parsed models
tidypredict_fit_randomForest <- function(parsedmodel) {
  # Check if this is a classification model (string predictions)
  first_pred <- parsedmodel$trees[[1]][[1]]$prediction
  if (is.character(first_pred)) {
    abort_classification_unsupported("randomForest")
  }

  expr_mean(generate_case_when_trees(parsedmodel))
}

# Extractors --------------------------------------------------

#' @export
tidypredict_class_trees.randomForest <- function(x, ...) {
  rlang::check_dots_empty()

  if (is.null(x$classes)) {
    cli::cli_abort(
      c(
        "Model is not a classification model.",
        i = "Use {.fn tidypredict_trees} for regression models."
      )
    )
  }

  lvls <- x$classes
  term_labels <- names(x$forest$ncat)

  # For each class, generate nested case_when expressions for all trees
  res <- list()
  for (lvl in lvls) {
    tree_exprs <- map(seq_len(x$ntree), function(tree_no) {
      build_nested_rf_vote_tree(x, tree_no, term_labels, lvl)
    })
    res[[lvl]] <- tree_exprs
  }
  res
}

#' @export
tidypredict_n_trees.randomForest <- function(x, ...) {
  rlang::check_dots_empty()

  # randomForest stores this as a double.
  as.integer(x$ntree)
}

# Build nested case_when for randomForest voting tree
build_nested_rf_vote_tree <- function(
  model,
  tree_no,
  term_labels,
  class_level
) {
  classes <- model$classes

  build_nested_rf_tree(
    model,
    tree_no,
    term_labels,
    # Vote 1 if the leaf predicts this class, 0 otherwise
    leaf_value = function(prediction) {
      if (classes[prediction] == class_level) 1L else 0L
    }
  )
}

#' @export
tidypredict_trees.randomForest <- function(x, ...) {
  rlang::check_dots_empty()

  if (!is.null(x$classes)) {
    cli::cli_abort(
      c(
        "Classification models are not supported.",
        i = "Use {.fn tidypredict_class_trees} for classification models."
      )
    )
  }

  # The bias correction applies to the forest average, so it cannot be carried
  # by the individual tree expressions returned here.
  if (!is.null(x$coefs)) {
    cli::cli_abort(
      "Models fitted with {.code corr.bias = TRUE} are not supported."
    )
  }

  term_labels <- names(x$forest$ncat)

  map(seq_len(x$ntree), function(tree_no) {
    build_nested_rf_tree(x, tree_no, term_labels)
  })
}

build_tree_formula.pm_tree_randomForest <- function(model) {
  expr_na_if_incomplete(
    rf_correct_bias(build_tree_formula_forest(model), model$general$coefs),
    model$general$predictors
  )
}
