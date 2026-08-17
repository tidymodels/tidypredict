# Model parser -------------------------------------

# Only a regression forest has a single numeric value per leaf to build a
# formula from.
#
# Read from `treetype` rather than from a leaf's prediction: a probability or
# survival forest has no `prediction` column in `treeInfo()` at all, so
# `first_tree$prediction[...]` is `NULL`, `is.character(NULL)` is `FALSE`, and a
# check on the leaf value lets both straight through.
#
# A parsnip fit stores the ranger object one level down, and `treetype` is
# recorded on the forest as well as on the model, so take whichever is there.
ranger_check_supported <- function(model, call = rlang::caller_env()) {
  tree_type <- model$treetype %||% model$forest$treetype

  if (identical(tree_type, "Classification")) {
    abort_classification_unsupported("ranger", call = call)
  }
  if (identical(tree_type, "Probability estimation")) {
    cli::cli_abort(
      c(
        "Probability forests are not supported for ranger.",
        i = "A forest fit with {.code probability = TRUE} predicts one
        probability per class, which cannot be written as a single formula.",
        i = "Only regression models can be converted to tidy formulas."
      ),
      call = call
    )
  }
  if (identical(tree_type, "Survival")) {
    cli::cli_abort(
      c(
        "Survival forests are not supported for ranger.",
        i = "A survival forest predicts a curve over time rather than a single
        value, which cannot be written as a single formula.",
        i = "Only regression models can be converted to tidy formulas."
      ),
      call = call
    )
  }

  invisible(model)
}

#' @export
parse_model.ranger <- function(model) {
  ranger_check_supported(model)

  pm <- list()
  pm$general$model <- "ranger"
  pm$general$type <- "tree"
  pm$general$version <- 3
  pm$tree_info_list <- map(
    seq_len(model$num.trees),
    function(tree_no) ranger_tree_info_full(model, tree_no)
  )
  as_parsed_model(pm)
}

# The levels of each factor predictor, and `NULL` for a numeric one, paired
# with whether `ranger` treated the predictor as ordered.
#
# Under `respect.unordered.factors = "order"` the levels are not in the
# factor's own order: `ranger` sorts them by mean response and splits on a
# position in that sorted sequence, so the stored order is the one to use.
ranger_predictor_levels <- function(model) {
  levels <- model$forest$covariate.levels
  if (is.null(levels)) {
    return(list(levels = list(), is_ordered = list()))
  }

  vars <- model$forest$independent.variable.names %||% names(levels)
  is_ordered <- as.list(model$forest$is.ordered[seq_along(vars)])
  names(is_ordered) <- vars

  list(levels = levels, is_ordered = is_ordered)
}

# `ranger` writes a split on a factor predictor in one of two forms, neither of
# which is a threshold on the column itself.
#
# An ordered predictor, which covers `respect.unordered.factors = "ignore"` and
# `"order"` as well as genuinely ordered factors, gets a numeric split point
# naming a position in the stored level sequence: everything up to that
# position goes left.
#
# An unordered predictor, which is `"partition"`, gets a comma-separated list
# of level indices that go *right*. Left is the complement. Reading the list as
# the left set instead is wrong but plausible, and produces a small enough
# error to look like a rounding problem.
ranger_split_info <- function(col, split_val, levels, is_ordered) {
  if (is.null(levels)) {
    return(list(col = col, val = as.numeric(split_val), is_categorical = FALSE))
  }

  if (isTRUE(is_ordered)) {
    left <- levels[seq_len(floor(as.numeric(split_val)))]
  } else {
    right_idx <- as.integer(strsplit(as.character(split_val), ",")[[1]])
    left <- levels[-right_idx]
    return(list(
      col = col,
      vals = as.list(left),
      is_categorical = TRUE,
      missing_level = levels[[1]]
    ))
  }

  list(col = col, vals = as.list(left), is_categorical = TRUE)
}

# `ranger` compares an ordered or numeric split as `value > splitval`, and a
# missing value fails that test, so it takes the same branch as a value at or
# below the split point.
#
# An unordered split instead indexes a bitmask by the level's position.
# `ranger` derives that position from the raw value, and a missing value
# collapses to the position of the first level, so the row goes wherever the
# first level goes. That is not the same as always going one way: it depends on
# which side the first level is on at that node.
ranger_split_condition <- function(split) {
  condition <- build_nested_split_condition(split)

  if (!is.null(split$missing_level)) {
    if (split$missing_level %in% unlist(split$vals)) {
      return(expr(!!build_nested_split_missing(split) | !!condition))
    }
    return(condition)
  }

  expr(!!build_nested_split_missing(split) | !!condition)
}

# Convert ranger treeInfo to standard tree_info format
ranger_tree_info_full <- function(model, tree_no) {
  tree <- ranger::treeInfo(model, tree_no)
  info <- ranger_predictor_levels(model)

  # Build node_splits list
  node_splits <- vector("list", nrow(tree))
  for (i in seq_len(nrow(tree))) {
    if (!tree$terminal[i]) {
      var_name <- as.character(tree$splitvarName[i])

      node_splits[[i]] <- list(
        primary = ranger_split_info(
          var_name,
          tree$splitval[i],
          info$levels[[var_name]],
          info$is_ordered[[var_name]]
        )
      )
    }
  }

  list(
    nodeID = tree$nodeID,
    leftChild = tree$leftChild,
    rightChild = tree$rightChild,
    splitvarName = as.character(tree$splitvarName),
    terminal = tree$terminal,
    prediction = tree$prediction,
    node_splits = node_splits
  )
}

# Fit model (nested) -----------------------------------

#' @export
tidypredict_fit.ranger <- function(model, ...) {
  tidypredict_fit_ranger_nested(model)
}

# Nested formula builder for ranger
tidypredict_fit_ranger_nested <- function(model) {
  ranger_check_supported(model)

  n_trees <- model$num.trees
  tree_exprs <- map(seq_len(n_trees), function(tree_no) {
    build_nested_ranger_tree(model, tree_no)
  })

  # A forest of stumps mentions no column, so anchor it to one. These are the
  # predictors `ranger:::predict.ranger()` itself requires in `newdata`.
  expr_recycle_over_column(
    expr_mean(tree_exprs, n_trees),
    model$forest$independent.variable.names
  )
}

# Build nested case_when for a single ranger tree
#
# `leaf_col` is the `treeInfo()` column holding the value a leaf contributes.
# Regression trees use `prediction`; probability forests have one `pred.<class>`
# column per class.
build_nested_ranger_tree <- function(model, tree_no, leaf_col = "prediction") {
  tree <- ranger::treeInfo(model, tree_no)

  # Pre-extract columns as vectors for fast indexing (avoids slow df[i,] access)
  leftChild <- tree$leftChild
  rightChild <- tree$rightChild
  splitvarName <- as.character(tree$splitvarName)
  splitval <- tree$splitval
  terminal <- tree$terminal
  prediction <- tree[[leaf_col]]
  info <- ranger_predictor_levels(model)

  build_node <- function(node_id) {
    # node_id is 0-indexed, convert to 1-indexed for vector access
    idx <- node_id + 1L

    if (terminal[idx]) {
      return(prediction[idx])
    }

    left_id <- leftChild[idx]
    right_id <- rightChild[idx]
    split_var <- splitvarName[idx]
    split_val <- splitval[idx]

    left_subtree <- build_node(left_id)
    right_subtree <- build_node(right_id)

    split <- ranger_split_info(
      split_var,
      split_val,
      info$levels[[split_var]],
      info$is_ordered[[split_var]]
    )
    condition <- ranger_split_condition(split)

    expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
  }

  build_node(0L)
}

# Legacy flat case_when (for v1/v2 parsed model compatibility) ----------------
# These functions are preserved for backwards compatibility when loading
# parsed models saved with version < 3.

# Used by tidypredict_fit.pm_tree() for v1/v2 ranger parsed models
tidypredict_fit_ranger <- function(parsedmodel) {
  # Check if this is a classification model (string predictions). A v1/v2 model
  # saved from a probability or survival forest recorded no leaf value at all.
  first_pred <- parsedmodel$trees[[1]][[1]]$prediction
  if (is.character(first_pred) || is.null(first_pred)) {
    abort_classification_unsupported("ranger")
  }

  expr_mean(generate_case_when_trees(parsedmodel))
}

# Legacy tree extraction functions (no longer used) ---------------------------
# These functions were used by the old parse_model.ranger() to populate pm$trees
# in the flat path format. Now parse_model.ranger() uses ranger_tree_info_full()
# to populate pm$tree_info_list instead. Kept for reference.

get_child_info <- function(tree) {
  child_info <- numeric(max(tree$nodeID))
  left_child <- tree$leftChild
  right_child <- tree$rightChild
  node_id <- tree$nodeID

  for (i in seq_len(nrow(tree))) {
    node <- node_id[[i]]

    child <- left_child[[i]]
    if (!is.na(child)) {
      child_info[child] <- node
    }

    child <- right_child[[i]]
    if (!is.na(child)) {
      child_info[child] <- node
    }
  }

  child_info
}

get_ra_path <- function(node_id, tree, child_info, default_op = TRUE) {
  find <- node_id
  path <- node_id

  leftChild <- tree$leftChild
  rightChild <- tree$rightChild

  splitval <- tree$splitval
  splitvarName <- tree$splitvarName

  # Handle stump trees (no splits) - return empty path
  if (length(child_info) == 0 || find < 1 || find > length(child_info)) {
    return(list())
  }

  new <- child_info[[find]]
  path <- find
  repeat {
    if (new == 0) {
      path <- c(path, 0)
      break
    }
    path <- c(path, new)
    find <- new
    new <- child_info[[find]]
  }

  map2(
    path[1:length(path) - 1],
    path[2:length(path)],
    ~ {
      lc <- leftChild[.y + 1] == .x
      lr <- rightChild[.y + 1] == .x
      if (default_op) {
        if (lc) {
          op <- "less"
        }
        if (lr) {
          op <- "more-equal"
        }
      } else {
        if (lc) {
          op <- "less-equal"
        }
        if (lr) {
          op <- "more"
        }
      }
      list(
        type = "conditional",
        col = as.character(splitvarName[.y + 1]),
        val = splitval[.y + 1],
        op = op
      )
    }
  )
}

get_ra_tree <- function(tree_no, model) {
  tree <- ranger::treeInfo(model, tree_no)
  paths <- tree$nodeID[tree[, "terminal"]]

  child_info <- get_child_info(tree)

  map(
    paths,
    ~ {
      prediction <- tree$prediction[tree$nodeID == .x]
      if (!is.null(prediction)) {
        if (is.factor(prediction)) {
          prediction <- as.character(prediction)
        }
        list(
          prediction = prediction,
          path = get_ra_path(.x, tree, child_info, FALSE)
        )
      } else {
        preds <- map_lgl(colnames(tree), ~ "pred." == substr(.x, 1, 5))
        preds_table <- tree[tree$nodeID == .x, preds]
        predictors <- map_chr(colnames(preds_table), ~ substr(.x, 6, nchar(.x)))
        colnames(preds_table) <- predictors
        predictions <- map(preds_table, ~.x)
        max_pred <- map_lgl(predictions, ~ .x == max(map_dbl(predictions, ~.x)))
        prediction <- names(predictions)[max_pred]
        prediction <- prediction[[1]]
        prob <- predictions[max_pred]
        prob <- prob[[1]]
        predictions <- imap(predictions, ~ list(pred = .y, prob = .x))
        list(
          prediction = prediction,
          prob = prob,
          probs = predictions,
          path = get_ra_path(.x, tree, child_info, FALSE)
        )
      }
    }
  )
}

get_ra_trees <- function(model) {
  map(
    seq_len(model$num.trees),
    ~ get_ra_tree(.x, model)
  )
}

# Test ---------------------------------------------

# `predict.ranger()` returns a `ranger.prediction` list rather than a vector,
# so the default method cannot read the predictions out of it.
#' @export
tidypredict_test.ranger <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  df <- maybe_head(df, max_rows)

  base <- predict(model, df)$predictions
  te <- tidypredict_to_column(
    df,
    model,
    add_interval = FALSE,
    vars = c("fit_te", "upr_te", "lwr_te")
  )

  test_results_numeric(base, te[, "fit_te"], threshold, model$call)
}

# For {orbital} -----------------------------------------------

#' Extract classification probability trees for ranger models
#'
#' For use in orbital package.
#' @param model A ranger model object fitted with `probability = TRUE`
#' @keywords internal
#' @export
.extract_ranger_classprob <- function(model) {
  if (!inherits(model, "ranger")) {
    cli::cli_abort(
      "{.arg model} must be {.cls ranger}, not {.obj_type_friendly {model}}."
    )
  }

  # Get class levels from treeInfo
  tree <- ranger::treeInfo(model, 1)
  pred_cols <- grep("^pred\\.", names(tree), value = TRUE)

  if (length(pred_cols) == 0) {
    cli::cli_abort(
      c(
        "Model does not contain probability information.",
        i = "Fit the ranger model with {.code probability = TRUE}."
      )
    )
  }

  lvls <- sub("^pred\\.", "", pred_cols)

  # For each class, generate nested case_when expressions for all trees
  res <- list()
  for (lvl in lvls) {
    tree_exprs <- map(seq_len(model$num.trees), function(tree_no) {
      build_nested_ranger_prob_tree(model, tree_no, lvl)
    })
    res[[lvl]] <- tree_exprs
  }
  res
}

# Build nested case_when for ranger probability tree
build_nested_ranger_prob_tree <- function(model, tree_no, class_level) {
  build_nested_ranger_tree(model, tree_no, paste0("pred.", class_level))
}

#' Extract regression trees for ranger models
#'
#' For use in orbital package.
#' @param model A ranger model object (regression)
#' @keywords internal
#' @export
.extract_ranger_trees <- function(model) {
  if (!inherits(model, "ranger")) {
    cli::cli_abort(
      "{.arg model} must be {.cls ranger}, not {.obj_type_friendly {model}}."
    )
  }

  # Check if this is a classification model
  first_tree <- ranger::treeInfo(model, 1)
  first_pred <- first_tree$prediction[first_tree$terminal][1]
  if (is.character(first_pred) || is.factor(first_pred)) {
    cli::cli_abort(
      c(
        "Classification models are not supported.",
        i = "Use {.fn .extract_ranger_classprob} for classification models."
      )
    )
  }

  n_trees <- model$num.trees
  map(seq_len(n_trees), function(tree_no) {
    build_nested_ranger_tree(model, tree_no)
  })
}

build_tree_formula.pm_tree_ranger <- function(model) {
  expr_mean(map(
    model$tree_info_list,
    \(tree_info) generate_nested_case_when_tree(tree_info, missing = "left")
  ))
}
