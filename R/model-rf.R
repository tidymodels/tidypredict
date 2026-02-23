# Model parser -------------------------------------

#' @export
parse_model.randomForest <- function(model) {
  # Check if this is a classification model
  if (!is.null(model$classes)) {
    cli::cli_abort(
      c(
        "Classification models are not supported for randomForest.",
        i = "Only regression models can be converted to tidy formulas.",
        i = "Classification requires a voting mechanism that cannot be expressed as a single formula."
      )
    )
  }

  pm <- list()
  pm$general$model <- "randomForest"
  pm$general$type <- "tree"
  pm$general$version <- 3
  term_labels <- names(model$forest$ncat)
  pm$tree_info_list <- map(
    seq_len(model$ntree),
    function(tree_no) rf_tree_info_full(model, tree_no, term_labels)
  )
  as_parsed_model(pm)
}

# Convert randomForest getTree to standard tree_info format
rf_tree_info_full <- function(model, tree_no, term_labels) {
  tree <- randomForest::getTree(model, tree_no)
  n_nodes <- nrow(tree)

  # randomForest uses 1-indexed nodes, convert to 0-indexed
  # Also convert child IDs to 0-indexed (or NA for leaves)
  left_child <- tree[, "left daughter"]
  right_child <- tree[, "right daughter"]
  left_child <- ifelse(left_child == 0, NA_integer_, left_child - 1L)
  right_child <- ifelse(right_child == 0, NA_integer_, right_child - 1L)

  terminal <- tree[, "status"] == -1
  prediction <- ifelse(terminal, tree[, "prediction"], NA_real_)

  # Build split var names
  split_var_idx <- tree[, "split var"]
  splitvarName <- ifelse(
    split_var_idx == 0,
    NA_character_,
    term_labels[split_var_idx]
  )

  # Build node_splits list
  node_splits <- vector("list", n_nodes)
  for (i in seq_len(n_nodes)) {
    if (!terminal[i]) {
      node_splits[[i]] <- list(
        primary = list(
          col = splitvarName[i],
          val = tree[i, "split point"],
          is_categorical = FALSE
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

# Fit model (nested) -----------------------------------------------

#' @export
tidypredict_fit.randomForest <- function(model, ...) {
  tidypredict_fit_rf_nested(model)
}

# Nested formula builder for randomForest
tidypredict_fit_rf_nested <- function(model) {
  # Check if this is a classification model
  if (!is.null(model$classes)) {
    cli::cli_abort(
      c(
        "Classification models are not supported for randomForest.",
        i = "Only regression models can be converted to tidy formulas.",
        i = "Classification requires a voting mechanism that cannot be expressed as a single formula."
      )
    )
  }

  n_trees <- model$ntree
  term_labels <- names(model$forest$ncat)

  tree_exprs <- map(seq_len(n_trees), function(tree_no) {
    build_nested_rf_tree(model, tree_no, term_labels)
  })

  res <- reduce_addition(tree_exprs)
  expr_division(res, n_trees)
}

# Build nested case_when for a single randomForest tree
build_nested_rf_tree <- function(model, tree_no, term_labels) {
  tree <- randomForest::getTree(model, tree_no)

  # Pre-extract columns as vectors for fast indexing (avoids slow row access)
  # Use unname() once here instead of on every recursive call
  status <- unname(tree[, "status"])
  prediction <- unname(tree[, "prediction"])
  left_daughter <- unname(tree[, "left daughter"])
  right_daughter <- unname(tree[, "right daughter"])
  split_var <- unname(tree[, "split var"])
  split_point <- unname(tree[, "split point"])

  build_node <- function(node_id) {
    # Check if terminal (leaf) node - status == -1
    if (status[node_id] == -1) {
      return(prediction[node_id])
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
    col_sym <- rlang::sym(col_name)

    # Numeric split: left = <= splitval, right = > splitval
    condition <- expr(!!col_sym <= !!split_val)

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
    cli::cli_abort(
      c(
        "Classification models are not supported for randomForest.",
        i = "Only regression models can be converted to tidy formulas.",
        i = "Classification requires a voting mechanism that cannot be expressed as a single formula."
      )
    )
  }

  res <- generate_case_when_trees(parsedmodel)
  res <- reduce_addition(res)
  n_trees <- length(parsedmodel$trees)
  expr_division(res, n_trees)
}

# Legacy tree extraction functions (no longer used) ---------------------------
# These functions were used by the old parse_model.randomForest() to populate
# pm$trees in the flat path format. Now parse_model.randomForest() uses
# rf_tree_info_full() to populate pm$tree_info_list instead. Kept for reference.

parse_rf_path <- function(row_id, tree, columns, default_op = TRUE) {
  find <- row_id
  path <- row_id
  for (j in row_id:1) {
    dir <- NULL
    if (tree[j, "left daughter"] == find || tree[j, "right daughter"] == find) {
      find <- j
      path <- c(path, j)
    }
  }
  purrr::map2(
    path[1:length(path) - 1],
    path[2:length(path)],
    ~ {
      rb <- tree[.y, ]
      if (default_op) {
        if (rb["left daughter"] == .x) {
          op <- "less"
        }
        if (rb["right daughter"] == .x) {
          op <- "more-equal"
        }
      } else {
        if (rb["left daughter"] == .x) {
          op <- "less-equal"
        }
        if (rb["right daughter"] == .x) {
          op <- "more"
        }
      }
      list(
        type = "conditional",
        col = columns[rb["split var"]],
        val = rb["split point"][[1]],
        op = op
      )
    }
  )
}

parse_rf_tree <- function(tree_no, model) {
  predictions <- model$classes
  term_labels <- names(model$forest$ncat)
  tree <- randomForest::getTree(model, tree_no)
  paths <- seq_len(nrow(tree))[tree[, "status"] == -1]
  purrr::map(
    paths,
    ~ {
      if (is.null(predictions)) {
        # Classification
        prediction <- tree[.x, "prediction"]
      } else {
        # Regression
        prediction <- predictions[tree[.x, "prediction"]]
      }
      list(
        prediction = prediction,
        path = parse_rf_path(.x, tree, term_labels, default_op = FALSE)
      )
    }
  )
}

get_rf_trees <- function(model) {
  purrr::map(
    seq_len(model$ntree),
    ~ parse_rf_tree(.x, model)
  )
}

# For {orbital} -----------------------------------------------

#' Extract classification vote trees for randomForest models
#'
#' For use in orbital package.
#' @param model A randomForest model object
#' @keywords internal
#' @export
.extract_rf_classprob <- function(model) {
  if (!inherits(model, "randomForest")) {
    cli::cli_abort(
      "{.arg model} must be {.cls randomForest}, not {.obj_type_friendly {model}}."
    )
  }

  # Check if this is a classification model
  if (is.null(model$classes)) {
    cli::cli_abort(
      c(
        "Model is not a classification model.",
        i = "Use {.fn tidypredict_fit} for regression models."
      )
    )
  }

  # Get class levels from the model
  lvls <- model$classes
  term_labels <- names(model$forest$ncat)

  # For each class, generate nested case_when expressions for all trees
  res <- list()
  for (lvl in lvls) {
    tree_exprs <- map(seq_len(model$ntree), function(tree_no) {
      build_nested_rf_vote_tree(model, tree_no, term_labels, lvl)
    })
    res[[lvl]] <- tree_exprs
  }
  res
}

# Build nested case_when for randomForest voting tree
build_nested_rf_vote_tree <- function(
  model,
  tree_no,
  term_labels,
  class_level
) {
  tree <- randomForest::getTree(model, tree_no)
  classes <- model$classes

  # Pre-extract columns as vectors for fast indexing (avoids slow row access)
  # Use unname() once here instead of on every recursive call
  status <- unname(tree[, "status"])
  prediction <- unname(tree[, "prediction"])
  left_daughter <- unname(tree[, "left daughter"])
  right_daughter <- unname(tree[, "right daughter"])
  split_var <- unname(tree[, "split var"])
  split_point <- unname(tree[, "split point"])

  build_node <- function(node_id) {
    # Check if terminal (leaf) node - status == -1
    if (status[node_id] == -1) {
      # Return 1 if prediction matches class_level, 0 otherwise
      pred_class <- classes[prediction[node_id]]
      return(if (pred_class == class_level) 1L else 0L)
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
    col_sym <- rlang::sym(col_name)

    # Numeric split: left = <= splitval, right = > splitval
    condition <- expr(!!col_sym <= !!split_val)

    expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
  }

  build_node(1L)
}
