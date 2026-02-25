# Model parser -------------------------------------

#' @export
parse_model.ranger <- function(model) {
  # Check if this is a classification model
  first_tree <- ranger::treeInfo(model, 1)
  first_pred <- first_tree$prediction[first_tree$terminal][1]
  if (is.character(first_pred) || is.factor(first_pred)) {
    cli::cli_abort(
      c(
        "Classification models are not supported for ranger.",
        i = "Only regression models can be converted to tidy formulas.",
        i = "Classification requires a voting mechanism that cannot be expressed as a single formula."
      )
    )
  }

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

# Convert ranger treeInfo to standard tree_info format
ranger_tree_info_full <- function(model, tree_no) {
  tree <- ranger::treeInfo(model, tree_no)

  # Build node_splits list
  node_splits <- vector("list", nrow(tree))
  for (i in seq_len(nrow(tree))) {
    if (!tree$terminal[i]) {
      var_name <- as.character(tree$splitvarName[i])
      split_val <- tree$splitval[i]

      if (is.na(split_val)) {
        # Categorical split
        # Unreachable with ranger >= 0.14.1: factor splits use numeric indices
        split_class <- tree$splitclass[i]
        cats <- strsplit(as.character(split_class), ", ")[[1]]
        node_splits[[i]] <- list(
          primary = list(
            col = var_name,
            vals = as.list(cats),
            is_categorical = TRUE
          )
        )
      } else {
        # Numeric split
        node_splits[[i]] <- list(
          primary = list(
            col = var_name,
            val = split_val,
            is_categorical = FALSE
          )
        )
      }
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
  # Check if this is a classification model
  first_tree <- ranger::treeInfo(model, 1)
  first_pred <- first_tree$prediction[first_tree$terminal][1]
  if (is.character(first_pred) || is.factor(first_pred)) {
    cli::cli_abort(
      c(
        "Classification models are not supported for ranger.",
        i = "Only regression models can be converted to tidy formulas.",
        i = "Classification requires a voting mechanism that cannot be expressed as a single formula."
      )
    )
  }

  n_trees <- model$num.trees
  tree_exprs <- map(seq_len(n_trees), function(tree_no) {
    build_nested_ranger_tree(model, tree_no)
  })

  res <- reduce_addition(tree_exprs)
  expr_division(res, n_trees)
}

# Build nested case_when for a single ranger tree
build_nested_ranger_tree <- function(model, tree_no) {
  tree <- ranger::treeInfo(model, tree_no)

  # Pre-extract columns as vectors for fast indexing (avoids slow df[i,] access)
  nodeID <- tree$nodeID
  leftChild <- tree$leftChild
  rightChild <- tree$rightChild
  splitvarName <- as.character(tree$splitvarName)
  splitval <- tree$splitval
  terminal <- tree$terminal
  prediction <- tree$prediction
  splitclass <- tree$splitclass

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

    col_sym <- rlang::sym(split_var)

    if (is.na(split_val)) {
      # Unreachable with ranger >= 0.14.1: factor splits use numeric indices
      cats <- strsplit(as.character(splitclass[idx]), ", ")[[1]]
      condition <- expr(!!col_sym %in% !!cats)
    } else {
      condition <- expr(!!col_sym <= !!split_val)
    }

    expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
  }

  build_node(0L)
}

# Legacy flat case_when (for v1/v2 parsed model compatibility) ----------------
# These functions are preserved for backwards compatibility when loading
# parsed models saved with version < 3.

# Used by tidypredict_fit.pm_tree() for v1/v2 ranger parsed models
tidypredict_fit_ranger <- function(parsedmodel) {
  # Check if this is a classification model (string predictions)
  first_pred <- parsedmodel$trees[[1]][[1]]$prediction
  if (is.character(first_pred)) {
    cli::cli_abort(
      c(
        "Classification models are not supported for ranger.",
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
  splitclass <- tree$splitclass
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
      if (is.na(splitval[.y + 1])) {
        # Unreachable with ranger >= 0.14.1: factor splits use numeric indices
        if (lc) {
          op <- "in"
        }
        if (lr) {
          op <- "not-in"
        }
        vals <- strsplit(as.character(splitclass[.y + 1]), ", ")[[1]]
        list(
          type = "set",
          col = as.character(splitvarName[.y + 1]),
          vals = map(vals, ~.x),
          op = op
        )
      } else {
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
  tree <- ranger::treeInfo(model, tree_no)

  # Pre-extract columns as vectors for fast indexing (avoids slow df[i,] access)
  nodeID <- tree$nodeID
  leftChild <- tree$leftChild
  rightChild <- tree$rightChild
  splitvarName <- as.character(tree$splitvarName)
  splitval <- tree$splitval
  terminal <- tree$terminal
  splitclass <- tree$splitclass
  prob_col <- paste0("pred.", class_level)
  prob_vals <- tree[[prob_col]]

  build_node <- function(node_id) {
    # node_id is 0-indexed, convert to 1-indexed for vector access
    idx <- node_id + 1L

    if (terminal[idx]) {
      return(prob_vals[idx])
    }

    left_id <- leftChild[idx]
    right_id <- rightChild[idx]
    split_var <- splitvarName[idx]
    split_val <- splitval[idx]

    left_subtree <- build_node(left_id)
    right_subtree <- build_node(right_id)

    col_sym <- rlang::sym(split_var)

    if (is.na(split_val)) {
      # Unreachable with ranger >= 0.14.1: factor splits use numeric indices
      cats <- strsplit(as.character(splitclass[idx]), ", ")[[1]]
      condition <- expr(!!col_sym %in% !!cats)
    } else {
      condition <- expr(!!col_sym <= !!split_val)
    }

    expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
  }

  build_node(0L)
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
