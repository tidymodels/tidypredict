# Extract comprehensive tree info including surrogate splits
rpart_tree_info_full <- function(model) {
  frame <- model$frame
  splits <- model$splits
  orig_node_ids <- as.integer(rownames(frame))
  is_terminal <- frame$var == "<leaf>"

  # Check if surrogates are used during prediction
  # usesurrogate: 0 = don't use (NAs stop at internal nodes)
  #               1 = use surrogates, if all NA stop at internal node
  #               2 = use surrogates, if all NA go to majority (default)
  usesurr <- model$control$usesurrogate
  use_surrogates <- !identical(usesurr, 0L) && !identical(usesurr, 0)

  # Create mapping from original rpart node IDs to sequential 0-indexed IDs
  id_map <- setNames(seq_along(orig_node_ids) - 1L, orig_node_ids)

  # Build child relationships
  left_candidates <- 2L * orig_node_ids
  right_candidates <- 2L * orig_node_ids + 1L

  rpart_left <- ifelse(
    left_candidates %in% orig_node_ids,
    id_map[as.character(left_candidates)],
    NA_integer_
  )
  rpart_right <- ifelse(
    right_candidates %in% orig_node_ids,
    id_map[as.character(right_candidates)],
    NA_integer_
  )

  # Get predictions for ALL nodes (needed for usesurrogate=0 where NAs stop at internal nodes)
  if (model$method == "class") {
    ylevels <- attr(model, "ylevels")
    prediction <- ylevels[frame$yval]
  } else {
    prediction <- frame$yval
  }

  # Extract split info per node, including surrogates
  n_nodes <- nrow(frame)
  node_splits <- vector("list", n_nodes)
  needs_swap <- rep(FALSE, n_nodes)
  majority_left <- rep(NA, n_nodes)

  if (!is.null(splits) && nrow(splits) > 0) {
    split_idx <- 1
    for (i in seq_len(n_nodes)) {
      if (!is_terminal[i]) {
        n_compete <- frame$ncompete[i]
        n_surr <- frame$nsurrogate[i]

        # Primary split
        primary <- extract_one_split(
          splits,
          split_idx,
          model,
          as.character(frame$var[i])
        )
        needs_swap[i] <- primary$needs_swap

        # Surrogate splits (skip competing splits)
        # Only extract if usesurrogate != 0
        surrogates <- list()
        if (use_surrogates) {
          surr_start <- split_idx + 1 + n_compete
          for (j in seq_len(n_surr)) {
            surr_idx <- surr_start + j - 1
            surr_var <- rownames(splits)[surr_idx]
            surr_info <- extract_one_split(splits, surr_idx, model, surr_var)
            # Adjust surrogate direction based on primary direction
            if (primary$needs_swap) {
              surr_info$needs_swap <- !surr_info$needs_swap
            }
            surrogates[[j]] <- surr_info
          }
        }

        # Majority direction: left child has more observations
        left_id <- 2L * orig_node_ids[i]
        right_id <- 2L * orig_node_ids[i] + 1L
        left_n <- frame$n[orig_node_ids == left_id]
        right_n <- frame$n[orig_node_ids == right_id]
        majority_left[i] <- left_n >= right_n

        node_splits[[i]] <- list(
          primary = primary,
          surrogates = surrogates
        )

        n_splits <- 1 + n_compete + n_surr
        split_idx <- split_idx + n_splits
      }
    }
  }

  # Swap left/right based on ncat direction
  # This aligns with tidypredict convention: "left" means "< split value"
  left_child <- ifelse(needs_swap, rpart_right, rpart_left)
  right_child <- ifelse(needs_swap, rpart_left, rpart_right)

  # Also swap majority_left to stay consistent with the new left/right
  majority_left_adjusted <- ifelse(needs_swap, !majority_left, majority_left)

  list(
    nodeID = seq_along(orig_node_ids) - 1L,
    leftChild = left_child,
    rightChild = right_child,
    splitvarName = as.character(frame$var),
    terminal = is_terminal,
    prediction = prediction,
    node_splits = node_splits,
    majority_left = majority_left_adjusted,
    use_surrogates = use_surrogates
  )
}

# Extract info for a single split from the splits matrix
extract_one_split <- function(splits, idx, model, var_name) {
  ncat <- splits[idx, "ncat"]
  index <- splits[idx, "index"]

  if (abs(ncat) == 1) {
    # Continuous split
    list(
      col = var_name,
      val = index,
      is_categorical = FALSE,
      needs_swap = ncat == 1
    )
  } else {
    # Categorical split
    csplit_row <- model$csplit[index, , drop = TRUE]
    xlevels <- attr(model, "xlevels")[[var_name]]
    if (ncat > 0) {
      left_levels <- xlevels[csplit_row == 1]
    } else {
      left_levels <- xlevels[csplit_row == 3]
    }
    list(
      col = var_name,
      vals = as.list(left_levels),
      is_categorical = TRUE,
      needs_swap = FALSE
    )
  }
}

# Legacy function for backward compatibility and orbital helpers
rpart_tree_info <- function(model) {
  info <- rpart_tree_info_full(model)

  split_val <- rep(NA_real_, length(info$nodeID))
  split_class <- rep(NA_character_, length(info$nodeID))

  for (i in seq_along(info$node_splits)) {
    if (!is.null(info$node_splits[[i]])) {
      primary <- info$node_splits[[i]]$primary
      if (primary$is_categorical) {
        split_class[i] <- paste0(primary$vals, collapse = ", ")
      } else {
        split_val[i] <- primary$val
      }
    }
  }

  data.frame(
    nodeID = info$nodeID,
    leftChild = info$leftChild,
    rightChild = info$rightChild,
    splitvarName = info$splitvarName,
    splitval = split_val,
    splitclass = split_class,
    terminal = info$terminal,
    prediction = info$prediction
  )
}

get_rpart_tree <- function(model) {
  info <- rpart_tree_info_full(model)
  terminal_ids <- info$nodeID[info$terminal]
  internal_ids <- info$nodeID[!info$terminal]

  # Build parent mapping (use -1 for no parent since 0 is a valid node ID)
  child_info <- rep(-1L, max(info$nodeID) + 1)
  for (i in seq_along(info$nodeID)) {
    node <- info$nodeID[i]
    lc <- info$leftChild[i]
    rc <- info$rightChild[i]
    if (!is.na(lc)) {
      child_info[lc + 1] <- node
    }
    if (!is.na(rc)) child_info[rc + 1] <- node
  }

  # Generate paths for terminal nodes (leaves)
  leaf_paths <- map(terminal_ids, function(leaf_id) {
    prediction <- info$prediction[info$nodeID == leaf_id]
    if (is.null(prediction)) {
      cli::cli_abort("Prediction column not found.")
    }
    if (is.factor(prediction)) {
      prediction <- as.character(prediction)
    }
    list(
      prediction = prediction,
      path = get_rpart_path(leaf_id, info, child_info)
    )
  })

  # For usesurrogate=0, also generate paths for internal nodes where NAs stop
  if (!info$use_surrogates && length(internal_ids) > 0) {
    na_stop_paths <- map(internal_ids, function(node_id) {
      prediction <- info$prediction[info$nodeID == node_id]
      if (is.factor(prediction)) {
        prediction <- as.character(prediction)
      }
      # Get path to this node, plus the "is.na(split_var)" condition
      path <- get_rpart_path_na_stop(node_id, info, child_info)
      list(
        prediction = prediction,
        path = path
      )
    })
    # NA stop paths should come BEFORE leaf paths (more specific conditions first)
    c(na_stop_paths, leaf_paths)
  } else {
    leaf_paths
  }
}

# Build path for usesurrogate=0 internal node "NA stop"
# This generates: path_to_node AND is.na(split_var_at_node)
get_rpart_path_na_stop <- function(node_id, info, child_info) {
  # Get the path to reach this node (conditions to get here with non-NA values)
  path_to_node <- get_rpart_path_simple(node_id, info, child_info)

  # Add the "is.na(split_var)" condition for this node
  node_idx <- which(info$nodeID == node_id)
  split_info <- info$node_splits[[node_idx]]

  if (!is.null(split_info)) {
    primary <- split_info$primary
    na_cond <- list(
      type = "na_check",
      col = primary$col
    )
    c(path_to_node, list(na_cond))
  } else {
    path_to_node
  }
}

# Build simple path without surrogate handling (for usesurrogate=0)
# Each condition is just "!is.na(var) & var <op> val"
get_rpart_path_simple <- function(node_id, info, child_info) {
  # Trace path from node to root
  path_nodes <- node_id
  current <- node_id
  while (current >= 0 && (current + 1) <= length(child_info)) {
    parent <- child_info[current + 1]
    if (parent < 0) {
      break
    }
    path_nodes <- c(path_nodes, parent)
    current <- parent
  }

  if (length(path_nodes) <= 1) {
    return(list())
  }

  # Build conditions for each step (child -> parent)
  conditions <- list()
  for (i in seq_len(length(path_nodes) - 1)) {
    child_node <- path_nodes[i]
    parent_node <- path_nodes[i + 1]

    parent_idx <- which(info$nodeID == parent_node)
    is_left_child <- info$leftChild[parent_idx] == child_node

    split_info <- info$node_splits[[parent_idx]]
    if (is.null(split_info)) {
      next
    }

    primary <- split_info$primary
    # For usesurrogate=0 paths, we need: !is.na(var) & var <op> val
    cond <- build_simple_condition(primary, is_left_child)
    conditions <- c(conditions, list(cond))
  }

  rev(conditions)
}

# Build a simple condition with NOT NULL check (for usesurrogate=0)
build_simple_condition <- function(primary, go_left) {
  if (primary$is_categorical) {
    list(
      type = "set_not_na",
      col = primary$col,
      vals = primary$vals,
      op = if (go_left) "in" else "not-in"
    )
  } else {
    list(
      type = "conditional_not_na",
      col = primary$col,
      val = primary$val,
      op = if (go_left) "less-equal" else "more"
    )
  }
}

# Build path from leaf to root with surrogate information
get_rpart_path <- function(leaf_id, info, child_info) {
  # Trace path from leaf to root
  path_nodes <- leaf_id
  current <- leaf_id
  while (current >= 0 && (current + 1) <= length(child_info)) {
    parent <- child_info[current + 1]
    if (parent < 0) {
      break
    }
    path_nodes <- c(path_nodes, parent)
    current <- parent
  }

  if (length(path_nodes) <= 1) {
    return(list())
  }

  # Build conditions for each step (child -> parent)
  conditions <- list()
  for (i in seq_len(length(path_nodes) - 1)) {
    child_node <- path_nodes[i]
    parent_node <- path_nodes[i + 1]

    # Find parent's index
    parent_idx <- which(info$nodeID == parent_node)

    # Is child the left or right child of parent?
    is_left_child <- info$leftChild[parent_idx] == child_node

    # Get split info for this node
    split_info <- info$node_splits[[parent_idx]]
    if (is.null(split_info)) {
      next
    }

    primary <- split_info$primary
    surrogates <- split_info$surrogates
    majority_left <- info$majority_left[parent_idx]

    # Create condition with surrogates
    cond <- build_rpart_condition(
      primary,
      surrogates,
      is_left_child,
      majority_left
    )
    conditions <- c(conditions, list(cond))
  }

  rev(conditions)
}

# Build a single condition with surrogate fallbacks
build_rpart_condition <- function(primary, surrogates, go_left, majority_left) {
  # Always use surrogate-style condition to handle NAs correctly
  # Even without surrogates, NAs go to majority child
  primary_cond <- if (primary$is_categorical) {
    list(
      col = primary$col,
      vals = primary$vals,
      op = if (go_left) "in" else "not-in"
    )
  } else {
    list(
      col = primary$col,
      val = primary$val,
      op = if (go_left) "less-equal" else "more"
    )
  }

  surr_conds <- map(surrogates, function(s) {
    # Determine surrogate direction
    # If surrogate needs_swap is TRUE, it goes opposite of primary
    surr_go_left <- if (s$needs_swap) !go_left else go_left

    if (s$is_categorical) {
      list(
        col = s$col,
        vals = s$vals,
        op = if (surr_go_left) "in" else "not-in"
      )
    } else {
      list(
        col = s$col,
        val = s$val,
        op = if (surr_go_left) "less-equal" else "more"
      )
    }
  })

  # majority_match: does this direction match where majority goes?
  majority_match <- (go_left && majority_left) || (!go_left && !majority_left)

  type <- if (primary$is_categorical) {
    "set_with_surrogates"
  } else {
    "conditional_with_surrogates"
  }

  list(
    type = type,
    primary = primary_cond,
    surrogates = surr_conds,
    majority_match = majority_match
  )
}

#' @export
parse_model.rpart <- function(model) {
  pm <- list()
  pm$general$model <- "rpart"
  pm$general$type <- "tree"
  pm$general$version <- 2
  pm$trees <- list(get_rpart_tree(model))
  as_parsed_model(pm)
}

#' @export
tidypredict_fit.rpart <- function(model) {
  parsedmodel <- parse_model(model)
  tree <- parsedmodel$trees[[1]]
  mode <- parsedmodel$general$mode
  generate_case_when_tree(tree, mode)
}

#' @export
tidypredict_test.rpart <- function(
  model,
  df = model$model,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  if (is.numeric(max_rows)) {
    df <- head(df, max_rows)
  }

  # rpart uses "vector" for regression, "class" for classification
  pred_type <- if (model$method == "class") "class" else "vector"
  preds <- predict(model, df, type = pred_type)

  if (pred_type == "class") {
    preds <- as.character(preds)
  }

  base <- data.frame(fit = as.vector(preds), row.names = NULL)

  te <- tidypredict_to_column(
    df,
    model,
    add_interval = FALSE,
    vars = c("fit_te", "upr_te", "lwr_te")
  )
  te <- data.frame(fit_te = te[, "fit_te"])

  raw_results <- cbind(base, te)

  if (pred_type == "class") {
    raw_results$fit_diff <- raw_results$fit != raw_results$fit_te
    raw_results$fit_threshold <- raw_results$fit_diff
  } else {
    raw_results$fit_diff <- raw_results$fit - raw_results$fit_te
    raw_results$fit_threshold <- abs(raw_results$fit_diff) > threshold
  }

  rowid <- seq_len(nrow(raw_results))
  raw_results <- cbind(data.frame(rowid), raw_results)

  threshold_df <- data.frame(fit_threshold = sum(raw_results$fit_threshold))
  alert <- any(threshold_df > 0)

  message <- paste0(
    "tidypredict test results\n",
    "Difference threshold: ",
    threshold,
    "\n"
  )

  if (alert) {
    if (pred_type == "class") {
      message <- paste0(
        message,
        "\nMismatched predictions: ",
        threshold_df$fit_threshold
      )
    } else {
      difference <- data.frame(fit_diff = max(abs(raw_results$fit_diff)))
      message <- paste0(
        message,
        "\nFitted records above the threshold: ",
        threshold_df$fit_threshold,
        "\n\nMax difference: ",
        difference$fit_diff
      )
    }
  } else {
    message <- paste0(
      message,
      "\n All results are within the difference threshold"
    )
  }

  results <- list()
  results$model_call <- model$call
  results$raw_results <- raw_results
  results$message <- message
  results$alert <- alert
  structure(results, class = c("tidypredict_test", "list"))
}

# For {orbital}
#' Extract classprob trees for rpart models
#'
#' For use in orbital package.
#' @param model An rpart model object
#' @keywords internal
#' @export
.extract_rpart_classprob <- function(model) {
  if (!inherits(model, "rpart")) {
    cli::cli_abort(
      "{.arg model} must be {.cls rpart}, not {.obj_type_friendly {model}}."
    )
  }

  if (model$method != "class") {
    cli::cli_abort(
      "{.arg model} must be a classification model (method = 'class')."
    )
  }

  # Extract class probabilities from yval2
  # yval2 structure: [yval, count_class1, ..., count_classN, prob_class1, ..., prob_classN, nodeprob]
  yval2 <- model$frame$yval2
  ylevels <- attr(model, "ylevels")
  n_classes <- length(ylevels)

  # Probability columns are at positions (n_classes + 2) to (2 * n_classes + 1)
  prob_cols <- seq(n_classes + 2, 2 * n_classes + 1)
  probs <- yval2[, prob_cols, drop = FALSE]
  colnames(probs) <- ylevels

  # Get tree structure
  tree_info <- rpart_tree_info(model)

  generate_one_tree <- function(tree_info) {
    paths <- tree_info$nodeID[tree_info[, "terminal"]]
    child_info <- get_child_info(tree_info)

    paths <- map(
      paths,
      \(x) {
        prediction <- tree_info$prediction[tree_info$nodeID == x]
        if (is.null(prediction)) {
          cli::cli_abort("Prediction column not found.")
        }
        list(
          prediction = prediction,
          path = get_ra_path(x, tree_info, child_info, FALSE)
        )
      }
    )

    pm <- list()
    pm$general$model <- "rpart"
    pm$general$type <- "tree"
    pm$general$version <- 2
    pm$trees <- list(paths)
    parsedmodel <- as_parsed_model(pm)

    tree <- parsedmodel$trees[[1]]
    mode <- parsedmodel$general$mode
    generate_case_when_tree(tree, mode)
  }

  res <- list()
  for (i in seq_len(ncol(probs))) {
    tree_info$prediction <- probs[, i]
    res[[i]] <- generate_one_tree(tree_info)
  }
  res
}
