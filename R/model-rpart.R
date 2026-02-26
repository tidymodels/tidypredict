# For {orbital}
#' Extract comprehensive tree info for rpart models
#'
#' Returns tree structure in format needed by nested case_when generator.
#' For use in orbital package.
#' @param model An rpart model object
#' @keywords internal
#' @export
.rpart_tree_info_full <- function(model) {
  rpart_tree_info_full(model)
}

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

  # Get predictions for ALL nodes (needed for usesurrogate=0)
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
    left_levels <- xlevels[csplit_row == 1]
    list(
      col = var_name,
      vals = as.list(left_levels),
      is_categorical = TRUE,
      needs_swap = FALSE
    )
  }
}

# Simplified tree info for tests (returns data frame like other models)
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

#' @export
parse_model.rpart <- function(model) {
  pm <- list()
  pm$general$model <- "rpart"
  pm$general$type <- "tree"
  pm$general$version <- 3
  pm$tree_info <- rpart_tree_info_full(model)
  as_parsed_model(pm)
}

#' @export
tidypredict_fit.rpart <- function(model, ...) {
  tree_info <- rpart_tree_info_full(model)
  generate_nested_case_when_tree(tree_info)
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

  # rpart uses type = "vector" for regression, type = "class" for classification
  pred_type <- if (model$method == "class") "class" else "vector"
  base <- predict(model, df, type = pred_type)

  # For classification, threshold should be 0 (exact match)
  if (model$method == "class") {
    threshold <- 0
    base <- as.character(base)
  }

  te <- tidypredict_to_column(
    df,
    model,
    add_interval = FALSE,
    vars = c("fit_te", "upr_te", "lwr_te")
  )

  raw_results <- data.frame(fit = base, fit_te = te$fit_te)
  raw_results$fit_diff <- if (model$method == "class") {
    as.numeric(raw_results$fit != raw_results$fit_te)
  } else {
    raw_results$fit - raw_results$fit_te
  }
  raw_results$fit_threshold <- abs(raw_results$fit_diff) > threshold

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
    difference <- max(abs(raw_results$fit_diff))
    message <- paste0(
      message,
      "\nFitted records above the threshold: ",
      threshold_df$fit_threshold,
      "\n\nMax difference: ",
      difference
    )
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
  tree_info <- rpart_tree_info_full(model)

  res <- list()
  for (i in seq_len(ncol(probs))) {
    tree_info_copy <- tree_info
    tree_info_copy$prediction <- probs[, i]
    res[[i]] <- generate_nested_case_when_tree(tree_info_copy)
  }
  res
}
