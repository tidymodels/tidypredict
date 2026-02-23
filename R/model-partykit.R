# For {orbital}
#' Extract comprehensive tree info for partykit models
#'
#' Returns tree structure in format needed by nested case_when generator.
#' For use in orbital package.
#' @param model A partykit model object
#' @keywords internal
#' @export
.partykit_tree_info_full <- function(model) {
  partykit_tree_info_full(model)
}

# Convert partykit tree info to the format needed by nested generator
partykit_tree_info_full <- function(model) {
  tree_df <- partykit_tree_info(model)

  # Build node_splits list in the format expected by nested generator
  node_splits <- vector("list", nrow(tree_df))

  for (i in seq_len(nrow(tree_df))) {
    if (!tree_df$terminal[i]) {
      var_name <- tree_df$splitvarName[i]

      if (!is.na(tree_df$splitclass[i])) {
        # Categorical split
        vals <- strsplit(tree_df$splitclass[i], ", ")[[1]]
        node_splits[[i]] <- list(
          primary = list(
            col = var_name,
            vals = as.list(vals),
            is_categorical = TRUE,
            needs_swap = FALSE
          ),
          surrogates = list()
        )
      } else {
        # Continuous split
        node_splits[[i]] <- list(
          primary = list(
            col = var_name,
            val = tree_df$splitval[i],
            is_categorical = FALSE,
            needs_swap = FALSE
          ),
          surrogates = list()
        )
      }
    }
  }

  list(
    nodeID = tree_df$nodeID,
    leftChild = tree_df$leftChild,
    rightChild = tree_df$rightChild,
    splitvarName = tree_df$splitvarName,
    terminal = tree_df$terminal,
    prediction = tree_df$prediction,
    node_splits = node_splits,
    majority_left = rep(NA, nrow(tree_df)),
    use_surrogates = FALSE
  )
}

partykit_tree_info <- function(model) {
  # Get all node IDs at once (avoids repeated tree traversals)
  all_node_ids <- partykit::nodeids(model)
  n_nodes <- length(all_node_ids)

  # Extract all nodes at once using nodeapply with all IDs
  all_nodes <- partykit::nodeapply(model, ids = all_node_ids, FUN = identity)

  # Pre-extract node properties to avoid repeated list access
  is_split <- logical(n_nodes)
  splitvarID <- integer(n_nodes)
  splitval <- numeric(n_nodes)
  split_index <- vector("list", n_nodes)
  left_child <- integer(n_nodes)
  right_child <- integer(n_nodes)

  for (i in seq_len(n_nodes)) {
    node <- all_nodes[[i]]
    is_split[i] <- !partykit::is.terminal(node)
    if (is_split[i]) {
      splitvarID[i] <- node$split$varid
      splitval[i] <- node$split$breaks %||% NA_real_
      split_index[[i]] <- node$split$index
      kids <- partykit::kids_node(node)
      left_child[i] <- partykit::id_node(kids[[1]])
      right_child[i] <- partykit::id_node(kids[[2]])
    } else {
      splitvarID[i] <- NA_integer_
      splitval[i] <- NA_real_
      left_child[i] <- NA_integer_
      right_child[i] <- NA_integer_
    }
  }

  # Extract predictions from fitted data (only need to access once)
  fitted_data <- model$fitted
  response_col <- fitted_data[["(response)"]]
  node_col <- fitted_data[["(fitted)"]]

  if (is.numeric(response_col)) {
    # Regression: compute mean per node
    node_means <- tapply(response_col, node_col, mean)
    prediction <- ifelse(!is_split, node_means[as.character(all_node_ids)], NA)
  } else {
    # Classification: compute mode per node
    stat_mode <- function(x) {
      counts <- rev(sort(table(x)))
      if (length(counts) > 1 && counts[[1]] == counts[[2]]) {
        ties <- counts[counts[1] == counts]
        return(names(rev(ties))[1])
      }
      names(counts)[1]
    }
    node_modes <- tapply(response_col, node_col, stat_mode)
    prediction <- ifelse(!is_split, node_modes[as.character(all_node_ids)], NA)
  }

  # Get variable info
  vars <- as.character(attr(model$terms, "variables"))
  vars <- vars[2:length(vars)]

  var_details <- map_chr(model$data, class)
  var_class <- as.character(var_details)
  var_name <- names(var_details)

  # Build categorical split strings
  if (length(var_class) > 0) {
    class_splits <- character(n_nodes)
    for (i in seq_len(n_nodes)) {
      if (is.na(splitvarID[i])) {
        class_splits[i] <- NA_character_
      } else {
        v <- vars[splitvarID[i]]
        if (var_class[var_name == v] == "factor") {
          lvls <- levels(model$data[, colnames(model$data) == v])
          pn <- split_index[[i]]
          pn <- ifelse(is.na(pn), 0, pn)
          if (any(pn == 3)) {
            cli::cli_abort("Three levels are not supported.")
          }
          class_splits[i] <- paste0(lvls[pn == 1], collapse = ", ")
        } else {
          class_splits[i] <- NA_character_
        }
      }
    }
  } else {
    class_splits <- rep(NA_character_, n_nodes)
  }

  data.frame(
    nodeID = all_node_ids - 1L,
    leftChild = left_child - 1L,
    rightChild = right_child - 1L,
    splitvarID = splitvarID,
    splitvarName = vars[splitvarID],
    splitval = splitval,
    splitclass = class_splits,
    terminal = !is_split,
    prediction = prediction
  )
}

#' @export
parse_model.party <- function(model) {
  pm <- list()
  pm$general$model <- "party"
  pm$general$type <- "tree"
  pm$general$version <- 3
  pm$tree_info <- partykit_tree_info_full(model)
  as_parsed_model(pm)
}

# Fit formula -----------------------------------

#' @export
tidypredict_fit.party <- function(model, ...) {
  tree_info <- partykit_tree_info_full(model)
  generate_nested_case_when_tree(tree_info)
}

# Legacy tree extraction (no longer used) ---------------------------
# This function was used by the old approach to populate pm$trees in flat path
# format. Now parse_model.party() uses partykit_tree_info_full() to populate
# pm$tree_info instead. Uses get_ra_path() and get_child_info() from
# model-ranger.R. Kept for reference.

get_pk_tree <- function(model) {
  tree <- partykit_tree_info(model)
  paths <- tree$nodeID[tree[, "terminal"]]

  child_info <- get_child_info(tree)

  map(
    paths,
    ~ {
      prediction <- tree$prediction[tree$nodeID == .x]
      if (is.null(prediction)) {
        cli::cli_abort("Prediction column not found.")
      }
      if (is.factor(prediction)) {
        prediction <- as.character(prediction)
      }
      list(
        prediction = prediction,
        path = get_ra_path(.x, tree, child_info, FALSE)
      )
    }
  )
}

# For {orbital}
#' Extract classprob trees for partykit models
#'
#' For use in orbital package.
#' @param model A partykit model object
#' @keywords internal
#' @export
.extract_partykit_classprob <- function(model) {
  if (!inherits(model, "party")) {
    cli::cli_abort(
      "{.arg model} must be {.cls party}, not {.obj_type_friendly {model}}."
    )
  }

  extract_classprob <- function(model) {
    mod <- model$fitted
    response <- mod[["(response)"]]
    weights <- mod[["(weights)"]]

    lvls <- levels(response)
    weights_sum <- tapply(weights, response, sum)
    weights_sum[is.na(weights_sum)] <- 0
    res <- weights_sum / sum(weights)
    names(res) <- lvls
    res
  }

  preds <- map(seq_along(model), ~ extract_classprob(model[[.x]]))
  preds <- matrix(
    unlist(preds),
    nrow = length(preds),
    byrow = TRUE,
    dimnames = list(NULL, names(preds[[1]]))
  )

  tree_info_full <- partykit_tree_info_full(model)

  res <- list()
  for (i in seq_len(ncol(preds))) {
    tree_info_copy <- tree_info_full
    tree_info_copy$prediction <- preds[, i]
    res[[i]] <- generate_nested_case_when_tree(tree_info_copy)
  }
  res
}
