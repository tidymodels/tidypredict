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
            strict = !tree_df$splitright[i],
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
  # `partysplit` records which end of the interval the break belongs to. The
  # default `right = TRUE` puts it on the left branch, `x <= break`; `right =
  # FALSE` makes the left branch `x < break`.
  split_right <- rep(TRUE, n_nodes)
  left_child <- integer(n_nodes)
  right_child <- integer(n_nodes)

  for (i in seq_len(n_nodes)) {
    node <- all_nodes[[i]]
    is_split[i] <- !partykit::is.terminal(node)
    if (is_split[i]) {
      splitvarID[i] <- node$split$varid
      splitval[i] <- node$split$breaks %||% NA_real_
      split_index[[i]] <- node$split$index
      split_right[i] <- node$split$right %||% TRUE
      kids <- partykit::kids_node(node)

      # With a break, `index` maps each interval to a kid, and it is not
      # always the identity: `partykit::as.party.rpart()` writes `2, 1`, which
      # puts the interval below the break on the *second* kid. Taking the kids
      # in order silently swaps both branches of every converted rpart tree.
      bin_kid <- node$split$index
      if (is.null(node$split$breaks) || is.null(bin_kid)) {
        bin_kid <- c(1L, 2L)
      }

      left_child[i] <- partykit::id_node(kids[[bin_kid[1]]])
      right_child[i] <- partykit::id_node(kids[[bin_kid[2]]])
    } else {
      splitvarID[i] <- NA_integer_
      splitval[i] <- NA_real_
      left_child[i] <- NA_integer_
      right_child[i] <- NA_integer_
    }
  }

  # Extract predictions from fitted data (only need to access once). The
  # "(weights)" column is all ones for a single tree, but cforest trees are
  # fit on in-bag subsamples, so node predictions must be weighted by it.
  fitted_data <- model$fitted
  response_col <- fitted_data[["(response)"]]
  node_col <- fitted_data[["(fitted)"]]
  weight_col <- fitted_data[["(weights)"]] %||% rep(1, length(response_col))

  if (is.numeric(response_col)) {
    # Regression: compute weighted mean per node
    node_sums <- tapply(weight_col * response_col, node_col, sum)
    node_wts <- tapply(weight_col, node_col, sum)
    node_means <- node_sums / node_wts
    prediction <- ifelse(!is_split, node_means[as.character(all_node_ids)], NA)
  } else {
    # Classification: compute weighted mode per node
    stat_mode <- function(idx) {
      counts <- rev(sort(tapply(weight_col[idx], response_col[idx], sum)))
      counts <- counts[!is.na(counts) & counts > 0]
      if (length(counts) > 1 && counts[[1]] == counts[[2]]) {
        ties <- counts[counts[1] == counts]
        return(names(rev(ties))[1])
      }
      names(counts)[1]
    }
    node_modes <- tapply(seq_along(node_col), node_col, stat_mode)
    prediction <- ifelse(!is_split, node_modes[as.character(all_node_ids)], NA)
  }

  # Get variable info. partykit's varid indexes the columns of model$data,
  # which is not always the same order as the terms (e.g. C5.0-converted
  # parties place the response last), so map through the data column names.
  vars <- names(model$data)

  # `class()` of an ordered factor is `c("ordered", "factor")`, so testing it
  # against a single string both errors on the length and would miscount an
  # ordered factor as continuous.
  is_factor_var <- vapply(model$data, is.factor, logical(1))
  is_ordered_var <- vapply(model$data, is.ordered, logical(1))

  # Build categorical split strings
  class_splits <- character(n_nodes)
  for (i in seq_len(n_nodes)) {
    if (is.na(splitvarID[i])) {
      class_splits[i] <- NA_character_
      next
    }

    v <- vars[splitvarID[i]]
    if (!is_factor_var[[v]]) {
      class_splits[i] <- NA_character_
      next
    }

    lvls <- levels(model$data[, colnames(model$data) == v])

    if (is_ordered_var[[v]]) {
      # An ordered factor is split by a break on the level's integer code
      # rather than by a set of levels, so the break names how far down the
      # level order the left branch reaches.
      # `right` applies to the break here as it does to a numeric one, so it
      # decides whether the level on the break itself is included. Which kid
      # the interval reaches is already handled with the other breaks.
      n_left <- if (split_right[i]) splitval[i] else splitval[i] - 1
      class_splits[i] <- paste0(lvls[seq_len(n_left)], collapse = ", ")
      # Recorded as a set of levels, so the break must not also be read as a
      # numeric threshold on the column
      splitval[i] <- NA_real_
      next
    }

    pn <- split_index[[i]]
    pn <- ifelse(is.na(pn), 0, pn)
    class_splits[i] <- paste0(lvls[pn == 1], collapse = ", ")
  }

  data.frame(
    nodeID = all_node_ids - 1L,
    leftChild = left_child - 1L,
    rightChild = right_child - 1L,
    splitvarID = splitvarID,
    splitvarName = vars[splitvarID],
    splitval = splitval,
    splitclass = class_splits,
    splitright = split_right,
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
  generate_nested_case_when_tree(tree_info, missing = "na")
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

  map(seq_len(ncol(preds)), function(i) {
    generate_nested_case_when_tree(
      tree_info_with_predictions(tree_info_full, preds[, i]),
      missing = "na"
    )
  })
}

build_tree_formula.pm_tree_party <- function(model) {
  generate_nested_case_when_tree(model$tree_info, missing = "na")
}
