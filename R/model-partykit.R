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
    branches = attr(tree_df, "branches"),
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
  split_breaks <- vector("list", n_nodes)
  kid_ids <- vector("list", n_nodes)
  left_child <- integer(n_nodes)
  right_child <- integer(n_nodes)

  for (i in seq_len(n_nodes)) {
    node <- all_nodes[[i]]
    is_split[i] <- !partykit::is.terminal(node)
    if (is_split[i]) {
      breaks <- node$split$breaks
      splitvarID[i] <- node$split$varid
      # A multiway split carries one break per cut point, which has no single
      # value to record here; those nodes are described by `branches` instead.
      splitval[i] <- if (length(breaks) == 1) breaks else NA_real_
      split_breaks[[i]] <- breaks
      split_index[[i]] <- node$split$index
      split_right[i] <- node$split$right %||% TRUE
      kids <- partykit::kids_node(node)

      # With a break, `index` maps each interval to a kid, and it is not
      # always the identity: `partykit::as.party.rpart()` writes `2, 1`, which
      # puts the interval below the break on the *second* kid. Taking the kids
      # in order silently swaps both branches of every converted rpart tree.
      bin_kid <- node$split$index
      if (is.null(breaks) || is.null(bin_kid)) {
        bin_kid <- seq_along(kids)
      }

      kid_ids[[i]] <- vapply(kids, partykit::id_node, integer(1))[bin_kid]
      left_child[i] <- kid_ids[[i]][1]
      right_child[i] <- kid_ids[[i]][2]
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

  branches <- vector("list", n_nodes)
  for (i in seq_len(n_nodes)) {
    if (is.na(splitvarID[i]) || length(kid_ids[[i]]) < 3) {
      next
    }
    v <- vars[splitvarID[i]]
    branches[[i]] <- partykit_node_branches(
      col = v,
      kids = kid_ids[[i]] - 1L,
      breaks = split_breaks[[i]],
      index = split_index[[i]],
      right = split_right[i],
      levels = levels(model$data[, colnames(model$data) == v]),
      is_ordered = is_ordered_var[[v]]
    )
  }

  out <- data.frame(
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
  attr(out, "branches") <- branches
  out
}

# Describe a split with more than two kids as one condition per kid.
#
# The kids are already in interval order. Only the first `k - 1` need a
# condition, because the clauses are tried in order and the last kid is
# whatever is left over. That also means each condition is a plain upper
# bound rather than an interval.
partykit_node_branches <- function(
  col,
  kids,
  breaks,
  index,
  right,
  levels,
  is_ordered
) {
  if (is.null(breaks)) {
    # An unordered factor has no breaks: `index` names the kid each level
    # belongs to, so every kid is a set of levels.
    splits <- lapply(seq_len(length(kids) - 1), function(kid) {
      list(
        col = col,
        vals = as.list(levels[which(index == kid)]),
        is_categorical = TRUE
      )
    })
    return(list(kids = kids, splits = splits))
  }

  splits <- lapply(seq_along(breaks), function(b) {
    if (is_ordered) {
      n_left <- if (right) breaks[b] else breaks[b] - 1
      return(list(
        col = col,
        vals = as.list(levels[seq_len(n_left)]),
        is_categorical = TRUE
      ))
    }
    list(col = col, val = breaks[b], is_categorical = FALSE, strict = !right)
  })

  list(kids = kids, splits = splits)
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

# Extractors --------------------------------------------------

#' @export
tidypredict_class_exprs.party <- function(x, ...) {
  rlang::check_dots_empty()
  model <- x

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

  res <- map(seq_len(ncol(preds)), function(i) {
    generate_nested_case_when_tree(
      tree_info_with_predictions(tree_info_full, preds[, i]),
      missing = "na"
    )
  })
  # The generic is named by outcome level so callers never have to assume this
  # matches `levels()` of the outcome positionally.
  stats::setNames(res, colnames(preds))
}

#' @exportS3Method
build_tree_formula.pm_tree_party <- function(model) {
  generate_nested_case_when_tree(model$tree_info, missing = "na")
}

# Output metadata ---------------------------------

# `party_tree_info()` reads the mode off the response column of `fitted`, and
# the parsed form does not record it, so the fitted object answers.
party_is_classification <- function(x) {
  is.factor(x$fitted[["(response)"]])
}

#' @export
tidypredict_output_type.party <- function(x, ...) {
  rlang::check_dots_empty()

  if (party_is_classification(x)) {
    return("class")
  }
  "numeric"
}

#' @export
tidypredict_outcome_levels.party <- function(x, ...) {
  rlang::check_dots_empty()

  if (party_is_classification(x)) {
    return(levels(x$fitted[["(response)"]]))
  }
  NULL
}

#' @export
tidypredict_normalized.party <- function(x, ...) {
  rlang::check_dots_empty()

  # A single expression, so there are no per-level values to sum.
  NA
}
