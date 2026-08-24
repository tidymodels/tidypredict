# Constants ---------------------------------------------------------------

lgb_identity_objectives <- c(
  "regression",
  "regression_l2",
  "regression_l1",
  "huber",
  "fair",
  "quantile",
  "mape"
)
# How close to zero LightGBM's `IsZero()` counts as zero, from
# `kZeroAsMissingValueRange`.
lgb_zero_threshold <- 1e-35

lgb_exp_objectives <- c("poisson", "gamma", "tweedie")
lgb_sigmoid_objectives <- c("binary", "cross_entropy")
# `reg_sqrt` trains on `sqrt(|y|)` keeping the sign, so `ConvertOutput` squares
# the raw score back onto the response scale. Every identity objective takes the
# parameter, but `huber` does not act on it: its predictions stay on the raw
# scale whatever `reg_sqrt` says. Verified against `predict()` for all six.
lgb_reg_sqrt_objectives <- c(
  "regression",
  "regression_l2",
  "regression_l1",
  "fair",
  "quantile",
  "mape"
)
lgb_multiclass_objectives <- c("multiclass", "multiclassova")
lgb_supported_objectives <- c(
  lgb_identity_objectives,
  lgb_exp_objectives,
  lgb_sigmoid_objectives,
  lgb_multiclass_objectives
)

# Model parser -------------------------------------

#' @export
parse_model.lgb.Booster <- function(model) {
  pm <- list()
  pm$general$model <- "lgb.Booster"
  pm$general$type <- "lgb"
  pm$general$version <- 3

  # Extract params (objective, etc.)
  pm$general$params <- model$params

  # Extract feature names and multiclass info from JSON dump
  model_json <- jsonlite::fromJSON(model$dump_model())
  pm$general$feature_names <- model_json$feature_names
  pm$general$nfeatures <- length(model_json$feature_names)
  pm$general$num_class <- model_json$num_class
  pm$general$num_tree_per_iteration <- model_json$num_tree_per_iteration

  # Extract number of iterations
  pm$general$niter <- model$current_iter()

  # Extract linear tree info from model string (if any)
  linear_info <- parse_lgb_linear_trees(model, pm$general$feature_names)

  # Extract trees (flat path format for serialization)
  pm$trees <- get_lgb_trees(model, linear_info)

  as_parsed_model(pm)
}

# Parse linear tree info from model string
parse_lgb_linear_trees <- function(model, feature_names) {
  model_str <- model$save_model_to_string()
  lines <- strsplit(model_str, "\n")[[1]]

  # Find tree boundaries and extract linear info
  linear_info <- list()
  current_tree <- NULL
  is_linear <- FALSE
  leaf_const <- NULL
  num_features <- NULL
  leaf_features <- NULL
  leaf_coeff <- NULL

  save_tree_linear_info <- function() {
    if (!is.null(current_tree) && is_linear) {
      linear_info[[as.character(current_tree)]] <<- parse_lgb_linear_leaves(
        leaf_const,
        num_features,
        leaf_features,
        leaf_coeff,
        feature_names
      )
    }
  }

  for (line in lines) {
    if (startsWith(line, "Tree=")) {
      save_tree_linear_info()
      # Start new tree
      current_tree <- as.integer(sub("^Tree=", "", line))
      is_linear <- FALSE
      leaf_const <- NULL
      num_features <- NULL
      leaf_features <- NULL
      leaf_coeff <- NULL
    } else if (startsWith(line, "is_linear=1")) {
      is_linear <- TRUE
    } else if (startsWith(line, "leaf_const=")) {
      leaf_const <- sub("^leaf_const=", "", line)
    } else if (startsWith(line, "num_features=")) {
      num_features <- sub("^num_features=", "", line)
    } else if (startsWith(line, "leaf_features=")) {
      leaf_features <- sub("^leaf_features=", "", line)
    } else if (startsWith(line, "leaf_coeff=")) {
      leaf_coeff <- sub("^leaf_coeff=", "", line)
    } else if (startsWith(line, "end of trees")) {
      save_tree_linear_info()
      break
    }
  }

  linear_info
}

# Parse linear leaf info for a single tree
parse_lgb_linear_leaves <- function(
  const_str,
  num_features_str,
  features_str,
  coeff_str,
  feature_names
) {
  # Parse leaf_const (space-separated floats)
  consts <- as.numeric(strsplit(trimws(const_str), "\\s+")[[1]])
  n_leaves <- length(consts)

  # Parse num_features to know how many features per leaf
  num_feats <- as.integer(strsplit(trimws(num_features_str), "\\s+")[[1]])

  # Parse all features and coefficients as flat vectors
  features_str_trimmed <- trimws(features_str)
  coeff_str_trimmed <- trimws(coeff_str)

  if (nchar(features_str_trimmed) > 0) {
    all_features <- as.integer(strsplit(features_str_trimmed, "\\s+")[[1]])
    all_coeffs <- as.numeric(strsplit(coeff_str_trimmed, "\\s+")[[1]])
  } else {
    all_features <- integer(0)
    all_coeffs <- numeric(0)
  }

  # Split features and coefficients by num_feats
  idx <- 1
  linear_leaves <- lapply(seq_len(n_leaves), function(i) {
    nf <- num_feats[i]
    if (nf > 0) {
      feat_idx <- all_features[idx:(idx + nf - 1)]
      coeffs <- all_coeffs[idx:(idx + nf - 1)]
      idx <<- idx + nf
      list(
        intercept = consts[i],
        feature_names = feature_names[feat_idx + 1],
        coefficients = coeffs
      )
    } else {
      list(
        intercept = consts[i],
        feature_names = character(0),
        coefficients = numeric(0)
      )
    }
  })
  names(linear_leaves) <- as.character(seq_len(n_leaves) - 1)

  linear_leaves
}

get_lgb_trees <- function(model, linear_info = list()) {
  trees_df <- lightgbm::lgb.model.dt.tree(model)
  trees_df <- as.data.frame(trees_df)
  trees_df <- add_lgb_missing_type(trees_df, model)
  trees_df <- add_lgb_stump_trees(trees_df, model)

  # Check for unsupported decision types
  decision_types <- unique(trees_df$decision_type[
    !is.na(trees_df$decision_type)
  ])
  supported_types <- c("<=", "==")
  unsupported <- setdiff(decision_types, supported_types)
  if (length(unsupported) > 0) {
    # nocov start
    cli::cli_abort(
      c(
        "Unsupported decision type{?s} found: {.val {unsupported}}.",
        "i" = "Supported types: {.val {supported_types}}."
      ),
      .internal = TRUE
    )
    # nocov end
  }

  # Split by tree_index
  trees_split <- split(trees_df, trees_df$tree_index)

  # Process each tree with its linear info (if any)
  map(names(trees_split), function(tree_idx) {
    tree_linear <- linear_info[[tree_idx]]
    get_lgb_tree(trees_split[[tree_idx]], tree_linear)
  })
}

# Add the `missing_type` of each split node to `trees_df`.
#
# `lgb.model.dt.tree()` does not report it, and it decides how a missing value
# is routed, so it has to be read from the JSON dump and joined on by tree and
# split index.
add_lgb_missing_type <- function(trees_df, model) {
  dump <- jsonlite::fromJSON(model$dump_model(), simplifyVector = FALSE)

  keys <- character(0)
  types <- character(0)
  collect <- function(node, tree_index) {
    if (is.null(node$split_index)) {
      return(invisible())
    }
    keys <<- c(keys, paste(tree_index, node$split_index))
    types <<- c(types, node$missing_type %||% "None")
    collect(node$left_child, tree_index)
    collect(node$right_child, tree_index)
  }
  for (tree in dump$tree_info) {
    collect(tree$tree_structure, tree$tree_index)
  }

  lookup <- stats::setNames(types, keys)
  trees_df$missing_type <- unname(
    lookup[paste(trees_df$tree_index, trees_df$split_index)]
  )
  trees_df
}

# Add back the trees that `lgb.model.dt.tree()` drops.
#
# When LightGBM cannot make a split it emits a tree that is a bare leaf, and
# `lgb.model.dt.tree()` reports no rows at all for such a tree. The leaf value
# is still in the JSON dump, so each dropped tree is rebuilt here as the single
# leaf row the rest of the parser expects. If no split is ever possible
# LightGBM halts after one iteration and every tree is a stump, leaving
# `trees_df` empty.
#
# Restoring the trees is also what keeps a multiclass model correct, which is a
# separate concern from the empty-`trees_df` one and is easy to lose sight of.
# A multiclass fit assigns trees to classes positionally, so a dropped tree
# shifts every class after the gap. That needs only one absent class, which is
# ordinary in imbalanced or filtered data, and it is silent: the probabilities
# still sum to 1. Restoring the trees closes the gap and the positional
# assignment is right again. Anyone reworking this must keep that property:
# if the dropped trees are no longer restored, the class assignment has to be
# made `tree_index`-aware instead, or the defect returns unnoticed. (#419)
add_lgb_stump_trees <- function(trees_df, model) {
  dump <- jsonlite::fromJSON(model$dump_model(), simplifyVector = FALSE)

  stumps <- Filter(
    \(tree) is.null(tree$tree_structure$split_index),
    dump$tree_info
  )
  if (length(stumps) == 0) {
    return(trees_df)
  }

  rows <- map(stumps, function(tree) {
    row <- trees_df[NA_integer_, , drop = FALSE]
    row$tree_index <- as.integer(tree$tree_index)
    row$depth <- 0L
    row$leaf_index <- 0L
    row$leaf_value <- as.numeric(tree$tree_structure$leaf_value)
    row$leaf_count <- as.integer(tree$tree_structure$leaf_count)
    row
  })

  trees_df <- do.call(rbind, c(list(trees_df), rows))
  trees_df <- trees_df[order(trees_df$tree_index), , drop = FALSE]
  rownames(trees_df) <- NULL
  trees_df
}

# The left branch of a numerical split, `col <= val`, with the values
# `missing_type` sends this way folded in.
#
# `Tree::NumericalDecision` consults `default_left` only when the node's
# `missing_type` is `NaN` or `Zero`. Under `None`, which is what a feature with
# no missing value in the training data gets, a missing value is coerced to `0`
# and compared against the threshold like any other, so it goes wherever `0`
# goes. `Zero` treats an exact zero as missing as well.
lgb_numeric_left <- function(col, val, missing_type, default_left) {
  col <- rlang::sym(col)
  val <- as.numeric(val)
  missing_type <- missing_type %||% "NaN"

  if (missing_type == "None") {
    if (0 <= val) {
      return(expr(is.na(!!col) | !!col <= !!val))
    }
    return(expr(!is.na(!!col) & !!col <= !!val))
  }

  is_missing <- if (missing_type == "Zero") {
    expr(is.na(!!col) | abs(!!col) <= !!lgb_zero_threshold)
  } else {
    expr(is.na(!!col))
  }

  if (default_left) {
    expr(!!is_missing | !!col <= !!val)
  } else {
    expr(!(!!is_missing) & !!col <= !!val)
  }
}

get_lgb_children_map <- function(tree_df) {
  # For each split_index, find its children (ordered by row index)
  split_indices <- tree_df$split_index[!is.na(tree_df$split_index)]

  children_map <- lapply(split_indices, function(si) {
    # Children are rows where node_parent==si OR leaf_parent==si
    child_rows <- which(tree_df$node_parent == si | tree_df$leaf_parent == si)
    # Sort by row index: first is LEFT, second is RIGHT
    child_rows[order(child_rows)]
  })
  names(children_map) <- as.character(split_indices)
  children_map
}

get_lgb_tree <- function(tree_df, linear_info = NULL) {
  # Build children map for direction detection
  children_map <- get_lgb_children_map(tree_df)

  # Pre-extract columns as vectors for fast indexing
  leaf_index <- tree_df$leaf_index
  leaf_value <- tree_df$leaf_value
  leaf_parent <- tree_df$leaf_parent
  split_index <- tree_df$split_index
  node_parent <- tree_df$node_parent
  decision_type <- tree_df$decision_type
  default_left <- tree_df$default_left == "TRUE"
  split_feature <- tree_df$split_feature
  threshold <- tree_df$threshold
  missing_type <- tree_df$missing_type

  # Build split_index to row lookup (avoid repeated which() calls)
  max_split_idx <- suppressWarnings(max(split_index, na.rm = TRUE))
  if (is.finite(max_split_idx)) {
    split_idx_to_row <- integer(max_split_idx + 1)
    for (i in seq_along(split_index)) {
      si <- split_index[i]
      if (!is.na(si)) {
        split_idx_to_row[si + 1L] <- i
      }
    }
  } else {
    # No splits (stump tree) - empty lookup
    split_idx_to_row <- integer(0)
  }

  # Find leaf rows
  leaf_rows <- which(!is.na(leaf_index))

  # For each leaf, trace path to root
  map(leaf_rows, function(leaf_row) {
    leaf_idx <- leaf_index[leaf_row]
    leaf_idx_str <- as.character(leaf_idx)
    leaf_val <- leaf_value[leaf_row]

    # Check if this tree has linear info for this leaf
    if (!is.null(linear_info) && leaf_idx_str %in% names(linear_info)) {
      leaf_linear <- linear_info[[leaf_idx_str]]
      # Store both linear info and fallback value (used when features are NA)
      leaf_linear$fallback <- leaf_val
      list(
        prediction = NULL,
        linear = leaf_linear,
        path = get_lgb_path_fast(
          leaf_row,
          leaf_parent,
          split_idx_to_row,
          node_parent,
          decision_type,
          default_left,
          split_feature,
          threshold,
          children_map,
          missing_type
        )
      )
    } else {
      list(
        prediction = leaf_val,
        linear = NULL,
        path = get_lgb_path_fast(
          leaf_row,
          leaf_parent,
          split_idx_to_row,
          node_parent,
          decision_type,
          default_left,
          split_feature,
          threshold,
          children_map,
          missing_type
        )
      )
    }
  })
}

# Fast path extraction using pre-extracted vectors
get_lgb_path_fast <- function(
  leaf_row,
  leaf_parent,
  split_idx_to_row,
  node_parent,
  decision_type,
  default_left,
  split_feature,
  threshold,
  children_map,
  missing_type
) {
  path <- list()
  current_row <- leaf_row
  current_parent_split <- leaf_parent[leaf_row]

  while (!is.na(current_parent_split)) {
    # Look up parent row directly (O(1) instead of O(n))
    parent_row <- split_idx_to_row[current_parent_split + 1L]

    # Determine direction: is current_row the LEFT or RIGHT child?
    children <- children_map[[as.character(current_parent_split)]]
    is_left_child <- (current_row == children[1])

    # Build condition based on decision type
    dec_type <- decision_type[parent_row]
    def_left <- default_left[parent_row]

    if (dec_type == "<=") {
      # Numerical split
      if (is_left_child) {
        op <- "less-equal"
        missing_with_us <- def_left
      } else {
        op <- "more"
        missing_with_us <- !def_left
      }

      condition <- list(
        type = "conditional",
        col = split_feature[parent_row],
        val = threshold[parent_row],
        op = op,
        missing = missing_with_us,
        missing_type = missing_type[parent_row],
        default_left = def_left
      )
    } else if (dec_type == "==") {
      # Categorical split: threshold is "0||1||3" format
      check_lgb_categorical_default_left(def_left)
      category_set <- parse_lgb_categorical_threshold(threshold[parent_row])

      # A missing value always goes right, so it is never carried with the
      # left branch.
      op <- if (is_left_child) "in" else "not-in"

      condition <- list(
        type = "set",
        col = split_feature[parent_row],
        vals = category_set,
        op = op,
        missing = FALSE
      )
    }

    path <- c(path, list(condition))

    # Move up the tree
    current_row <- parent_row
    current_parent_split <- node_parent[parent_row]
  }

  rev(path) # Reverse to get root-to-leaf order
}

# Parse LightGBM categorical threshold format "0||1||3" -> c(0, 1, 3)
parse_lgb_categorical_threshold <- function(threshold) {
  as.integer(strsplit(threshold, "[|][|]")[[1]])
}

# `Tree::CategoricalDecision` sends a missing value, and any negative value,
# right whatever `default_left` says, and `Tree::SplitCategorical` never sets
# the bit in the first place, so a categorical split with `default_left` set is
# a state LightGBM does not produce. Nothing here could route it correctly, so
# it is refused rather than silently mishandled.
check_lgb_categorical_default_left <- function(default_left) {
  if (isTRUE(default_left)) {
    cli::cli_abort(
      "A categorical split cannot set {.field default_left}.",
      .internal = TRUE
    )
  }
  invisible(NULL)
}

# Shared helpers -----------------------------------------------

# Helper for sigmoid transformation.
#
# LightGBM's `binary`, `cross_entropy` and `multiclassova` objectives apply
# `1 / (1 + exp(-sigmoid * x))`, where `sigmoid` is a fit parameter defaulting
# to 1. Fixing it at 1 rescales every probability of a model fit with any other
# value.
# The `sigmoid` scaling an objective actually applies.
#
# Not every objective that ends in a logistic honours it. `binary` and
# `multiclassova` do; `cross_entropy` ignores it and is always plain
# `1 / (1 + exp(-x))`, whatever `sigmoid` was set to. Verified against
# `predict()` at sigmoid 1, 2 and 3.
lgb_sigmoid_param <- function(objective, params) {
  if (identical(objective, "cross_entropy")) {
    return(1)
  }
  params$sigmoid %||% 1
}

lgb_sigmoid <- function(f, sigmoid = 1) {
  if (identical(sigmoid, 1) || identical(sigmoid, 1L)) {
    return(expr_logistic(f))
  }
  expr_logistic(expr(!!sigmoid * !!f))
}

# Apply multiclass transformation to tree expressions
# Shared by nested and from_parsed multiclass builders
apply_lgb_multiclass_transformation <- function(
  trees,
  num_class,
  objective,
  sigmoid = 1
) {
  n_trees <- length(trees)

  # Group trees by class: tree i belongs to class (i-1) %% num_class
  class_trees <- lapply(seq_len(num_class), function(class_idx) {
    which((seq_len(n_trees) - 1) %% num_class == (class_idx - 1))
  })

  # Build raw score formula for each class
  raw_scores <- lapply(class_trees, function(indices) {
    reduce_addition(trees[indices])
  })

  # Apply transformation based on objective
  if (objective == "multiclass") {
    result <- expr_softmax(raw_scores)
  } else if (objective == "multiclassova") {
    # One-vs-all: sigmoid for each class independently
    result <- map(raw_scores, \(score) lgb_sigmoid(score, sigmoid))
  }

  names(result) <- paste0("class_", seq_len(num_class) - 1)
  result
}

# Build linear prediction formula: intercept + sum(coeff * feature)
# LightGBM uses fallback leaf_value when ANY feature in the formula is NA
build_lgb_linear_prediction <- function(linear) {
  intercept <- linear$intercept
  feature_names <- linear$feature_names
  coefficients <- linear$coefficients
  fallback <- linear$fallback

  if (length(feature_names) == 0) {
    # No features, just return intercept
    return(intercept)
  }

  # Build the linear formula: intercept + sum(coeff * feature)
  terms <- map(seq_along(feature_names), function(i) {
    feat <- as.name(feature_names[i])
    coef <- coefficients[i]
    expr(!!coef * !!feat)
  })
  all_terms <- c(list(intercept), terms)
  linear_formula <- reduce_addition(all_terms)

  # Build condition: any feature is NA
  na_checks <- map(feature_names, function(fn) {
    feat <- as.name(fn)
    expr(is.na(!!feat))
  })
  any_na <- reduce_or(na_checks)

  # If any feature is NA, use fallback; otherwise use linear formula
  expr(ifelse(!!any_na, !!fallback, !!linear_formula))
}

# A leaf of a linear tree stores its coefficients under `linear` and leaves
# `prediction` empty, so the parsed path has to turn that back into an
# expression before the shared tree builder reads `prediction`. Saving and
# loading a parsed model can turn the numeric vectors into lists, so they are
# flattened here.
resolve_lgb_leaf_prediction <- function(leaf) {
  linear <- leaf$linear

  if (is.null(linear)) {
    if (is.null(leaf$prediction)) {
      cli::cli_abort("Leaf has no prediction.", .internal = TRUE)
    }
    return(leaf)
  }

  linear$intercept <- unlist(linear$intercept)
  linear$fallback <- unlist(linear$fallback)
  linear$feature_names <- as.character(unlist(linear$feature_names))
  linear$coefficients <- as.numeric(unlist(linear$coefficients))

  leaf$prediction <- build_lgb_linear_prediction(linear)
  leaf
}

# Apply lightgbm objective transformation to formula
apply_lgb_objective <- function(f, objective, params) {
  if (objective %in% lgb_exp_objectives) {
    return(expr(exp(!!f)))
  }

  if (objective %in% lgb_sigmoid_objectives) {
    return(lgb_sigmoid(f, lgb_sigmoid_param(objective, params)))
  }

  if (objective %in% lgb_reg_sqrt_objectives && isTRUE(params$reg_sqrt)) {
    return(expr(sign(!!f) * (!!f)^2))
  }

  # Identity objectives - return as-is
  f
}

# Fit model (nested) -----------------------------------------------

#' @export
tidypredict_fit.lgb.Booster <- function(model, ...) {
  parsedmodel <- parse_model(model)
  build_fit_formula_lgb_nested(parsedmodel, model)
}

# Nested formula builder for lightgbm (from model directly)
build_fit_formula_lgb_nested <- function(parsedmodel, model) {
  assemble_lgb_formula(parsedmodel, function() {
    # Pass feature_names to avoid redundant JSON parsing
    extract_lgb_trees_nested(model, parsedmodel$general$feature_names)
  })
}

# Nested formula builder for lightgbm (from parsed model, version 3)
build_fit_formula_lgb_from_parsed <- function(parsedmodel) {
  assemble_lgb_formula(parsedmodel, function() {
    map(parsedmodel$trees, function(tree) {
      build_nested_from_flat_paths(
        map(tree, resolve_lgb_leaf_prediction),
        build_lgb_nested_condition,
        lgb_is_left_op
      )
    })
  })
}

# Only the source of the trees differs between a fitted model and a parsed one,
# so `build_trees` supplies them and everything else is shared. It is a function
# rather than a value so that the objective is validated before the trees are
# built.
assemble_lgb_formula <- function(parsedmodel, build_trees) {
  n_trees <- length(parsedmodel$trees)

  if (n_trees == 0) {
    cli::cli_abort("Model has no trees.")
  }

  objective <- parsedmodel$general$params$objective %||% "regression"

  if (!objective %in% lgb_supported_objectives) {
    cli::cli_abort(
      c(
        "Unsupported objective: {.val {objective}}.",
        "i" = "Supported objectives: {.val {lgb_supported_objectives}}."
      )
    )
  }

  trees <- build_trees()

  # A model of stumps mentions no column, so anchor it to one. The feature
  # names recorded at parse time are the columns `newdata` has to supply.
  recycle <- function(f) {
    expr_recycle_over_column(f, parsedmodel$general$feature_names)
  }

  if (objective %in% lgb_multiclass_objectives) {
    num_class <- parsedmodel$general$num_class
    if (is.null(num_class) || num_class < 2) {
      cli::cli_abort("Multiclass model must have num_class >= 2.")
    }
    return(map(
      apply_lgb_multiclass_transformation(
        trees,
        num_class,
        objective,
        parsedmodel$general$params$sigmoid %||% 1
      ),
      recycle
    ))
  }

  f <- reduce_addition(trees)

  # RF boosting averages trees instead of summing
  boosting <- parsedmodel$general$params$boosting
  if (!is.null(boosting) && boosting == "rf") {
    f <- expr_division(f, n_trees)
  }

  recycle(apply_lgb_objective(f, objective, parsedmodel$general$params))
}

# Build condition for lightgbm nested generation from path element
lgb_is_left_op <- function(op) {
  op %in% c("less-equal", "in")
}

build_lgb_nested_condition <- function(path_elem) {
  col <- rlang::sym(path_elem$col)
  missing <- path_elem$missing %||% FALSE

  if (path_elem$type == "conditional") {
    # For nested generation, we only build the left condition (less-equal), so
    # a model parsed before `missing_type` was recorded carries the same
    # information in `missing`.
    lgb_numeric_left(
      path_elem$col,
      path_elem$val,
      path_elem$missing_type,
      path_elem$default_left %||% missing
    )
  } else if (path_elem$type == "set") {
    vals <- unlist(path_elem$vals)
    expr(!!col %in% !!vals)
  } else {
    cli::cli_abort("Unknown path element type: {.val {path_elem$type}}")
  }
}

# Extract trees in nested format
# feature_names and linear_info can be passed to avoid redundant JSON/string parsing
extract_lgb_trees_nested <- function(
  model,
  feature_names = NULL,
  linear_info = NULL
) {
  trees_df <- lightgbm::lgb.model.dt.tree(model)
  trees_df <- as.data.frame(trees_df)
  trees_df <- add_lgb_missing_type(trees_df, model)
  trees_df <- add_lgb_stump_trees(trees_df, model)

  # Extract linear tree info (only if not provided)
  if (is.null(feature_names)) {
    model_json <- jsonlite::fromJSON(model$dump_model())
    feature_names <- model_json$feature_names
  }
  if (is.null(linear_info)) {
    linear_info <- parse_lgb_linear_trees(model, feature_names)
  }

  trees_split <- split(trees_df, trees_df$tree_index)

  map(names(trees_split), function(tree_idx) {
    tree_df <- trees_split[[tree_idx]]
    tree_linear <- linear_info[[tree_idx]]
    build_nested_lgb_tree(tree_df, tree_linear)
  })
}

# Build nested case_when for a single lightgbm tree
build_nested_lgb_tree <- function(tree_df, linear_info = NULL) {
  # Build children map
  children_map <- get_lgb_children_map(tree_df)

  # Find root node (split_index == 0)
  root_row <- which(tree_df$split_index == 0)

  if (length(root_row) == 0) {
    leaf_row <- which(!is.na(tree_df$leaf_index))
    if (length(leaf_row) == 1) {
      leaf_idx <- tree_df$leaf_index[[leaf_row]]
      leaf_value <- tree_df$leaf_value[[leaf_row]]
      if (
        !is.null(linear_info) && as.character(leaf_idx) %in% names(linear_info)
      ) {
        return(build_lgb_linear_prediction(linear_info[[as.character(
          leaf_idx
        )]]))
      }
      return(leaf_value)
    }
    cli::cli_abort("Unable to find root or leaf node.", .internal = TRUE)
  }

  build_nested_lgb_node(root_row, tree_df, children_map, linear_info)
}

# Recursively build nested case_when node
build_nested_lgb_node <- function(
  node_row,
  tree_df,
  children_map,
  linear_info
) {
  # Check if this is a leaf
  if (!is.na(tree_df$leaf_index[[node_row]])) {
    leaf_idx <- tree_df$leaf_index[[node_row]]
    leaf_value <- tree_df$leaf_value[[node_row]]

    # Check for linear leaf
    if (
      !is.null(linear_info) && as.character(leaf_idx) %in% names(linear_info)
    ) {
      leaf_linear <- linear_info[[as.character(leaf_idx)]]
      leaf_linear$fallback <- leaf_value
      return(build_lgb_linear_prediction(leaf_linear))
    }

    return(leaf_value)
  }

  # Internal node - get split info
  split_index <- tree_df$split_index[[node_row]]
  decision_type <- tree_df$decision_type[[node_row]]
  col <- tree_df$split_feature[[node_row]]
  threshold <- tree_df$threshold[[node_row]]
  default_left <- tree_df$default_left[[node_row]] == "TRUE"

  # Get children (first is LEFT, second is RIGHT)
  children <- children_map[[as.character(split_index)]]
  left_row <- children[1]
  right_row <- children[2]

  # Recurse
  left_subtree <- build_nested_lgb_node(
    left_row,
    tree_df,
    children_map,
    linear_info
  )
  right_subtree <- build_nested_lgb_node(
    right_row,
    tree_df,
    children_map,
    linear_info
  )

  # Build condition
  col_sym <- rlang::sym(col)

  if (decision_type == "<=") {
    # Numerical split: LEFT = (<= threshold), RIGHT = (> threshold)
    condition <- lgb_numeric_left(
      col,
      threshold,
      tree_df$missing_type[[node_row]],
      default_left
    )
  } else if (decision_type == "==") {
    # Categorical split: LEFT = (in set), RIGHT = (not in set).
    #
    # A missing value always goes right, which `%in%` does too.
    check_lgb_categorical_default_left(default_left)
    category_set <- parse_lgb_categorical_threshold(threshold)
    condition <- expr(!!col_sym %in% !!category_set)
  } else {
    # nocov start
    cli::cli_abort(
      "Unsupported decision type: {.val {decision_type}}.",
      .internal = TRUE
    )
    # nocov end
  }

  expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
}

# Extractors --------------------------------------------------

#' @export
tidypredict_trees.lgb.Booster <- function(x, ...) {
  rlang::check_dots_empty()

  extract_lgb_trees_nested(x)
}

#' @export
tidypredict_n_trees.lgb.Booster <- function(x, ...) {
  rlang::check_dots_empty()

  # Trees with a single leaf are dropped by the extractor, so this counts the
  # trees actually returned rather than the number LightGBM reports.
  length(tidypredict_trees(x))
}

# Output metadata ---------------------------------

# The same objective groups `build_fit_formula_lgb()` switches on: the
# multiclass objectives softmax one raw score per class, the sigmoid objectives
# give a single binary probability, and the rest stay numeric.
lgb_parsed_objective <- function(x) {
  x$general$params$objective %||% "regression"
}

#' @export
tidypredict_output_type.pm_lgb <- function(x, ...) {
  rlang::check_dots_empty()

  objective <- lgb_parsed_objective(x)
  if (objective %in% c(lgb_multiclass_objectives, lgb_sigmoid_objectives)) {
    return("prob")
  }
  "numeric"
}

#' @export
tidypredict_outcome_levels.pm_lgb <- function(x, ...) {
  rlang::check_dots_empty()

  # LightGBM is fit on integer labels. The multiclass expressions come back
  # named `class_0`, `class_1` and so on, which are positions, not levels.
  NULL
}

#' @export
tidypredict_normalized.pm_lgb <- function(x, ...) {
  rlang::check_dots_empty()

  if (lgb_parsed_objective(x) %in% lgb_multiclass_objectives) {
    return(TRUE)
  }
  NA
}
