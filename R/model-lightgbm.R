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
lgb_exp_objectives <- c("poisson", "gamma", "tweedie")
lgb_sigmoid_objectives <- c("binary", "cross_entropy")
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
  pm$general$version <- 1

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

  # Extract trees
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
    if (grepl("^Tree=", line)) {
      save_tree_linear_info()
      # Start new tree
      current_tree <- as.integer(sub("^Tree=", "", line))
      is_linear <- FALSE
      leaf_const <- NULL
      num_features <- NULL
      leaf_features <- NULL
      leaf_coeff <- NULL
    } else if (grepl("^is_linear=1", line)) {
      is_linear <- TRUE
    } else if (grepl("^leaf_const=", line)) {
      leaf_const <- sub("^leaf_const=", "", line)
    } else if (grepl("^num_features=", line)) {
      num_features <- sub("^num_features=", "", line)
    } else if (grepl("^leaf_features=", line)) {
      leaf_features <- sub("^leaf_features=", "", line)
    } else if (grepl("^leaf_coeff=", line)) {
      leaf_coeff <- sub("^leaf_coeff=", "", line)
    } else if (grepl("^end of trees", line)) {
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

  # Find leaf rows
  leaf_rows <- which(!is.na(tree_df$leaf_index))

  # For each leaf, trace path to root
  map(leaf_rows, function(leaf_row) {
    leaf_idx <- tree_df$leaf_index[[leaf_row]]
    leaf_idx_str <- as.character(leaf_idx)
    leaf_value <- tree_df$leaf_value[[leaf_row]]

    # Check if this tree has linear info for this leaf
    if (!is.null(linear_info) && leaf_idx_str %in% names(linear_info)) {
      leaf_linear <- linear_info[[leaf_idx_str]]
      # Store both linear info and fallback value (used when features are NA)
      leaf_linear$fallback <- leaf_value
      list(
        prediction = NULL,
        linear = leaf_linear,
        path = get_lgb_path(leaf_row, tree_df, children_map)
      )
    } else {
      list(
        prediction = leaf_value,
        linear = NULL,
        path = get_lgb_path(leaf_row, tree_df, children_map)
      )
    }
  })
}

get_lgb_path <- function(leaf_row, tree_df, children_map) {
  path <- list()
  current_row <- leaf_row
  current_parent_split <- tree_df$leaf_parent[[leaf_row]]

  while (!is.na(current_parent_split)) {
    # Find the parent's row (split_index should be unique within a tree)
    parent_row <- which(tree_df$split_index == current_parent_split)[[1]]

    # Determine direction: is current_row the LEFT or RIGHT child?
    children <- children_map[[as.character(current_parent_split)]]
    is_left_child <- (current_row == children[1])

    # Build condition based on decision type
    decision_type <- tree_df$decision_type[[parent_row]]
    default_left <- tree_df$default_left[[parent_row]] == "TRUE"

    if (decision_type == "<=") {
      # Numerical split
      if (is_left_child) {
        op <- "less-equal"
        missing_with_us <- default_left
      } else {
        op <- "more"
        missing_with_us <- !default_left
      }

      condition <- list(
        type = "conditional",
        col = tree_df$split_feature[[parent_row]],
        val = tree_df$threshold[[parent_row]],
        op = op,
        missing = missing_with_us
      )
    } else if (decision_type == "==") {
      # Categorical split: threshold is "0||1||3" format
      # LEFT = category IN set, RIGHT = category NOT IN set
      category_set <- parse_lgb_categorical_threshold(
        tree_df$threshold[[parent_row]]
      )

      if (is_left_child) {
        op <- "in"
        missing_with_us <- default_left
      } else {
        op <- "not-in"
        missing_with_us <- !default_left
      }

      condition <- list(
        type = "set",
        col = tree_df$split_feature[[parent_row]],
        vals = category_set,
        op = op,
        missing = missing_with_us
      )
    }

    path <- c(path, list(condition))

    # Move up the tree
    current_row <- parent_row
    current_parent_split <- tree_df$node_parent[[parent_row]]
  }

  rev(path) # Reverse to get root-to-leaf order
}

# Parse LightGBM categorical threshold format "0||1||3" -> c(0, 1, 3)
parse_lgb_categorical_threshold <- function(threshold) {
  as.integer(strsplit(threshold, "[|][|]")[[1]])
}

# Fit model -----------------------------------------------

#' @export
tidypredict_fit.lgb.Booster <- function(model, nested = FALSE, ...) {
  parsedmodel <- parse_model(model)
  if (nested) {
    build_fit_formula_lgb_nested(parsedmodel, model)
  } else {
    build_fit_formula_lgb(parsedmodel)
  }
}

# Nested formula builder for lightgbm
build_fit_formula_lgb_nested <- function(parsedmodel, model) {
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

  if (objective %in% lgb_multiclass_objectives) {
    return(build_fit_formula_lgb_multiclass_nested(
      parsedmodel,
      model,
      objective
    ))
  }

  # Extract nested trees
  trees <- extract_lgb_trees_nested(model)

  # RF boosting averages trees instead of summing
  boosting <- parsedmodel$general$params$boosting
  if (!is.null(boosting) && boosting == "rf") {
    f <- reduce_addition(trees)
    f <- expr_division(f, n_trees)
  } else {
    f <- reduce_addition(trees)
  }

  # Apply transformation
  if (objective %in% lgb_exp_objectives) {
    f <- expr(exp(!!f))
  } else if (objective %in% lgb_sigmoid_objectives) {
    f <- lgb_sigmoid(f)
  }

  f
}

build_fit_formula_lgb_multiclass_nested <- function(
  parsedmodel,
  model,
  objective
) {
  n_trees <- length(parsedmodel$trees)
  num_class <- parsedmodel$general$num_class

  if (is.null(num_class) || num_class < 2) {
    cli::cli_abort("Multiclass model must have num_class >= 2.")
  }

  # Extract all nested trees
  trees <- extract_lgb_trees_nested(model)

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
    # Softmax: exp(raw_i) / sum(exp(raw_j))
    exp_raws <- map(raw_scores, ~ expr(exp(!!.x)))
    denom <- reduce_addition(exp_raws)

    result <- map(seq_len(num_class), function(i) {
      expr(exp(!!raw_scores[[i]]) / (!!denom))
    })
  } else if (objective == "multiclassova") {
    # One-vs-all: sigmoid for each class independently
    result <- map(raw_scores, lgb_sigmoid)
  }

  names(result) <- paste0("class_", seq_len(num_class) - 1)
  result
}

# Extract trees in nested format
extract_lgb_trees_nested <- function(model) {
  trees_df <- lightgbm::lgb.model.dt.tree(model)
  trees_df <- as.data.frame(trees_df)

  # Extract linear tree info
  model_json <- jsonlite::fromJSON(model$dump_model())
  feature_names <- model_json$feature_names
  linear_info <- parse_lgb_linear_trees(model, feature_names)

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
    # Single leaf tree - return leaf value
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
    if (default_left) {
      # Missing goes left
      condition <- expr(!!col_sym <= !!as.numeric(threshold) | is.na(!!col_sym))
    } else {
      # Missing goes right - condition is just <=
      condition <- expr(!!col_sym <= !!as.numeric(threshold))
    }
  } else if (decision_type == "==") {
    # Categorical split: LEFT = (in set), RIGHT = (not in set)
    category_set <- parse_lgb_categorical_threshold(threshold)
    if (default_left) {
      condition <- expr(!!col_sym %in% !!category_set | is.na(!!col_sym))
    } else {
      condition <- expr(!!col_sym %in% !!category_set)
    }
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

build_fit_formula_lgb <- function(parsedmodel) {
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

  if (objective %in% lgb_multiclass_objectives) {
    return(build_fit_formula_lgb_multiclass(parsedmodel, objective))
  }

  # Single output objectives: sum all trees
  f <- build_lgb_tree_sum(seq_len(n_trees), parsedmodel)

  # RF boosting averages trees instead of summing
  boosting <- parsedmodel$general$params$boosting
  if (!is.null(boosting) && boosting == "rf") {
    f <- expr_division(f, n_trees)
  }

  # Apply transformation
  if (objective %in% lgb_exp_objectives) {
    f <- expr(exp(!!f))
  } else if (objective %in% lgb_sigmoid_objectives) {
    f <- lgb_sigmoid(f)
  }

  f
}

build_fit_formula_lgb_multiclass <- function(parsedmodel, objective) {
  n_trees <- length(parsedmodel$trees)
  num_class <- parsedmodel$general$num_class

  if (is.null(num_class) || num_class < 2) {
    cli::cli_abort("Multiclass model must have num_class >= 2.")
  }

  # Group trees by class: tree i belongs to class (i-1) %% num_class
  # (trees are 1-indexed in our list, but LightGBM uses 0-indexed tree_index)
  class_trees <- lapply(seq_len(num_class), function(class_idx) {
    which((seq_len(n_trees) - 1) %% num_class == (class_idx - 1))
  })

  # Build raw score formula for each class
  raw_scores <- lapply(
    class_trees,
    build_lgb_tree_sum,
    parsedmodel = parsedmodel
  )

  # Apply transformation based on objective
  if (objective == "multiclass") {
    # Softmax: exp(raw_i) / sum(exp(raw_j))
    exp_raws <- map(raw_scores, ~ expr(exp(!!.x)))
    denom <- reduce_addition(exp_raws)

    result <- map(seq_len(num_class), function(i) {
      expr(exp(!!raw_scores[[i]]) / (!!denom))
    })
  } else if (objective == "multiclassova") {
    # One-vs-all: sigmoid for each class independently
    result <- map(raw_scores, lgb_sigmoid)
  }

  names(result) <- paste0("class_", seq_len(num_class) - 1)
  result
}

# Helper to build sum of tree predictions for given indices
build_lgb_tree_sum <- function(tree_indices, parsedmodel) {
  if (length(tree_indices) == 0) {
    # nocov start
    cli::cli_abort("No trees found for tree indices.", .internal = TRUE)
    # nocov end
  }
  tree_formulas <- map(
    tree_indices,
    ~ expr(case_when(!!!get_lgb_case_tree(.x, parsedmodel)))
  )
  reduce_addition(tree_formulas)
}

# Helper for sigmoid transformation
lgb_sigmoid <- function(f) {
  expr(1 / (1 + exp(-(!!f))))
}

get_lgb_case_tree <- function(tree_no, parsedmodel) {
  map(
    parsedmodel$trees[[tree_no]],
    ~ get_lgb_case(.x$path, .x$prediction, .x$linear)
  )
}

get_lgb_case <- function(path, prediction, linear = NULL) {
  conditions <- map(path, get_lgb_case_fun)
  cl <- combine_path_conditions(conditions)
  pred_expr <- if (!is.null(linear)) {
    build_lgb_linear_prediction(linear)
  } else {
    prediction
  }
  expr(!!cl ~ !!pred_expr)
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
  any_na <- if (length(na_checks) == 1) {
    na_checks[[1]]
  } else {
    reduce_or(na_checks)
  }

  # If any feature is NA, use fallback; otherwise use linear formula
  expr(ifelse(!!any_na, !!fallback, !!linear_formula))
}

# For {orbital}
#' Extract processed LightGBM trees
#'
#' For use in orbital package.
#' @param model A LightGBM model object
#' @param nested Logical, whether to use nested case_when (default FALSE)
#' @keywords internal
#' @export
.extract_lgb_trees <- function(model, nested = FALSE) {
  if (!inherits(model, "lgb.Booster")) {
    cli::cli_abort(
      "{.arg model} must be {.cls lgb.Booster}, not {.obj_type_friendly {model}}."
    )
  }

  if (nested) {
    extract_lgb_trees_nested(model)
  } else {
    parsedmodel <- parse_model(model)
    map(
      seq_len(length(parsedmodel$trees)),
      ~ expr(case_when(!!!get_lgb_case_tree(.x, parsedmodel)))
    )
  }
}

get_lgb_case_fun <- function(.x) {
  if (.x$type == "conditional") {
    build_lgb_conditional_expr(.x$col, .x$op, .x$val, .x$missing)
  } else if (.x$type == "set") {
    build_lgb_set_expr(.x$col, .x$op, .x$vals, .x$missing)
  } else {
    cli::cli_abort("Unknown condition type: {.val {(.x$type)}}")
  }
}

build_lgb_conditional_expr <- function(col, op, val, include_missing) {
  col_name <- as.name(col)
  val <- as.numeric(val)

  base_expr <- switch(
    op,
    "less-equal" = expr(!!col_name <= !!val),
    "more" = expr(!!col_name > !!val),
    # nocov start
    cli::cli_abort(
      "Unknown operator for conditional: {.val {op}}.",
      .internal = TRUE
    )
    # nocov end
  )

  add_missing_condition(base_expr, col_name, include_missing)
}

build_lgb_set_expr <- function(col, op, vals, include_missing) {
  col_name <- as.name(col)

  base_expr <- switch(
    op,
    "in" = expr(!!col_name %in% !!vals),
    "not-in" = expr(!(!!col_name %in% !!vals)),
    cli::cli_abort("Unknown operator for set: {.val {op}}")
  )

  add_missing_condition(base_expr, col_name, include_missing)
}

add_missing_condition <- function(base_expr, col_name, include_missing) {
  if (include_missing) {
    expr((!!base_expr | is.na(!!col_name)))
  } else {
    base_expr
  }
}
