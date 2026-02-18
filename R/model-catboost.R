# Constants ---------------------------------------------------------------

catboost_identity_objectives <- c(
  "RMSE",
  "MAE",
  "Quantile",
  "MAPE",
  "Poisson",
  "Huber",
  "LogCosh",
  "Expectile",
  "Tweedie"
)
catboost_sigmoid_objectives <- c("Logloss", "CrossEntropy")
catboost_multiclass_objectives <- c("MultiClass", "MultiClassOneVsAll")
catboost_supported_objectives <- c(
  catboost_identity_objectives,
  catboost_sigmoid_objectives,
  catboost_multiclass_objectives
)

# Model parser ------------------------------------------------------------

catboost_catboost.save_model <- function(...) {
  rlang::eval_tidy(rlang::call2("catboost.save_model", ..., .ns = "catboost"))
}

catboost_catboost.load_pool <- function(...) {
  rlang::eval_tidy(rlang::call2("catboost.load_pool", ..., .ns = "catboost"))
}

catboost_catboost.train <- function(...) {
  rlang::eval_tidy(rlang::call2("catboost.train", ..., .ns = "catboost"))
}

catboost_catboost.predict <- function(...) {
  rlang::eval_tidy(rlang::call2("catboost.predict", ..., .ns = "catboost"))
}

#' @export
parse_model.catboost.Model <- function(model) {
  pm <- list()
  pm$general$model <- "catboost.Model"
  pm$general$type <- "catboost"
  pm$general$version <- 1

  tmp_file <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_file), add = TRUE)
  catboost_catboost.save_model(model, tmp_file, file_format = "json")
  model_json <- jsonlite::fromJSON(tmp_file, simplifyVector = FALSE)

  pm$general$params <- list()
  loss_fn <- model_json$model_info$params$loss_function
  if (!is.null(loss_fn) && is.list(loss_fn) && !is.null(loss_fn$type)) {
    pm$general$params$objective <- loss_fn$type
  }

  # Extract num_class for multiclass models
  objective <- pm$general$params$objective
  class_params <- model_json$model_info$class_params
  is_multiclass <- !is.null(objective) &&
    objective %in% catboost_multiclass_objectives &&
    !is.null(class_params) &&
    !is.null(class_params$class_names)

  if (is_multiclass) {
    pm$general$num_class <- length(class_params$class_names)
  } else {
    pm$general$num_class <- 1
  }

  features_info <- model_json$features_info

  # Extract float features
  float_features <- features_info$float_features
  if (length(float_features) > 0) {
    feature_names <- vapply(
      float_features,
      function(f) f$feature_id %||% paste0("feature_", f$flat_feature_index),
      character(1)
    )
  } else {
    feature_names <- character(0)
  }

  # Extract categorical features
  cat_features <- features_info$categorical_features
  cat_feature_names <- character(0)
  if (length(cat_features) > 0) {
    cat_feature_info <- lapply(cat_features, function(f) {
      list(
        feature_id = f$feature_id %||%
          paste0("cat_feature_", f$flat_feature_index),
        feature_index = f$feature_index,
        flat_feature_index = f$flat_feature_index,
        hash_values = unlist(f$values)
      )
    })
    cat_feature_names <- vapply(
      cat_feature_info,
      `[[`,
      character(1),
      "feature_id"
    )
    pm$general$cat_features <- cat_feature_info
    pm$general$cat_feature_names <- cat_feature_names
  } else {
    pm$general$cat_features <- list()
    pm$general$cat_feature_names <- character(0)
  }

  pm$general$feature_names <- feature_names
  pm$general$nfeatures <- length(feature_names) + length(cat_feature_names)

  oblivious_trees <- model_json$oblivious_trees
  nonoblivious_trees <- model_json$trees

  # Extract scale and bias
  scale_and_bias <- model_json$scale_and_bias
  pm$general$scale <- scale_and_bias[[1]]
  pm$general$bias <- scale_and_bias[[2]][[1]]

  num_class <- pm$general$num_class

  if (length(oblivious_trees) > 0) {
    pm$general$niter <- length(oblivious_trees)
    pm$general$tree_type <- "oblivious"
    pm$trees <- get_catboost_trees(
      oblivious_trees,
      float_features,
      cat_features,
      num_class
    )
  } else if (length(nonoblivious_trees) > 0) {
    pm$general$niter <- length(nonoblivious_trees)
    pm$general$tree_type <- "nonoblivious"
    pm$trees <- get_catboost_nonoblivious_trees(
      nonoblivious_trees,
      float_features,
      cat_features,
      num_class
    )
  } else {
    pm$general$niter <- 0
    pm$general$tree_type <- "none"
    pm$trees <- list()
  }

  as_parsed_model(pm)
}

#' Set categorical feature mappings for CatBoost model
#'
#' CatBoost stores categorical features as hash values internally. This function
#' establishes the mapping between hash values and category names by examining
#' a data frame with the same factor columns used during training.
#'
#' @details
#' This function is only needed when using raw CatBoost models (trained with
#' `catboost.train()`). When using parsnip/bonsai, categorical features are
#' handled automatically and this function is not required.
#'
#' @param parsed_model A parsed CatBoost model from `parse_model()`
#' @param model The original CatBoost model object
#' @param data A data frame containing factor columns matching the categorical
#'   features used in the model. The factor levels must match those from
#'   training.
#' @return The parsed model with category mappings added
#'
#' @examples
#' \dontrun{
#' # For raw CatBoost models with categorical features:
#' pm <- parse_model(catboost_model)
#' pm <- set_catboost_categories(pm, catboost_model, training_data)
#' tidypredict_fit(pm)
#'
#' # For parsnip/bonsai models, this is not needed:
#' # tidypredict_fit(parsnip_model_fit)  # works automatically
#' }
#' @export
set_catboost_categories <- function(parsed_model, model, data) {
  if (!inherits(parsed_model, "pm_catboost")) {
    cli::cli_abort(
      "{.arg parsed_model} must be a parsed CatBoost model."
    )
  }

  if (!inherits(model, "catboost.Model")) {
    cli::cli_abort(
      "{.arg model} must be a {.cls catboost.Model}, not {.obj_type_friendly {model}}."
    )
  }

  cat_features <- parsed_model$general$cat_features
  if (length(cat_features) == 0) {
    return(parsed_model)
  }

  # Build mapping for each categorical feature
  for (i in seq_along(cat_features)) {
    feat_info <- cat_features[[i]]
    feat_name <- feat_info$feature_id
    hash_values <- feat_info$hash_values

    if (!feat_name %in% names(data)) {
      cli::cli_abort(
        "Column {.val {feat_name}} not found in {.arg data}."
      )
    }

    col_data <- data[[feat_name]]
    if (!is.factor(col_data)) {
      cli::cli_abort(
        "Column {.val {feat_name}} must be a factor."
      )
    }

    categories <- levels(col_data)
    mapping <- build_catboost_hash_mapping(
      model,
      data,
      feat_name,
      categories,
      hash_values
    )
    parsed_model$general$cat_features[[i]]$hash_to_category <- mapping
  }

  parsed_model
}

# Hash mapping functions --------------------------------------------------
# Strategy: Train probe models to discover which hash belongs to which category.
# CatBoost stores categorical features as hashes internally, but doesn't expose
# the hash function. We identify mappings by training models where each category
# has a unique target value.

build_catboost_hash_mapping <- function(
  model,
  data,
  feat_name,
  categories,
  hash_values
) {
  all_hashes <- get_catboost_all_hashes(feat_name, categories)
  identified <- identify_catboost_hashes(feat_name, categories)
  complete_mapping <- fill_catboost_hash_mapping(
    identified$mapping,
    identified$hashes,
    all_hashes,
    categories
  )
  extract_catboost_hash_mapping(complete_mapping, hash_values)
}

get_catboost_all_hashes <- function(feat_name, categories) {
  n_cat <- length(categories)
  train_data <- make_catboost_probe_data(feat_name, categories, seq_len(n_cat))
  model <- train_catboost_probe_model(train_data, feat_name, n_cat, depth = 3L)
  extract_catboost_model_hashes(model)
}

identify_catboost_hashes <- function(feat_name, categories) {
  n_cat <- length(categories)
  mapping <- list()
  identified_hashes <- character(0)

  for (cat in categories) {
    target <- ifelse(categories == cat, 100, 0)
    train_data <- make_catboost_probe_data(feat_name, categories, target)
    model <- train_catboost_probe_model(
      train_data,
      feat_name,
      n_cat,
      depth = 1L
    )
    probe_hashes <- extract_catboost_model_hashes(model)

    if (length(probe_hashes) == 1) {
      hash_str <- as.character(probe_hashes)
      mapping[[hash_str]] <- cat
      identified_hashes <- c(identified_hashes, hash_str)
    }
  }

  list(mapping = mapping, hashes = identified_hashes)
}

fill_catboost_hash_mapping <- function(
  mapping,
  identified_hashes,
  all_hashes,
  categories
) {
  remaining_cats <- setdiff(categories, unlist(mapping))
  remaining_hashes <- setdiff(as.character(all_hashes), identified_hashes)

  if (length(remaining_cats) == 1 && length(remaining_hashes) == 1) {
    mapping[[remaining_hashes]] <- remaining_cats
  }

  mapping
}

extract_catboost_hash_mapping <- function(complete_mapping, hash_values) {
  result <- stats::setNames(
    rep(NA_character_, length(hash_values)),
    as.character(hash_values)
  )

  for (hash in hash_values) {
    hash_str <- as.character(hash)
    if (hash_str %in% names(complete_mapping)) {
      result[[hash_str]] <- complete_mapping[[hash_str]]
    }
  }

  as.list(result)
}

make_catboost_probe_data <- function(feat_name, categories, target) {
  train_data <- data.frame(
    cat_col = factor(categories, levels = categories),
    target = target
  )
  names(train_data)[1] <- feat_name
  train_data
}

train_catboost_probe_model <- function(train_data, feat_name, n_cat, depth) {
  pool <- catboost_catboost.load_pool(
    train_data[, feat_name, drop = FALSE],
    label = train_data$target
  )

  catboost_catboost.train(
    pool,
    params = list(
      iterations = if (depth == 1L) 10L else 100L,
      depth = depth,
      learning_rate = 1.0,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      one_hot_max_size = n_cat + 1L,
      min_data_in_leaf = 1L
    )
  )
}

extract_catboost_model_hashes <- function(model) {
  tmp_file <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_file), add = TRUE)
  catboost_catboost.save_model(model, tmp_file, file_format = "json")
  model_json <- jsonlite::fromJSON(tmp_file, simplifyVector = FALSE)
  unlist(model_json$features_info$categorical_features[[1]]$values)
}

process_catboost_trees <- function(
  trees,
  tree_fn,
  float_features,
  cat_features,
  num_class
) {
  if (num_class <= 1) {
    map(
      trees,
      tree_fn,
      float_features = float_features,
      cat_features = cat_features,
      class_idx = NULL,
      num_class = 1
    )
  } else {
    # Multiclass: create num_class virtual trees per iteration
    # Trees are stored round-robin by class: tree 1 class 0, tree 1 class 1, ...
    result <- list()
    for (i in seq_along(trees)) {
      for (class_idx in seq_len(num_class)) {
        result <- c(
          result,
          list(tree_fn(
            trees[[i]],
            float_features,
            cat_features = cat_features,
            class_idx = class_idx,
            num_class = num_class
          ))
        )
      }
    }
    result
  }
}

get_catboost_trees <- function(
  oblivious_trees,
  float_features,
  cat_features,
  num_class
) {
  process_catboost_trees(
    oblivious_trees,
    get_catboost_tree,
    float_features,
    cat_features,
    num_class
  )
}

# Non-oblivious tree parsing (Depthwise/Lossguide grow_policy) ----------------

get_catboost_nonoblivious_trees <- function(
  trees,
  float_features,
  cat_features,
  num_class
) {
  process_catboost_trees(
    trees,
    get_catboost_nonoblivious_tree,
    float_features,
    cat_features,
    num_class
  )
}

get_catboost_nonoblivious_tree <- function(
  tree_json,
  float_features,
  cat_features = NULL,
  class_idx = NULL,
  num_class = 1
) {
  # Recursively collect all leaf paths
  leaves <- collect_nonoblivious_leaves(
    tree_json,
    float_features,
    cat_features,
    path = list()
  )

  # Extract predictions for the appropriate class
  map(leaves, function(leaf) {
    pred_value <- leaf$value
    # For multiclass, leaf$value is a list with one element per class
    if (
      !is.null(class_idx) && num_class > 1 && length(pred_value) >= num_class
    ) {
      pred_value <- pred_value[[class_idx]]
    } else if (is.list(pred_value) && length(pred_value) == 1) {
      # Single class but still stored as list
      pred_value <- pred_value[[1]]
    }
    list(prediction = pred_value, path = leaf$path)
  })
}

collect_nonoblivious_leaves <- function(
  node,
  float_features,
  cat_features,
  path
) {
  # Check if this is a leaf node (has "value" field)
  if (!is.null(node$value)) {
    return(list(list(value = node$value, path = path)))
  }

  # Internal node: process split and recurse
  split <- node$split
  left_node <- node$left
  right_node <- node$right

  # Parse the split condition
  left_condition <- parse_nonoblivious_split(
    split,
    float_features,
    cat_features,
    go_left = TRUE
  )
  right_condition <- parse_nonoblivious_split(
    split,
    float_features,
    cat_features,
    go_left = FALSE
  )

  # Recurse into both branches
  left_leaves <- collect_nonoblivious_leaves(
    left_node,
    float_features,
    cat_features,
    path = c(path, list(left_condition))
  )
  right_leaves <- collect_nonoblivious_leaves(
    right_node,
    float_features,
    cat_features,
    path = c(path, list(right_condition))
  )

  c(left_leaves, right_leaves)
}

parse_nonoblivious_split <- function(
  split,
  float_features,
  cat_features,
  go_left
) {
  split_type <- split$split_type %||% "FloatFeature"

  if (split_type == "OneHotFeature") {
    # left = matches category, right = doesn't match
    op <- if (go_left) "equal" else "not-equal"
    make_categorical_split(split, cat_features, op)
  } else {
    # left = <= border, right = > border
    op <- if (go_left) "less-equal" else "more"
    make_float_split(split, float_features, op)
  }
}

# Split building helpers (shared by oblivious and non-oblivious) --------------

make_float_split <- function(split, float_features, op) {
  feature_index <- split$float_feature_index + 1L
  feature_info <- float_features[[feature_index]]
  feature_name <- feature_info$feature_id %||%
    paste0("feature_", feature_info$flat_feature_index)
  nan_treatment <- feature_info$nan_value_treatment %||% "AsIs"

  list(
    type = "conditional",
    col = feature_name,
    val = split$border,
    op = op,
    missing = get_catboost_missing(nan_treatment, op)
  )
}

make_categorical_split <- function(split, cat_features, op) {
  cat_feature_index <- split$cat_feature_index + 1L
  cat_feature_info <- cat_features[[cat_feature_index]]
  feature_name <- cat_feature_info$feature_id %||%
    paste0("cat_feature_", cat_feature_info$flat_feature_index)

  list(
    type = "categorical",
    col = feature_name,
    hash_value = split$value,
    op = op,
    missing = FALSE
  )
}

# Oblivious tree parsing ------------------------------------------------------

get_catboost_tree <- function(
  tree_json,
  float_features,
  cat_features = NULL,
  class_idx = NULL,
  num_class = 1
) {
  splits <- tree_json$splits
  leaf_values <- unlist(tree_json$leaf_values)
  n_splits <- length(splits)

  if (n_splits == 0) {
    return(make_catboost_stump(leaf_values, class_idx, num_class))
  }

  n_leaves <- 2^n_splits
  map(seq_len(n_leaves) - 1L, function(leaf_idx) {
    path <- lapply(seq_len(n_splits), function(split_idx) {
      parse_catboost_split(
        splits[[split_idx]],
        leaf_idx,
        split_idx,
        float_features,
        cat_features
      )
    })

    list(
      prediction = get_catboost_leaf_value(
        leaf_values,
        leaf_idx,
        class_idx,
        num_class
      ),
      path = path
    )
  })
}

make_catboost_stump <- function(leaf_values, class_idx, num_class) {
  if (!is.null(class_idx) && num_class > 1) {
    pred_value <- leaf_values[class_idx]
  } else {
    pred_value <- leaf_values[1]
  }
  list(list(prediction = pred_value, path = list()))
}

get_catboost_leaf_value <- function(
  leaf_values,
  leaf_idx,
  class_idx,
  num_class
) {
  # For multiclass: [leaf0_class0, leaf0_class1, ..., leaf1_class0, ...]
  if (!is.null(class_idx) && num_class > 1) {
    leaf_values[leaf_idx * num_class + class_idx]
  } else {
    leaf_values[leaf_idx + 1L]
  }
}

parse_catboost_split <- function(
  split,
  leaf_idx,
  split_idx,
  float_features,
  cat_features
) {
  split_type <- split$split_type %||% "FloatFeature"
  bit_val <- get_catboost_bit_value(leaf_idx, split_idx)

  if (split_type == "OneHotFeature") {
    parse_catboost_categorical_split(split, bit_val, cat_features)
  } else {
    parse_catboost_float_split(split, bit_val, float_features)
  }
}

get_catboost_bit_value <- function(leaf_idx, split_idx) {
  bit_pos <- split_idx - 1L
  bitwAnd(bitwShiftR(leaf_idx, bit_pos), 1L)
}

parse_catboost_categorical_split <- function(split, bit_val, cat_features) {
  # bit=0: doesn't match, bit=1: matches
  op <- if (bit_val == 0L) "not-equal" else "equal"
  make_categorical_split(split, cat_features, op)
}

parse_catboost_float_split <- function(split, bit_val, float_features) {
  # bit=1: > border (more), bit=0: <= border (less-equal)
  op <- if (bit_val == 1L) "more" else "less-equal"
  make_float_split(split, float_features, op)
}

get_catboost_missing <- function(nan_treatment, op) {
  # nan_treatment determines where NaN values go:
  # - "AsIs": No special treatment, NaN doesn't match anything -> FALSE
  # - "AsTrue": NaN goes left (where condition > border is TRUE) -> TRUE if op="more"
  # - "AsFalse": NaN goes right (where condition is FALSE) -> TRUE if op="less-equal"
  if (nan_treatment == "AsTrue") {
    return(op == "more")
  } else if (nan_treatment == "AsFalse") {
    return(op == "less-equal")
  } else {
    # "AsIs" or unknown
    return(FALSE)
  }
}

# Fit model -----------------------------------------------

#' @export
tidypredict_fit.catboost.Model <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_catboost(parsedmodel)
}

# Internal function for parsnip model_fit objects with CatBoost
# Called from tidymodels.R
tidypredict_fit_catboost_parsnip <- function(model) {
  cb_model <- model$fit
  parsedmodel <- parse_model(cb_model)

  has_cat_features <- length(parsedmodel$general$cat_features) > 0
  if (has_cat_features) {
    parsedmodel <- setup_catboost_parsnip_categories(parsedmodel, model)
  }

  build_fit_formula_catboost(parsedmodel)
}

setup_catboost_parsnip_categories <- function(parsedmodel, model) {
  has_xlevels <- !is.null(model$preproc$xlevels) &&
    length(model$preproc$xlevels) > 0

  if (!has_xlevels) {
    cli::cli_abort(
      c(
        "Model has categorical features but no factor level information.",
        "i" = "Ensure the model was fit with factor columns, not character columns."
      )
    )
  }

  mapping_data <- build_catboost_parsnip_mapping_data(
    model$preproc$xlevels,
    parsedmodel$general$cat_feature_names
  )

  if (ncol(mapping_data) > 0) {
    parsedmodel <- set_catboost_categories(parsedmodel, model$fit, mapping_data)
  }

  parsedmodel
}

build_catboost_parsnip_mapping_data <- function(xlevels, cat_feature_names) {
  mapping_data <- data.frame(row.names = 1)

  for (feat_name in cat_feature_names) {
    if (feat_name %in% names(xlevels)) {
      lvls <- xlevels[[feat_name]]
      mapping_data[[feat_name]] <- factor(lvls[1], levels = lvls)
    }
  }

  mapping_data
}

build_fit_formula_catboost <- function(parsedmodel) {
  n_trees <- length(parsedmodel$trees)

  if (n_trees == 0) {
    cli::cli_abort("Model has no trees.")
  }

  objective <- parsedmodel$general$params$objective %||% "RMSE"

  if (!objective %in% catboost_supported_objectives) {
    cli::cli_abort(
      c(
        "Unsupported objective: {.val {objective}}.",
        "i" = "Supported objectives: {.val {catboost_supported_objectives}}."
      )
    )
  }

  if (objective %in% catboost_multiclass_objectives) {
    return(build_fit_formula_catboost_multiclass(parsedmodel, objective))
  }

  tree_formulas <- map(
    seq_len(n_trees),
    function(i) expr(case_when(!!!get_catboost_case_tree(i, parsedmodel)))
  )
  f <- reduce_addition(tree_formulas)
  f <- apply_catboost_scale_bias(f, parsedmodel)

  if (objective %in% catboost_sigmoid_objectives) {
    f <- expr(1 / (1 + exp(-(!!f))))
  }

  f
}

apply_catboost_scale_bias <- function(formula, parsedmodel) {
  scale <- parsedmodel$general$scale %||% 1
  bias <- parsedmodel$general$bias %||% 0

  if (scale != 1) {
    formula <- expr(!!scale * (!!formula))
  }
  if (bias != 0) {
    formula <- expr(!!formula + !!bias)
  }
  formula
}

build_fit_formula_catboost_multiclass <- function(parsedmodel, objective) {
  n_trees <- length(parsedmodel$trees)
  num_class <- parsedmodel$general$num_class

  if (is.null(num_class) || num_class < 2) {
    cli::cli_abort("Multiclass model must have num_class >= 2.")
  }

  # Trees are stored round-robin by class: tree1_class0, tree1_class1, ...,
  # tree2_class0, tree2_class1, ...
  class_trees <- lapply(seq_len(num_class), function(class_idx) {
    which((seq_len(n_trees) - 1) %% num_class == (class_idx - 1))
  })

  raw_scores <- lapply(class_trees, build_catboost_tree_sum, parsedmodel)
  raw_scores <- lapply(raw_scores, apply_catboost_scale_bias, parsedmodel)

  if (objective == "MultiClass") {
    # Softmax: exp(raw_i) / sum(exp(raw_j))
    exp_raws <- map(raw_scores, ~ expr(exp(!!.x)))
    denom <- reduce_addition(exp_raws)
    result <- map(seq_len(num_class), function(i) {
      expr(exp(!!raw_scores[[i]]) / (!!denom))
    })
  } else {
    # MultiClassOneVsAll: sigmoid for each class independently
    result <- map(raw_scores, ~ expr(1 / (1 + exp(-(!!.x)))))
  }

  names(result) <- paste0("class_", seq_len(num_class) - 1)
  result
}

build_catboost_tree_sum <- function(tree_indices, parsedmodel) {
  if (length(tree_indices) == 0) {
    # nocov start
    cli::cli_abort("No trees found for tree indices.", .internal = TRUE)
    # nocov end
  }
  tree_formulas <- map(
    tree_indices,
    function(i) expr(case_when(!!!get_catboost_case_tree(i, parsedmodel)))
  )
  reduce_addition(tree_formulas)
}

get_catboost_case_tree <- function(tree_no, parsedmodel) {
  cat_mapping <- get_catboost_cat_mapping(parsedmodel)
  map(
    parsedmodel$trees[[tree_no]],
    function(leaf) get_catboost_case(leaf$path, leaf$prediction, cat_mapping)
  )
}

# Build a combined hash -> category mapping from all categorical features
get_catboost_cat_mapping <- function(parsedmodel) {
  cat_features <- parsedmodel$general$cat_features
  if (length(cat_features) == 0) {
    return(list())
  }

  mapping <- list()
  for (feat in cat_features) {
    if (!is.null(feat$hash_to_category)) {
      for (hash_str in names(feat$hash_to_category)) {
        mapping[[hash_str]] <- feat$hash_to_category[[hash_str]]
      }
    }
  }
  mapping
}

get_catboost_case <- function(path, prediction, cat_mapping = list()) {
  conditions <- map(path, get_catboost_case_fun, cat_mapping = cat_mapping)
  cl <- if (length(conditions) == 0) TRUE else reduce_and(conditions)
  expr(!!cl ~ !!prediction)
}

get_catboost_case_fun <- function(.x, cat_mapping = list()) {
  if (.x$type == "categorical") {
    return(get_catboost_categorical_condition(.x, cat_mapping))
  }

  if (.x$type != "conditional") {
    # nocov start
    cli::cli_abort(
      "CatBoost only supports conditional and categorical splits, not {.val {.x$type}}.",
      .internal = TRUE
    )
    # nocov end
  }

  build_catboost_comparison_expr(
    col = .x$col,
    op = .x$op,
    val = .x$val,
    include_missing = .x$missing
  )
}

build_catboost_comparison_expr <- function(col, op, val, include_missing) {
  col_name <- as.name(col)
  val <- as.numeric(val)

  base_expr <- switch(
    op,
    "less-equal" = expr(!!col_name <= !!val),
    "more" = expr(!!col_name > !!val),
    cli::cli_abort("Unknown operator: {.val {op}}.", .internal = TRUE) # nocov
  )

  if (include_missing) {
    expr((!!base_expr | is.na(!!col_name)))
  } else {
    base_expr
  }
}

get_catboost_categorical_condition <- function(.x, cat_mapping) {
  col_name <- as.name(.x$col)
  hash_value <- .x$hash_value
  hash_str <- as.character(hash_value)

  # Look up the category name from the hash
  category <- cat_mapping[[hash_str]]

  if (is.null(category) || is.na(category)) {
    cli::cli_abort(
      c(
        "No category mapping found for hash {.val {hash_value}}.",
        "i" = "For raw CatBoost models, use {.fn set_catboost_categories} to establish the mapping:",
        " " = "{.code pm <- set_catboost_categories(pm, model, training_data)}",
        "i" = "For parsnip/bonsai models, pass the model_fit object directly to {.fn tidypredict_fit}."
      )
    )
  }

  if (.x$op == "equal") {
    i <- expr(!!col_name == !!category)
  } else if (.x$op == "not-equal") {
    i <- expr(!!col_name != !!category)
  } else {
    # nocov start
    cli::cli_abort(
      "Unknown categorical operator: {.val {.x$op}}.",
      .internal = TRUE
    )
    # nocov end
  }

  i
}

# For {orbital}
#' Extract processed CatBoost trees
#'
#' For use in orbital package.
#' @param model A CatBoost model object
#' @keywords internal
#' @export
.extract_catboost_trees <- function(model) {
  if (!inherits(model, "catboost.Model")) {
    cli::cli_abort(
      "{.arg model} must be {.cls catboost.Model}, not {.obj_type_friendly {model}}."
    )
  }

  parsedmodel <- parse_model(model)

  map(
    seq_along(parsedmodel$trees),
    function(i) expr(case_when(!!!get_catboost_case_tree(i, parsedmodel)))
  )
}
