# Model parser -------------------------------------

#' @export
parse_model.catboost.Model <- function(model) {
  pm <- list()
  pm$general$model <- "catboost.Model"
  pm$general$type <- "catboost"
  pm$general$version <- 1

  tmp_file <- tempfile(fileext = ".json")
  on.exit(unlink(tmp_file), add = TRUE)
  catboost::catboost.save_model(model, tmp_file, file_format = "json")
  model_json <- jsonlite::fromJSON(tmp_file, simplifyVector = FALSE)

  pm$general$params <- list()
  loss_fn <- model_json$model_info$params$loss_function
  if (!is.null(loss_fn)) {
    if (is.list(loss_fn) && !is.null(loss_fn$type)) {
      pm$general$params$objective <- loss_fn$type
    } else {
      pm$general$params$objective <- loss_fn
    }
  }

  features_info <- model_json$features_info
  float_features <- features_info$float_features
  feature_names <- vapply(
    float_features,
    function(f) f$feature_id %||% paste0("feature_", f$flat_feature_index),
    character(1)
  )
  pm$general$feature_names <- feature_names
  pm$general$nfeatures <- length(feature_names)

  oblivious_trees <- model_json$oblivious_trees
  pm$general$niter <- length(oblivious_trees)

  pm$trees <- get_catboost_trees(oblivious_trees, float_features)

  as_parsed_model(pm)
}

get_catboost_trees <- function(oblivious_trees, float_features) {
  map(oblivious_trees, get_catboost_tree, float_features = float_features)
}

get_catboost_tree <- function(tree_json, float_features) {
  splits <- tree_json$splits
  leaf_values <- unlist(tree_json$leaf_values)

  n_splits <- length(splits)

  # Handle stump (no splits, single leaf)
  if (n_splits == 0) {
    return(list(list(
      prediction = leaf_values[1],
      path = list()
    )))
  }

  n_leaves <- 2^n_splits

  map(seq_len(n_leaves) - 1L, function(leaf_idx) {
    path <- vector("list", n_splits)

    for (split_idx in seq_len(n_splits)) {
      split <- splits[[split_idx]]
      # Bit position in leaf index (0-indexed from LSB)
      bit_pos <- split_idx - 1L
      # Get bit value: 1 means left (>), 0 means right (<=)
      bit_val <- bitwAnd(bitwShiftR(leaf_idx, bit_pos), 1L)

      # Get feature info
      feature_index <- split$float_feature_index + 1L # Convert to 1-indexed
      feature_info <- float_features[[feature_index]]
      feature_name <- feature_info$feature_id %||%
        paste0("feature_", feature_info$flat_feature_index)

      # Get border value
      border <- split$border

      # Determine operator based on bit value
      # CatBoost: > border goes left (bit=1), <= border goes right (bit=0)
      if (bit_val == 1L) {
        op <- "more"
      } else {
        op <- "less-equal"
      }

      # Handle NaN treatment
      nan_treatment <- feature_info$nan_value_treatment %||% "AsIs"
      missing_with_us <- get_catboost_missing(nan_treatment, op)

      path[[split_idx]] <- list(
        type = "conditional",
        col = feature_name,
        val = border,
        op = op,
        missing = missing_with_us
      )
    }

    list(
      prediction = leaf_values[leaf_idx + 1L], # 1-indexed in R
      path = path
    )
  })
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
