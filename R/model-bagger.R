# A {baguette} `bagger()` object is an ensemble of models fit on bootstrap
# samples of the training data, stored as parsnip model fits in `model_df`.
# Only the CART (`rpart`) and C5.0 base models are supported here. Regression
# predictions are the mean of the individual tree predictions, and
# classification predictions are the class with the largest mean class
# probability.

# Parse model --------------------------------------

#' @export
parse_model.bagger <- function(model) {
  trees <- bagger_base_fits(model)

  pm <- list()
  pm$general$model <- "bagger"
  pm$general$type <- "tree"
  pm$general$version <- 3

  classes <- bagger_classes(model)
  if (is.null(classes)) {
    pm$tree_info_list <- map(trees, rpart_tree_info_full)
  } else {
    pm$general$classes <- classes
    pm$tree_info_list <- map(trees, bagger_classprob_tree_info)
  }

  as_parsed_model(pm)
}

# Pull the base model fits out of the ensemble, erroring for base models that
# are not supported
bagger_base_fits <- function(model, call = rlang::caller_env()) {
  base_model <- model$base_model[[1]]
  if (!base_model %in% c("CART", "C5.0")) {
    cli::cli_abort(
      c(
        "Only {.val CART} and {.val C5.0} bagged models are supported, not {.val {base_model}}.",
        i = "Fit the model with {.code base_model = \"CART\"} or {.code base_model = \"C5.0\"}."
      ),
      call = call
    )
  }

  fits <- map(model$model_df$model, function(x) x$fit)

  if (identical(base_model, "C5.0")) {
    for (fit in fits) {
      c50_check_supported(fit, call = call)
    }
  }

  fits
}

# The class probability trees of a single base model fit
bagger_classprob_tree_info <- function(fit, call = rlang::caller_env()) {
  if (inherits(fit, "C5.0")) {
    c50_classprob_tree_info(fit, call = call)
  } else {
    rpart_classprob_tree_info(fit)
  }
}

# `NULL` for regression models, the outcome levels for classification models
bagger_classes <- function(model) {
  model$model_df$model[[1]]$lvl
}

# Fit model -----------------------------------------------

#' @export
tidypredict_fit.bagger <- function(model, ...) {
  bagger_build_formula(parse_model(model))
}

bagger_build_formula <- function(parsedmodel) {
  tree_info_list <- parsedmodel$tree_info_list
  classes <- parsedmodel$general$classes

  if (is.null(classes)) {
    return(bagger_mean_tree(tree_info_list))
  }

  probs <- map(
    seq_along(classes),
    function(i) bagger_mean_tree(map(tree_info_list, function(x) x[[i]]))
  )
  bagger_class_case_when(probs, classes)
}

# Average the per-tree expressions of a single quantity
bagger_mean_tree <- function(tree_info_list) {
  tree_exprs <- map(tree_info_list, generate_nested_case_when_tree)
  expr_division(reduce_addition(tree_exprs), length(tree_exprs))
}

# Return the class with the largest probability, with ties going to the class
# that comes first, matching `which.max()`
bagger_class_case_when <- function(probs, classes) {
  n <- length(classes)
  if (n == 1) {
    return(classes[[1]])
  }

  args <- list()
  for (i in seq_len(n - 1L)) {
    comparisons <- map(
      seq.int(i + 1L, n),
      function(j) expr(!!probs[[i]] >= !!probs[[j]])
    )
    condition <- combine_path_conditions(comparisons)
    args[[i]] <- expr(!!condition ~ !!classes[[i]])
  }
  args$.default <- classes[[n]]
  rlang::call2("case_when", !!!args)
}

# Test model -----------------------------------------------

#' @export
tidypredict_test.bagger <- function(
  model,
  df = NULL,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  if (is.numeric(max_rows)) {
    df <- head(df, max_rows)
  }

  is_class <- !is.null(bagger_classes(model))
  if (is_class) {
    threshold <- 0
    base <- as.character(predict(model, df, type = "class")$.pred_class)
  } else {
    base <- predict(model, df)$.pred
  }

  te <- rlang::eval_tidy(tidypredict_fit(model), df)

  raw_results <- data.frame(
    rowid = seq_along(base),
    fit = base,
    fit_te = if (is_class) as.character(te) else te
  )
  raw_results$fit_diff <- if (is_class) {
    as.numeric(raw_results$fit != raw_results$fit_te)
  } else {
    raw_results$fit - raw_results$fit_te
  }
  raw_results$fit_threshold <- abs(raw_results$fit_diff) > threshold

  alert <- any(raw_results$fit_threshold)

  message <- paste0(
    "tidypredict test results\n",
    "Difference threshold: ",
    threshold,
    "\n"
  )

  if (alert) {
    message <- paste0(
      message,
      "\nFitted records above the threshold: ",
      sum(raw_results$fit_threshold),
      "\n\nMax difference: ",
      max(abs(raw_results$fit_diff))
    )
  } else {
    message <- paste0(
      message,
      "\n All results are within the difference threshold"
    )
  }

  results <- list()
  results$raw_results <- raw_results
  results$message <- message
  results$alert <- alert
  structure(results, class = c("tidypredict_test", "list"))
}

# For {orbital} -----------------------------------------------

#' Extract regression trees for bagger models
#'
#' For use in orbital package.
#' @param model A bagger model object (regression)
#' @keywords internal
#' @export
.extract_bagger_trees <- function(model) {
  bagger_check_model(model)

  if (!is.null(bagger_classes(model))) {
    cli::cli_abort(
      c(
        "Classification models are not supported.",
        i = "Use {.fn .extract_bagger_classprob} for classification models."
      )
    )
  }

  map(bagger_base_fits(model), tidypredict_fit)
}

#' Extract class probability trees for bagger models
#'
#' Returns one list of per-tree expressions for each outcome level. For use in
#' orbital package.
#' @param model A bagger model object (classification)
#' @keywords internal
#' @export
.extract_bagger_classprob <- function(model) {
  bagger_check_model(model)

  classes <- bagger_classes(model)
  if (is.null(classes)) {
    cli::cli_abort(
      c(
        "Model is not a classification model.",
        i = "Use {.fn .extract_bagger_trees} for regression models."
      )
    )
  }

  tree_info_list <- map(bagger_base_fits(model), bagger_classprob_tree_info)

  res <- map(
    seq_along(classes),
    function(i) {
      map(tree_info_list, function(x) generate_nested_case_when_tree(x[[i]]))
    }
  )
  names(res) <- classes
  res
}

bagger_check_model <- function(model, call = rlang::caller_env()) {
  if (!inherits(model, "bagger")) {
    cli::cli_abort(
      "{.arg model} must be {.cls bagger}, not {.obj_type_friendly {model}}.",
      call = call
    )
  }
  invisible(model)
}
