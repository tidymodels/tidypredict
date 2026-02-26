#' Tests base predict function against tidypredict
#'
#' Compares the results of predict() and tidypredict_to_column()
#' functions.
#'
#' @param model An R model or a list with a parsed model. It currently supports
#' lm(), glm() and randomForest() models.
#' @param df A data frame that contains all of the needed fields to run the prediction.
#' It defaults to the "model" data frame object inside the model object.
#' @param threshold The number that a given result difference, between predict() and
#' tidypredict_to_column() should not exceed. For continuous predictions, the default
#' value is 0.000000000001 (1e-12), and for categorical predictions, the default value is
#' 0.
#' @param include_intervals Switch to indicate if the prediction intervals should be
#' included in the test. It defaults to FALSE.
#' @param max_rows The number of rows in the object passed in the df argument. Highly
#' recommended for large data sets.
#' @param xg_df A xgb.DMatrix object, required only for XGBoost models. It defaults to
#' NULL
#' recommended for large data sets.
#'
#' @examples
#'
#' model <- lm(mpg ~ wt + cyl * disp, offset = am, data = mtcars)
#' tidypredict_test(model)
#' @export
tidypredict_test <- function(
  model,
  df = model$model,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  UseMethod("tidypredict_test")
}

#' @export
tidypredict_test.party <- function(
  model,
  df = model$data,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  tidypredict_test_default(
    model = model,
    df = df,
    threshold = threshold,
    include_intervals = include_intervals,
    max_rows = max_rows,
    xg_df = xg_df
  )
}

#' @export
tidypredict_test.default <- function(
  model,
  df = model$model,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  tidypredict_test_default(
    model = model,
    df = df,
    threshold = threshold,
    include_intervals = include_intervals,
    max_rows = max_rows,
    xg_df = xg_df
  )
}
tidypredict_test_default <- function(
  model,
  df = model$model,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  offset <- model$call$offset
  ismodels <- paste0(colnames(model$model), collapse = " ") ==
    paste0(colnames(df), collapse = " ")

  if (!is.null(offset) && ismodels) {
    index <- colnames(df) == "(offset)"
    colnames(df) <- replace(colnames(df), index, as.character(offset))
  }

  interval <- "none"
  if (include_intervals) {
    interval <- "prediction"
  }

  if (is.numeric(max_rows)) {
    df <- head(df, max_rows)
  }

  preds <- predict(model, df, interval = interval, type = "response")

  if (!include_intervals) {
    base <- data.frame(fit = as.vector(preds), row.names = NULL)
  } else {
    base <- as.data.frame(preds)
  }

  te <- tidypredict_to_column(
    df,
    model,
    add_interval = include_intervals,
    vars = c("fit_te", "upr_te", "lwr_te")
  )
  if (include_intervals) {
    te <- te[, c("fit_te", "upr_te", "lwr_te")]
  } else {
    te <- data.frame(fit_te = te[, "fit_te"])
  }

  raw_results <- cbind(base, te)
  raw_results$fit_diff <- raw_results$fit - raw_results$fit_te
  raw_results$fit_threshold <- abs(raw_results$fit_diff) > threshold

  if (include_intervals) {
    raw_results$lwr_diff <- abs(raw_results$lwr - raw_results$lwr_te)
    raw_results$upr_diff <- abs(raw_results$upr - raw_results$upr_te)
    raw_results$lwr_threshold <- raw_results$lwr_diff > threshold
    raw_results$upr_threshold <- raw_results$upr_diff > threshold
  }

  rowid <- seq_len(nrow(raw_results))
  raw_results <- cbind(data.frame(rowid), raw_results)

  threshold_df <- data.frame(fit_threshold = sum(raw_results$fit_threshold))
  if (include_intervals) {
    threshold_df$lwr_threshold <- sum(raw_results$lwr_threshold)
    threshold_df$upr_threshold <- sum(raw_results$upr_threshold)
  }

  alert <- any(threshold_df > 0)

  message <- paste0(
    "tidypredict test results\n",
    "Difference threshold: ",
    threshold,
    "\n"
  )

  if (alert) {
    difference <- data.frame(fit_diff = max(raw_results$fit_diff))
    if (include_intervals) {
      difference$lwr_diff <- max(raw_results$lwr_diff)
      difference$upr_diff <- max(raw_results$upr_diff)
    }
    message <- paste0(
      message,
      "\nFitted records above the threshold: ",
      threshold_df$fit_threshold,
      if (!is.null(threshold_df$lwr_threshold)) {
        "\nLower interval records above the threshold: "
      },
      threshold_df$lwr_threshold,
      if (!is.null(threshold_df$upr_threshold)) {
        "\nUpper interval records above the threshold: "
      },
      threshold_df$upr_threshold,
      "\n\nFit max  difference:",
      difference$upr_diff,
      "\nLower max difference:",
      difference$lwr_diff,
      "\nUpper max difference:",
      difference$fit_diff
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

#' @export
tidypredict_test.glmnet <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  if (is.numeric(max_rows)) {
    df <- head(df, max_rows) # nocov
  }

  preds <- predict(model, as.matrix(df), type = "response")
  base <- data.frame(fit = as.vector(preds), row.names = NULL)

  te <- tidypredict_to_column(
    df,
    model,
    add_interval = FALSE,
    vars = c("fit_te", "upr_te", "lwr_te")
  )
  te <- data.frame(fit_te = te[, "fit_te"])

  raw_results <- cbind(base, te)
  raw_results$fit_diff <- raw_results$fit - raw_results$fit_te
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
    difference <- data.frame(fit_diff = max(raw_results$fit_diff))
    message <- paste0(
      message,
      "\nFitted records above the threshold: ",
      threshold_df$fit_threshold,
      "\n\nMax difference: ",
      difference$fit_diff
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

#' @export
tidypredict_test.xgb.Booster <- function(
  model,
  df = model$model,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  xgb_booster(
    model = model,
    df = df,
    threshold = threshold,
    include_intervals = include_intervals,
    max_rows = max_rows,
    xg_df = xg_df
  )
}

# Legacy method for old xgboost models with underscore prefix class
#' @export
tidypredict_test._xgb.Booster <- function(
  model,
  df = model$model,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  xgb_booster(
    model = model,
    df = df,
    threshold = threshold,
    include_intervals = include_intervals,
    max_rows = max_rows,
    xg_df = df
  )
}

xgb_booster <- function(
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
  base <- predict(model, xg_df)
  te <- tidypredict_to_column(
    df,
    model,
    add_interval = FALSE,
    vars = c("fit_te", "upr_te", "lwr_te")
  )
  raw_results <- cbind(base, te)
  raw_results$fit_diff <- raw_results$base - raw_results$fit_te
  raw_results$fit_threshold <- raw_results$fit_diff > threshold

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
    difference <- data.frame(fit_diff = max(raw_results$fit_diff))
    message <- paste0(
      message,
      "\nFitted records above the threshold: ",
      threshold_df$fit_threshold,
      "\n\nMax difference: ",
      difference$fit_diff
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

setOldClass(c("tidypredict_test", "list"))

#' @export
tidypredict_test.lgb.Booster <- function(
  model,
  df = NULL,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  lgb_booster(
    model = model,
    df = df,
    threshold = threshold,
    include_intervals = include_intervals,
    max_rows = max_rows,
    lgb_df = xg_df
  )
}

lgb_booster <- function(
  model,
  df = NULL,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  lgb_df = NULL
) {
  if (is.null(lgb_df)) {
    cli::cli_abort(
      c(
        "LightGBM models require a matrix for predictions.",
        "i" = "Pass the prediction matrix via the {.arg xg_df} argument."
      )
    )
  }

  if (is.null(df)) {
    df <- as.data.frame(lgb_df)
  }

  if (is.numeric(max_rows)) {
    df <- head(df, max_rows)
    lgb_df <- lgb_df[seq_len(max_rows), , drop = FALSE]
  }

  base <- predict(model, lgb_df)

  # Check for multiclass (returns matrix)
  if (is.matrix(base)) {
    cli::cli_abort(
      c(
        "tidypredict_test does not support multiclass LightGBM models.",
        "i" = "Use tidypredict_fit() directly for multiclass predictions."
      )
    )
  }

  te <- tidypredict_to_column(
    df,
    model,
    add_interval = FALSE,
    vars = c("fit_te", "upr_te", "lwr_te")
  )

  raw_results <- cbind(data.frame(base = base), te)
  raw_results$fit_diff <- abs(raw_results$base - raw_results$fit_te)
  raw_results$fit_threshold <- raw_results$fit_diff > threshold

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
    difference <- data.frame(fit_diff = max(raw_results$fit_diff))
    message <- paste0(
      message,
      "\nFitted records above the threshold: ",
      threshold_df$fit_threshold,
      "\n\nMax difference: ",
      difference$fit_diff
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

#' @export
tidypredict_test.model_fit <- function(
  model,
  df = model$model,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  tidypredict_test(
    model = model$fit,
    df = df,
    threshold = threshold,
    include_intervals = include_intervals,
    max_rows = max_rows,
    xg_df = xg_df
  )
}

#' @export
tidypredict_test.catboost.Model <- function(
  model,
  df = NULL,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  catboost_model(
    model = model,
    df = df,
    threshold = threshold,
    include_intervals = include_intervals,
    max_rows = max_rows,
    cb_df = xg_df
  )
}

catboost_model <- function(
  model,
  df = NULL,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  cb_df = NULL
) {
  if (is.null(cb_df)) {
    cli::cli_abort(
      c(
        "CatBoost models require a matrix for predictions.",
        "i" = "Pass the prediction matrix via the {.arg xg_df} argument."
      )
    )
  }

  if (is.null(df)) {
    df <- as.data.frame(cb_df)
  }

  if (is.numeric(max_rows)) {
    df <- head(df, max_rows)
    cb_df <- cb_df[seq_len(max_rows), , drop = FALSE]
  }

  # Create pool for prediction
  pool <- catboost_catboost.load_pool(cb_df)

  # Detect objective type
  pm <- parse_model(model)
  objective <- pm$general$params$objective
  is_binary <- !is.null(objective) &&
    objective %in% c("Logloss", "CrossEntropy")
  is_multiclass <- !is.null(objective) &&
    objective %in% c("MultiClass", "MultiClassOneVsAll")

  if (is_multiclass) {
    return(catboost_model_multiclass(
      model,
      df,
      threshold,
      pool,
      pm,
      objective
    ))
  }

  if (is_binary) {
    base <- catboost_catboost.predict(
      model,
      pool,
      prediction_type = "Probability"
    )
  } else {
    base <- catboost_catboost.predict(model, pool)
  }

  te <- tidypredict_to_column(
    df,
    model,
    add_interval = FALSE,
    vars = c("fit_te", "upr_te", "lwr_te")
  )

  raw_results <- cbind(data.frame(base = base), te)
  raw_results$fit_diff <- abs(raw_results$base - raw_results$fit_te)
  raw_results$fit_threshold <- raw_results$fit_diff > threshold

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
    difference <- data.frame(fit_diff = max(raw_results$fit_diff))
    message <- paste0(
      message,
      "\nFitted records above the threshold: ",
      threshold_df$fit_threshold,
      "\n\nMax difference: ",
      difference$fit_diff
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

catboost_model_multiclass <- function(
  model,
  df,
  threshold,
  pool,
  pm,
  objective
) {
  num_class <- pm$general$num_class

  # Get native predictions as matrix
  base <- catboost_catboost.predict(
    model,
    pool,
    prediction_type = "Probability"
  )

  # Get tidypredict formulas (returns a list)
  formulas <- tidypredict_fit(model)

  # Evaluate each class formula
  te_preds <- lapply(formulas, function(f) rlang::eval_tidy(f, df))
  te_matrix <- do.call(cbind, te_preds)

  # Compare predictions
  diffs <- abs(base - te_matrix)
  alert <- any(diffs > threshold)

  message <- paste0(
    "tidypredict test results (multiclass: ",
    num_class,
    " classes)\n",
    "Difference threshold: ",
    threshold,
    "\n"
  )

  message <- paste0(
    message,
    "\n All results are within the difference threshold"
  )

  # Build raw_results for consistency
  raw_results <- data.frame(rowid = seq_len(nrow(df)))
  for (i in seq_len(num_class)) {
    raw_results[[paste0("base_class_", i - 1)]] <- base[, i]
    raw_results[[paste0("te_class_", i - 1)]] <- te_matrix[, i]
    raw_results[[paste0("diff_class_", i - 1)]] <- diffs[, i]
  }
  raw_results$max_diff <- apply(diffs, 1, max)
  raw_results$fit_threshold <- raw_results$max_diff > threshold

  results <- list()
  results$raw_results <- raw_results
  results$message <- message
  results$alert <- alert
  structure(results, class = c("tidypredict_test", "list"))
}

#' print method for test predictions results
#' @keywords internal
#' @export
print.tidypredict_test <- function(x, ...) {
  cat(x$message)
}

#' Knit print method for test predictions results
#' @keywords internal
#' @export
knit_print.tidypredict_test <- function(x, ...) {
  x$message
}
