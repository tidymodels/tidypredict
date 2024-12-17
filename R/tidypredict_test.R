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
tidypredict_test <- function(model, df = model$model, threshold = 0.000000000001,
                             include_intervals = FALSE, max_rows = NULL, xg_df = NULL) {
  UseMethod("tidypredict_test")
}

#' @export
tidypredict_test.party <- function(model, df = model$data, threshold = 0.000000000001,
                                   include_intervals = FALSE, max_rows = NULL, xg_df = NULL) {
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
tidypredict_test.default <- function(model, df = model$model, threshold = 0.000000000001,
                                     include_intervals = FALSE, max_rows = NULL, xg_df = NULL) {
  tidypredict_test_default(
    model = model,
    df = df,
    threshold = threshold,
    include_intervals = include_intervals,
    max_rows = max_rows,
    xg_df = xg_df
  )
}
tidypredict_test_default <- function(model, df = model$model, threshold = 0.000000000001,
                                     include_intervals = FALSE, max_rows = NULL, xg_df = NULL) {
  offset <- model$call$offset
  ismodels <- paste0(colnames(model$model), collapse = " ") == paste0(colnames(df), collapse = " ")

  if (!is.null(offset) && ismodels) {
    index <- colnames(df) == "(offset)"
    colnames(df) <- replace(colnames(df), index, as.character(offset))
  }

  interval <- if (include_intervals) "prediction" else "none"

  if (is.numeric(max_rows)) df <- head(df, max_rows)

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
    "Difference threshold: ", threshold,
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
      "\nFitted records above the threshold: ", threshold_df$fit_threshold,
      if (!is.null(threshold_df$lwr_threshold)) {
        "\nLower interval records above the threshold: "
      }, threshold_df$lwr_threshold,
      if (!is.null(threshold_df$upr_threshold)) {
        "\nUpper interval records above the threshold: "
      }, threshold_df$upr_threshold,
      "\n\nFit max  difference:", difference$upr_diff,
      "\nLower max difference:", difference$lwr_diff,
      "\nUpper max difference:", difference$fit_diff
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
tidypredict_test.xgb.Booster <- function(model, df = model$model, threshold = 0.000000000001,
                                         include_intervals = FALSE, max_rows = NULL, xg_df = NULL) {
  xgb_booster(
    model = model,
    df = df,
    threshold = threshold,
    include_intervals = include_intervals,
    max_rows = max_rows,
    xg_df = xg_df
  )
}

#' @export
tidypredict_test._xgb.Booster <- function(model, df = model$model, threshold = 0.000000000001,
                                          include_intervals = FALSE, max_rows = NULL, xg_df = NULL) {
  xgb_booster(
    model = model,
    df = df,
    threshold = threshold,
    include_intervals = include_intervals,
    max_rows = max_rows,
    xg_df = df
  )
}

xgb_booster <- function(model, df = model$model, threshold = 0.000000000001,
                        include_intervals = FALSE, max_rows = NULL, xg_df = NULL) {
  if (is.numeric(max_rows)) df <- head(df, max_rows)
  base <- predict(model, xg_df)
  if ("model_fit" %in% class(model)) base <- base$.pred
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
    "Difference threshold: ", threshold,
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
      "\nFitted records above the threshold: ", threshold_df$fit_threshold,
      if (!is.null(threshold_df$lwr_threshold)) {
        "\nLower interval records above the threshold: "
      }, threshold_df$lwr_threshold,
      if (!is.null(threshold_df$upr_threshold)) {
        "\nUpper interval records above the threshold: "
      }, threshold_df$upr_threshold,
      "\n\nFit max  difference:", difference$upr_diff,
      "\nLower max difference:", difference$lwr_diff,
      "\nUpper max difference:", difference$fit_diff
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
tidypredict_test.model_fit <- function(model, df = model$model, threshold = 0.000000000001,
                                       include_intervals = FALSE, max_rows = NULL, xg_df = NULL) {
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
tidypredict_test.randomForest <- function(model, df = NULL, threshold = 0,
                                          include_intervals = FALSE, max_rows = NULL, xg_df = NULL) {
  cli::cli_abort("tidypredict_test does not support randomForest models")
}

#' @export
tidypredict_test.ranger <- function(model, df = NULL, threshold = 0,
                                    include_intervals = FALSE, max_rows = NULL, xg_df = NULL) {
  cli::cli_abort("tidypredict_test does not support ranger models")
}
setOldClass(c("tidypredict_test", "list"))

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
