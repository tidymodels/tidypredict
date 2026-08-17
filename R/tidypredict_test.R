#' Tests base predict function against tidypredict
#'
#' Compares the results of predict() and tidypredict_to_column()
#' functions.
#'
#' @param model An R model or a list with a parsed model.
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
#' @param xg_df The prediction matrix used to obtain the model's own
#' predictions. Required for XGBoost, LightGBM and CatBoost models, which
#' cannot predict from a data frame. Pass an `xgb.DMatrix` for XGBoost and a
#' numeric matrix for LightGBM and CatBoost. It defaults to NULL.
#'
#' @returns A list of test results comparing `predict()` and
#'   `tidypredict_to_column()`, including the maximum difference and whether it
#'   stays within `threshold`.
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
tidypredict_test.cforest <- function(
  model,
  df = model$data,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  df <- maybe_head(df, max_rows)

  base <- predict(model, newdata = df, type = "response")
  te <- tidypredict_to_column(
    df,
    model,
    add_interval = FALSE,
    vars = c("fit_te", "upr_te", "lwr_te")
  )

  test_results_numeric(base, te[, "fit_te"], threshold, model$call)
}

#' @export
tidypredict_test.ObliqueForest <- function(
  model,
  df = model$data,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  df <- maybe_head(df, max_rows)

  base <- predict(model, new_data = df)
  te <- tidypredict_to_column(
    df,
    model,
    add_interval = FALSE,
    vars = c("fit_te", "upr_te", "lwr_te")
  )

  test_results_numeric(base, te[, "fit_te"], threshold, model$call)
}

#' @export
tidypredict_test.C5.0 <- function(
  model,
  df = NULL,
  threshold = 0,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  df <- maybe_head(df, max_rows)

  base <- predict(model, df, type = "class")
  te <- rlang::eval_tidy(tidypredict_fit(model), df)

  test_results_class(base, te, model$call)
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

  df <- maybe_head(df, max_rows)

  preds <- predict(model, df, interval = interval, type = "response")
  te <- tidypredict_to_column(
    df,
    model,
    add_interval = include_intervals,
    vars = c("fit_te", "upr_te", "lwr_te")
  )

  if (!include_intervals) {
    return(test_results_numeric(preds, te[, "fit_te"], threshold, model$call))
  }

  preds <- as.data.frame(preds)
  test_results_numeric(
    preds$fit,
    te[, "fit_te"],
    threshold,
    model$call,
    intervals = list(
      lwr = preds$lwr,
      upr = preds$upr,
      lwr_te = te[, "lwr_te"],
      upr_te = te[, "upr_te"]
    )
  )
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
  if (inherits(model, "multnet")) {
    cli::cli_abort(
      c(
        "{.fn tidypredict_test} does not support multinomial glmnet models.",
        "i" = "Use {.fn tidypredict_fit} directly for multiclass predictions."
      )
    )
  }

  df <- maybe_head(df, max_rows)

  base <- predict(model, as.matrix(df), type = "response")
  te <- tidypredict_to_column(
    df,
    model,
    add_interval = FALSE,
    vars = c("fit_te", "upr_te", "lwr_te")
  )

  test_results_numeric(base, te[, "fit_te"], threshold, model$call)
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
  # If this is also a model_fit (parsnip), delegate to that method
  if (inherits(model, "model_fit")) {
    return(tidypredict_test.model_fit(
      model = model,
      df = df,
      threshold = threshold,
      include_intervals = include_intervals,
      max_rows = max_rows,
      xg_df = xg_df
    ))
  }
  xgb_booster(
    model = model,
    df = df,
    threshold = threshold,
    include_intervals = include_intervals,
    max_rows = max_rows,
    xg_df = xg_df %||% df
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
  df <- maybe_head(df, max_rows)

  base <- predict(model, xg_df)
  te <- tidypredict_to_column(
    df,
    model,
    add_interval = FALSE,
    vars = c("fit_te", "upr_te", "lwr_te")
  )

  test_results_numeric(base, te[, "fit_te"], threshold, model$call)
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

  test_results_numeric(base, te[, "fit_te"], threshold)
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

  test_results_numeric(base, te[, "fit_te"], threshold)
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

  test_results_multiclass(
    base,
    te_matrix,
    threshold,
    classes = seq_len(num_class) - 1,
    model_call = model$call
  )
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
