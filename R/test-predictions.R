#' Tests base predict function against tidypredict
#'
#' Compares the results of predict() and tidypredict_to_column()
#' functions.
#'
#' @param model An R model or a tibble with a parsed model. It currently supports
#' lm(), glm() and randomForest() models.
#' @param df A data frame that contains all of the needed fields to run the prediction.
#' It defaults to the "model" data frame object inside the model object.
#' @param threshold The number that a given result difference, between predict() and
#' tidypredict_to_column() should not exceed.  For continuous predictions, the default
#' value is 0.000000000001 (1e-12), for categorical predictions, the default value is
#' 0.
#' @param include_intervals Switch to indicate if the prediction intervals should be
#' included in the test.  It defaults to FALSE.
#' @param max_rows The number of rows in the object passed in the df argument. Highly
#' recommended for large data sets.
#'
#' @examples
#'
#' library(dplyr)
#' df <- mutate(mtcars, cyl = paste0("cyl", cyl))
#' model <- lm(mpg ~ wt + cyl * disp, offset = am, data = df)
#' tidypredict_test(model)
#'
#' @export
tidypredict_test <- function(model, df = model$model, threshold = 0.000000000001,
                             include_intervals = FALSE, max_rows = NULL) {
  UseMethod("tidypredict_test")
}

#' @export
tidypredict_test.default <- function(model, df = model$model, threshold = 0.000000000001,
                                     include_intervals = FALSE, max_rows = NULL) {
  offset <- model$call$offset
  ismodels <- paste0(colnames(model$model), collapse = " ") == paste0(colnames(df), collapse = " ")

  if (!is.null(offset) && ismodels) {
    index <- colnames(df) == "(offset)"
    colnames(df) <- replace(colnames(df), index, as.character(offset))
  }

  interval <- if (include_intervals) "prediction" else "none"

  if (is.numeric(max_rows)) df <- head(df, max_rows)

  base <- predict(model, df, interval = interval, type = "response")

  if (!include_intervals) {
    base <- data.frame(fit = base, row.names = NULL)
  } else {
    base <- as.data.frame(base)
  }

  te <- df %>%
    tidypredict_to_column(
      model,
      add_interval = include_intervals,
      vars = c("fit_te", "upr_te", "lwr_te")
    ) %>%
    select(contains("_te"))

  raw_results <- bind_cols(base, te) %>%
    mutate(
      fit_diff = abs(.data$fit - .data$fit_te),
      fit_threshold = .data$fit_diff > threshold
    )

  if (include_intervals) {
    raw_results <- raw_results %>%
      mutate(
        lwr_diff = abs(.data$lwr - .data$lwr_te),
        upr_diff = abs(.data$upr - .data$upr_te),
        lwr_threshold = .data$lwr_diff > threshold,
        upr_threshold = .data$upr_diff > threshold
      )
  }

  raw_results <- raw_results %>%
    rowid_to_column()

  threshold_df <- raw_results %>%
    select(contains("_threshold")) %>%
    summarise_all(sum)

  alert <- any(threshold_df > 0)

  message <- paste0(
    "tidypredict test results\n",
    "Difference threshold: ", threshold,
    "\n"
  )

  if (alert) {
    difference <- raw_results %>%
      select(contains("_diff")) %>%
      summarise_all(max)

    message <- paste0(
      message,
      "\nFitted records above the threshold: ", threshold_df$fit_threshold,
      if (!is.null(threshold_df$lwr_threshold)) {
        "\nLower interval records above the threshold: "
      } , threshold_df$lwr_threshold,
      if (!is.null(threshold_df$upr_threshold)) {
        "\nUpper interval records above the threshold: "
      } , threshold_df$upr_threshold,
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
tidypredict_test.randomForest <- function(model, df = NULL, threshold = 0,
                                          include_intervals = FALSE, max_rows = NULL) {
  raw_results <- df %>%
    mutate(
      predict = as.character(predict(model, df)),
      tidypredict = !! tidypredict_fit(model)
    )

  differences <- raw_results %>%
    filter(.data$tidypredict != predict | is.na(.data$tidypredict))

  alert <- nrow(differences) > threshold

  message <- "tidypredict test results\n"

  if (!alert) {
    message <- paste0(
      message,
      "\nSuccess, test is under the set threshold of: ",
      threshold
    )
  }

  if (nrow(differences) > 0) {
    message <- paste0(
      message,
      "\nPredictions that did not match predict(): ", nrow(differences)
    )
  } else {
    message <- paste0(
      message,
      "\nAll predictions matched"
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
tidypredict_test.ranger <- function(model, df = NULL, threshold = 0,
                                    include_intervals = FALSE, max_rows = NULL) {
  raw_results <- df %>%
    as_tibble() %>%
    mutate(
      predict = as.character(predict(model, df)$predictions),
      tidypredict = !! tidypredict_fit(model)
    )

  differences <- raw_results %>%
    filter(.data$tidypredict != predict | is.na(.data$tidypredict))

  alert <- nrow(differences) > threshold

  message <- "tidypredict test results\n"

  if (!alert) {
    message <- paste0(
      message,
      "\nSuccess, test is under the set threshold of: ",
      threshold
    )
  }

  if (nrow(differences) > 0) {
    message <- paste0(
      message,
      "\nPredictions that did not match predict(): ", nrow(differences)
    )
  } else {
    message <- paste0(
      message,
      "\nAll predictions matched"
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

#' print method for test predictions results
#' @keywords internal
#' @export
print.tidypredict_test <- function(x, ...) {
  cat(x$message)
}

#' Knit print method for test predictions results
#' @keywords internal
#' @export
#'
knit_print.tidypredict_test <- function(x, ...) {
  x$message
}
