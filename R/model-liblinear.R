# Predict ---------------------------------------

#' @export
tidypredict_fit.LiblineaR <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula(parsedmodel)
}

# Parse model --------------------------------------

#' @export
parse_model.LiblineaR <- function(model) parse_model_liblinear(model)

parse_model_liblinear <- function(model, call = rlang::caller_env()) {
  if (!model$Type %in% c(0, 6, 7)) {
    cli::cli_abort(
      c(
        "Only logistic regression {.pkg LiblineaR} models are supported.",
        i = "The model {.arg type} must be one of 0, 6, or 7, not {model$Type}."
      ),
      call = call
    )
  }
  if (model$NbClass != 2) {
    cli::cli_abort(
      c(
        "Only binary classification {.pkg LiblineaR} models are supported.",
        i = "This model has {model$NbClass} classes."
      ),
      call = call
    )
  }

  weights <- model$W[1, ]
  names(weights) <- colnames(model$W)

  # The decision value corresponds to the first class in `ClassNames`, while
  # tidypredict follows the glm convention of predicting the second factor
  # level. Flip the sign when they disagree.
  class_names <- as.character(model$ClassNames)
  levs <- levels(model$ClassNames)
  if (class_names[[1]] != levs[[2]]) {
    weights <- -weights
  }

  pm <- list()
  pm$general$model <- "LiblineaR"
  pm$general$version <- 2
  pm$general$type <- "regression"
  pm$general$is_glm <- 1
  pm$general$family <- "binomial"
  pm$general$link <- "logit"

  intercept <- 0
  if ("Bias" %in% names(weights) && model$Bias > 0) {
    intercept <- unname(weights[["Bias"]]) * model$Bias
  }

  terms <- list(
    list(
      label = "(Intercept)",
      coef = intercept,
      is_intercept = 1,
      fields = list()
    )
  )
  features <- setdiff(names(weights), "Bias")
  for (feature in features) {
    terms[[length(terms) + 1]] <- list(
      label = feature,
      coef = unname(weights[[feature]]),
      is_intercept = 0,
      fields = list(list(type = "ordinary", col = feature))
    )
  }

  pm$terms <- terms
  as_parsed_model(pm)
}

# Test --------------------------------------------

#' @export
tidypredict_test.LiblineaR <- function(
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

  features <- setdiff(colnames(model$W), "Bias")
  preds <- predict(
    model,
    as.matrix(df[, features, drop = FALSE]),
    proba = TRUE
  )$probabilities
  target <- levels(model$ClassNames)[[2]]
  base <- data.frame(fit = as.vector(preds[, target]), row.names = NULL)

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
