# Predict ---------------------------------------

# Parse model --------------------------------------

#' @export
parse_model.LiblineaR <- function(model) parse_model_liblinear(model)

# LiblineaR `type` codes fall into three families: logistic-regression
# classifiers (probabilities), SVM classifiers (a raw decision value), and
# support-vector regression (a linear prediction).
liblinear_lr_types <- c(0, 6, 7)
liblinear_svm_class_types <- c(1, 2, 3, 4, 5)
liblinear_regression_types <- c(11, 12, 13)

parse_model_liblinear <- function(model, call = rlang::caller_env()) {
  is_lr <- model$Type %in% liblinear_lr_types
  is_svm_class <- model$Type %in% liblinear_svm_class_types
  is_regression <- model$Type %in% liblinear_regression_types

  if (!is_lr && !is_svm_class && !is_regression) {
    cli::cli_abort(
      c(
        "This {.pkg LiblineaR} model {.arg type} is not supported.",
        i = "The model {.arg type} must be one of {.val {c(liblinear_lr_types,
          liblinear_svm_class_types, liblinear_regression_types)}}, not
          {model$Type}."
      ),
      call = call
    )
  }
  if ((is_lr || is_svm_class) && model$NbClass != 2) {
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

  if (is_lr) {
    # The decision value corresponds to the first class in `ClassNames`, while
    # tidypredict follows the glm convention of predicting the second factor
    # level. Flip the sign when they disagree.
    class_names <- as.character(model$ClassNames)
    levs <- levels(model$ClassNames)
    if (class_names[[1]] != levs[[2]]) {
      weights <- -weights
    }
  }

  pm <- list()
  pm$general$model <- "LiblineaR"
  pm$general$version <- 2
  pm$general$type <- "regression"
  if (is_lr) {
    # Logistic regression exposes probabilities: reuse the glm logit machinery
    # so `build_fit_formula()` emits `plogis(linear predictor)`.
    pm$general$is_glm <- 1
    pm$general$family <- "binomial"
    pm$general$link <- "logit"
  } else {
    # SVM regression predictions and SVM classification decision values are both
    # plain linear predictors.
    pm$general$is_glm <- 0
  }

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

# Output metadata ---------------------------------

# This is the model class the metadata generics exist for. The three `type`
# families produce three different things from an identically shaped result: a
# single linear expression. Logistic regression gives a probability, SVM
# classification gives a decision value whose sign picks the class, and SVR
# gives a plain number. Cutting an SVM decision value at 0.5 as though it were
# a probability misclassifies every row whose value lies between 0 and 0.5.

#' @export
tidypredict_output_type.LiblineaR <- function(x, ...) {
  rlang::check_dots_empty()

  if (x$Type %in% liblinear_lr_types) {
    return("prob")
  }
  if (x$Type %in% liblinear_svm_class_types) {
    return("decision")
  }
  if (x$Type %in% liblinear_regression_types) {
    return("numeric")
  }

  # parse_model_liblinear() rejects any other type, so reaching here means the
  # two lists have drifted apart.
  cli::cli_abort(
    "Unsupported {.pkg LiblineaR} model {.arg type} {.val {x$Type}}.",
    .internal = TRUE
  )
}

#' @export
tidypredict_outcome_levels.LiblineaR <- function(x, ...) {
  rlang::check_dots_empty()

  if (x$Type %in% liblinear_regression_types) {
    return(NULL)
  }

  # `ClassNames` is a factor whose element order reflects LiblineaR's internal
  # ordering, not the outcome's level order. `parse_model_liblinear()` already
  # normalises to the glm convention of predicting the second level, so report
  # the levels rather than the elements to stay consistent with it.
  levels(x$ClassNames)
}

#' @export
tidypredict_normalized.LiblineaR <- function(x, ...) {
  rlang::check_dots_empty()

  # Binary only, and a single expression, so there is no set of per-level
  # values to sum. `TRUE` would imply the caller can read probabilities for
  # every level straight off the result, which it cannot.
  NA
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
  newx <- as.matrix(df[, features, drop = FALSE])

  if (model$Type %in% liblinear_regression_types) {
    preds <- predict(model, newx)$predictions
  } else if (model$Type %in% liblinear_svm_class_types) {
    # SVM classifiers only expose a decision value, oriented to the first class.
    target <- as.character(model$ClassNames)[[1]]
    preds <- predict(model, newx, decisionValues = TRUE)$decisionValues[,
      target
    ]
  } else {
    target <- levels(model$ClassNames)[[2]]
    preds <- predict(model, newx, proba = TRUE)$probabilities[, target]
  }
  base <- data.frame(fit = as.vector(preds), row.names = NULL)

  te <- tidypredict_to_column(
    df,
    model,
    add_interval = FALSE,
    vars = c("fit_te", "upr_te", "lwr_te")
  )
  test_results_numeric(
    base$fit,
    te[, "fit_te"],
    threshold,
    model$call
  )
}
