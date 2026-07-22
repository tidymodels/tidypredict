# Predict ---------------------------------------

#' @export
tidypredict_fit.ksvm <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula(parsedmodel)
}

# Parse model --------------------------------------

#' @export
parse_model.ksvm <- function(model) parse_model_ksvm(model)

parse_model_ksvm <- function(model, call = rlang::caller_env()) {
  acceptable_formula(model)

  if (!inherits(kernlab::kernelf(model), "vanillakernel")) {
    cli::cli_abort(
      c(
        "Only linear {.pkg kernlab} SVM models are supported.",
        i = "The model must use the {.val vanilladot} (linear) kernel."
      ),
      call = call
    )
  }

  svm_type <- kernlab::type(model)
  is_classification <- svm_type %in% c("C-svc", "nu-svc", "C-bsvc")
  is_regression <- svm_type %in% c("eps-svr", "nu-svr", "eps-bsvr")
  if (!is_classification && !is_regression) {
    cli::cli_abort(
      c(
        "Only classification and regression {.pkg kernlab} SVM models are
        supported.",
        i = "The model {.arg type} is {.val {svm_type}}."
      ),
      call = call
    )
  }

  levs <- kernlab::lev(model)
  if (is_classification && length(levs) != 2) {
    cli::cli_abort(
      c(
        "Only binary classification {.pkg kernlab} SVM models are supported.",
        i = "This model has {length(levs)} classes."
      ),
      call = call
    )
  }

  # For a linear kernel the decision function collapses to a weighted sum of
  # the (scaled) predictors: `w = sum_i coef_i * support_vector_i`.
  sv <- kernlab::xmatrix(model)
  if (is.list(sv)) {
    sv <- sv[[1]]
  }
  cf <- kernlab::coef(model)
  if (is.list(cf)) {
    cf <- cf[[1]]
  }
  weights <- as.numeric(crossprod(sv, cf))
  names(weights) <- colnames(sv)
  bias <- as.numeric(kernlab::b(model))

  # kernlab scales predictors before fitting. Undo the scaling so the weights
  # apply to the predictors on their original scale. `x.scale` only covers the
  # columns that were scaled (e.g. dummy columns are left untouched).
  x_scale <- kernlab::scaling(model)$x.scale
  centers <- if (!is.null(x_scale)) x_scale[["scaled:center"]] else numeric(0)
  scales <- if (!is.null(x_scale)) x_scale[["scaled:scale"]] else numeric(0)

  linear <- weights
  intercept <- -bias
  for (nm in names(weights)) {
    if (nm %in% names(scales)) {
      linear[[nm]] <- weights[[nm]] / scales[[nm]]
      intercept <- intercept - weights[[nm]] * centers[[nm]] / scales[[nm]]
    }
  }

  pm <- list()
  pm$general$model <- "ksvm"
  pm$general$version <- 2
  pm$general$type <- "regression"

  if (is_regression) {
    # Regression targets are also scaled: `pred = y.scale * decision + y.center`.
    y_scale <- kernlab::scaling(model)$y.scale
    y_center <- if (!is.null(y_scale)) y_scale[["scaled:center"]] else 0
    y_sd <- if (!is.null(y_scale)) y_scale[["scaled:scale"]] else 1
    linear <- y_sd * linear
    intercept <- y_sd * intercept + y_center
    pm$general$is_glm <- 0
  } else {
    # Probabilities come from Platt scaling: `P = 1 / (1 + exp(A * dv + B))`,
    # which is the inverse logit of `-(A * dv + B)`. Fold `A` and `B` into the
    # linear predictor and reuse the glm logit machinery, following the glm
    # convention of predicting the second factor level.
    prob <- kernlab::prob.model(model)
    if (length(prob) == 0 || is.null(prob[[1]]$A)) {
      cli::cli_abort(
        c(
          "Classification {.pkg kernlab} SVM models require a probability
          model.",
          i = "Refit with {.code prob.model = TRUE}."
        ),
        call = call
      )
    }
    a <- prob[[1]]$A
    b <- prob[[1]]$B
    linear <- -a * linear
    intercept <- -a * intercept - b
    pm$general$is_glm <- 1
    pm$general$family <- "binomial"
    pm$general$link <- "logit"
  }

  terms_obj <- tryCatch(model@terms, error = function(cnd) NULL)
  vars <- if (!is.null(terms_obj)) {
    names(attr(terms_obj, "dataClasses"))
  } else {
    names(linear)
  }

  terms <- list(
    list(
      label = "(Intercept)",
      coef = intercept,
      is_intercept = 1,
      fields = list()
    )
  )
  for (feature in names(linear)) {
    terms[[length(terms) + 1]] <- list(
      label = feature,
      coef = unname(linear[[feature]]),
      is_intercept = 0,
      fields = parse_label_lm(feature, vars)
    )
  }

  pm$terms <- terms
  as_parsed_model(pm)
}

#' @export
acceptable_formula.ksvm <- function(model) {
  terms_obj <- tryCatch(model@terms, error = function(cnd) NULL)
  if (is.null(terms_obj)) {
    return(invisible())
  }
  accepted_funs <- c("~", "+", "-", "*", "(", ")", ":", "::", "factor", "stats")
  funs <- fun_calls(stats::formula(terms_obj))
  funs <- funs[!(funs %in% accepted_funs)]
  if (length(funs) > 0) {
    cli::cli_abort(
      c(
        x = "Functions inside the formula are not supported.",
        i = "Functions detected: {.val {funs}}.
            Use `dplyr` transformations to prepare the data."
      ),
      call. = FALSE
    )
  }
}

# Test --------------------------------------------

#' @export
tidypredict_test.ksvm <- function(
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

  if (kernlab::type(model) %in% c("C-svc", "nu-svc", "C-bsvc")) {
    target <- kernlab::lev(model)[[2]]
    preds <- kernlab::predict(model, df, type = "probabilities")[, target]
  } else {
    preds <- as.numeric(kernlab::predict(model, df))
  }
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
  results$model_call <- model@kcall
  results$raw_results <- raw_results
  results$message <- message
  results$alert <- alert
  structure(results, class = c("tidypredict_test", "list"))
}
