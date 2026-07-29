# The PLS models in {mixOmics} all predict with the same regression
# coefficients: `predict()` collapses the components down to a single
# `B.hat` matrix and applies it to the centered and scaled predictors. The
# sparse variants only differ in that unselected predictors get a zero
# coefficient, and the discriminant variants only differ in that the outcome
# is a dummy matrix, one column per class.
mixomics_classes <- c("mixo_pls", "mixo_spls", "mixo_plsda", "mixo_splsda")

# Predict ---------------------------------------

#' @export
tidypredict_fit.mixo_pls <- function(model) tidypredict_fit_mixomics(model)

#' @export
tidypredict_fit.mixo_spls <- function(model) tidypredict_fit_mixomics(model)

#' @export
tidypredict_fit.mixo_plsda <- function(model) tidypredict_fit_mixomics(model)

#' @export
tidypredict_fit.mixo_splsda <- function(model) tidypredict_fit_mixomics(model)

tidypredict_fit_mixomics <- function(model, vars = character(0)) {
  parsedmodel <- parse_model_mixomics(model, vars)

  if (inherits(parsedmodel, "pm_multiclass_regression")) {
    return(build_fit_formula_multinom(parsedmodel))
  }

  if (inherits(parsedmodel, "parsed_model")) {
    return(build_fit_formula(parsedmodel))
  }

  # Multivariate outcomes return one formula per response column
  map(parsedmodel, build_fit_formula)
}

# Parse model --------------------------------------

#' @export
parse_model.mixo_pls <- function(model) parse_model_mixomics(model)

#' @export
parse_model.mixo_spls <- function(model) parse_model_mixomics(model)

#' @export
parse_model.mixo_plsda <- function(model) parse_model_mixomics(model)

#' @export
parse_model.mixo_splsda <- function(model) parse_model_mixomics(model)

parse_model_mixomics <- function(model, vars = character(0)) {
  coefs <- mixomics_coefs(model)
  labels <- rownames(coefs)
  outcomes <- colnames(coefs)

  if (inherits(model, "DA")) {
    # `predict()` returns one linear predictor per class, which {plsmod} turns
    # into class probabilities with a softmax.
    class_terms <- lapply(
      outcomes,
      function(outcome) mixomics_terms(coefs[, outcome], labels, vars)
    )

    pm <- list()
    pm$general$model <- class(model)[[1]]
    pm$general$version <- 2
    pm$general$type <- "multiclass_regression"
    pm$general$family <- "multinomial"
    pm$general$ncomp <- model$ncomp
    pm$classes <- outcomes
    pm$class_terms <- class_terms

    return(as_parsed_model(pm))
  }

  pms <- lapply(
    outcomes,
    function(outcome) {
      pm <- list()
      pm$general$model <- class(model)[[1]]
      pm$general$version <- 2
      pm$general$type <- "regression"
      pm$general$is_glm <- 0
      pm$general$ncomp <- model$ncomp
      pm$terms <- mixomics_terms(coefs[, outcome], labels, vars)
      as_parsed_model(pm)
    }
  )

  if (length(pms) == 1) {
    return(pms[[1]])
  }

  set_names(pms, outcomes)
}

# Reproduces the coefficients that `predict.mixo_pls()` applies to `newdata`,
# but folded back onto the original scale of the predictors so that no
# centering or scaling is needed at prediction time. The matrix has one column
# per response, with the intercept as its first row.
mixomics_coefs <- function(model, call = rlang::caller_env()) {
  x <- model$X
  y <- if (inherits(model, "DA")) model$ind.mat else model$Y

  if (anyNA(x) || anyNA(y)) {
    cli::cli_abort(
      "Models fit on data with missing values are not supported.",
      call = call
    )
  }

  variates <- model$variates$X
  loadings <- model$loadings$X
  p_mat <- crossprod(x, variates)
  c_mat <- crossprod(y, variates)
  b_hat <- loadings %*% solve(crossprod(p_mat, loadings)) %*% t(c_mat)

  center_x <- attr(x, "scaled:center") %||% rep(0, ncol(x))
  scale_x <- attr(x, "scaled:scale") %||% rep(1, ncol(x))
  center_y <- attr(y, "scaled:center") %||% rep(0, ncol(y))
  scale_y <- attr(y, "scaled:scale") %||% rep(1, ncol(y))

  coefs <- sweep(b_hat / scale_x, 2, scale_y, "*")
  intercept <- center_y - colSums(coefs * center_x)

  coefs <- rbind(intercept, coefs)
  rownames(coefs) <- c("(Intercept)", colnames(x))
  colnames(coefs) <- colnames(y)
  coefs
}

mixomics_terms <- function(values, labels, vars) {
  map2(as.numeric(values), labels, function(value, label) {
    list(
      label = label,
      coef = value,
      is_intercept = as.integer(label == "(Intercept)"),
      fields = parse_label_lm(label, vars)
    )
  })
}

# {mixOmics} is fit from a numeric matrix, so a bare model has no way of
# knowing that a column such as `gear4` is a dummy variable. A parsnip fit does
# keep the formula around, which lets the dummy columns be expressed in terms
# of the original factors.
mixomics_parsnip_vars <- function(model) {
  terms <- model$preproc$terms
  if (is.null(terms)) {
    return(character(0))
  }
  names(attr(terms, "dataClasses"))
}

# Test ---------------------------------------------

#' @export
tidypredict_test.mixo_pls <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  mixomics_test(model, df, threshold, max_rows)
}

#' @export
tidypredict_test.mixo_spls <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  mixomics_test(model, df, threshold, max_rows)
}

#' @export
tidypredict_test.mixo_plsda <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  abort_mixomics_test("multiclass")
}

#' @export
tidypredict_test.mixo_splsda <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  abort_mixomics_test("multiclass")
}

mixomics_test <- function(
  model,
  df,
  threshold,
  max_rows,
  call = rlang::caller_env()
) {
  if (ncol(model$Y) > 1) {
    abort_mixomics_test("multivariate", call = call)
  }

  if (is.numeric(max_rows)) {
    df <- head(df, max_rows)
  }

  newdata <- as.matrix(df[, colnames(model$X), drop = FALSE])
  preds <- predict(model, newdata)$predict[, 1, model$ncomp]
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

  alert <- sum(raw_results$fit_threshold) > 0

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
  results$model_call <- model$call
  results$raw_results <- raw_results
  results$message <- message
  results$alert <- alert
  structure(results, class = c("tidypredict_test", "list"))
}

abort_mixomics_test <- function(type, call = rlang::caller_env()) {
  detail <- switch(
    type,
    multiclass = "Use {.fn tidypredict_fit} directly for multiclass predictions.",
    multivariate = "Use {.fn tidypredict_fit} directly for multivariate outcomes."
  )
  cli::cli_abort(
    c(
      "{.fn tidypredict_test} does not support this {.pkg mixOmics} model.",
      "i" = detail
    ),
    call = call
  )
}
