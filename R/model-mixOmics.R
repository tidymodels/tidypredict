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

tidypredict_fit_mixomics <- function(
  model,
  vars = character(0),
  preproc = NULL
) {
  parsedmodel <- parse_model_mixomics(model, vars, preproc)

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

parse_model_mixomics <- function(
  model,
  vars = character(0),
  preproc = NULL
) {
  coefs <- mixomics_coefs(model)
  labels <- rownames(coefs)
  outcomes <- colnames(coefs)
  fields <- preproc_fields(labels, preproc)

  if (inherits(model, "DA")) {
    # `predict()` returns one linear predictor per class, which {plsmod} turns
    # into class probabilities with a softmax.
    class_terms <- lapply(
      outcomes,
      function(outcome) {
        build_terms(coefs[, outcome], labels, vars, fields = fields)
      }
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
      pm$terms <- build_terms(coefs[, outcome], labels, vars, fields = fields)
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
  test_results_numeric(
    base$fit,
    te[, "fit_te"],
    threshold,
    model$call
  )
}

abort_mixomics_test <- function(type, call = rlang::caller_env()) {
  detail <- switch(
    type,
    multiclass = "multiclass predictions",
    multivariate = "multivariate outcomes"
  )
  abort_test_unsupported(
    "this {.pkg mixOmics} model",
    detail,
    call = call
  )
}
