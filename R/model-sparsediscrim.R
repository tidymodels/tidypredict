# The regularized LDA methods in {sparsediscrim} all share the same shape: one
# mean vector and prior per class, plus a single pooled covariance (or its
# inverse) shared by every class. They only differ in how that covariance is
# regularized.
sparsediscrim_classes <- c(
  "lda_diag",
  "lda_shrink_mean",
  "lda_shrink_cov",
  "lda_emp_bayes_eigen"
)

# Predict ---------------------------------------

# Parse model --------------------------------------

#' @export
parse_model.lda_diag <- function(model) parse_model_sparsediscrim(model)

#' @export
parse_model.lda_shrink_mean <- function(model) parse_model_sparsediscrim(model)

#' @export
parse_model.lda_shrink_cov <- function(model) parse_model_sparsediscrim(model)

#' @export
parse_model.lda_emp_bayes_eigen <- function(model) {
  parse_model_sparsediscrim(model)
}

parse_model_sparsediscrim <- function(model, vars = character(0)) {
  # The `x`/`y` interface keeps no formula, in which case every column of the
  # model matrix is used as-is.
  if (!is.null(model$.terms)) {
    acceptable_formula(model)
    vars <- names(attr(model$.terms, "dataClasses"))
  }

  # The posterior probabilities are the softmax of
  # `log(prior) - 0.5 * (x - xbar)' P (x - xbar)`, where `P` is the shared
  # precision matrix. The quadratic `x' P x` term is identical for every class,
  # so it cancels in the softmax and what is left is one linear predictor per
  # class.
  precision <- sparsediscrim_precision(model)
  labels <- model$col_names

  class_terms <- lapply(model$est, function(class_est) {
    xbar <- class_est$xbar
    coefs <- as.vector(precision %*% xbar)
    intercept <- log(class_est$prior) - 0.5 * sum(xbar * coefs)

    build_terms(
      c(intercept, coefs),
      c("(Intercept)", labels),
      vars
    )
  })

  new_multiclass_parsed_model(
    sparsediscrim_method(model),
    model$groups,
    unname(class_terms)
  )
}

sparsediscrim_method <- function(model) {
  intersect(class(model), sparsediscrim_classes)[[1]]
}

sparsediscrim_precision <- function(model) {
  method <- sparsediscrim_method(model)

  if (method == "lda_emp_bayes_eigen") {
    return(model$cov_inv)
  }

  variances <- switch(
    method,
    lda_shrink_cov = model$var_shrink,
    model$var_pool
  )
  diag(1 / variances, nrow = length(variances))
}


#' @export
acceptable_formula.lda_diag <- function(model) acceptable_sparsediscrim(model)

#' @export
acceptable_formula.lda_shrink_mean <- function(model) {
  acceptable_sparsediscrim(model)
}

#' @export
acceptable_formula.lda_shrink_cov <- function(model) {
  acceptable_sparsediscrim(model)
}

#' @export
acceptable_formula.lda_emp_bayes_eigen <- function(model) {
  acceptable_sparsediscrim(model)
}

# The fitted object stores its formula in `.terms` rather than `terms`, so the
# shared `lm` checks need a little help to find it.
acceptable_sparsediscrim <- function(model) {
  if (is.null(model$.terms)) {
    return(invisible(NULL))
  }
  acceptable_lm(list(terms = model$.terms))
}


# Test ---------------------------------------------

#' @export
tidypredict_test.lda_diag <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  abort_sparsediscrim_test()
}

#' @export
tidypredict_test.lda_shrink_mean <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  abort_sparsediscrim_test()
}

#' @export
tidypredict_test.lda_shrink_cov <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  abort_sparsediscrim_test()
}

#' @export
tidypredict_test.lda_emp_bayes_eigen <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  abort_sparsediscrim_test()
}

abort_sparsediscrim_test <- function(call = rlang::caller_env()) {
  abort_test_unsupported("{.pkg sparsediscrim} models", call = call)
}
