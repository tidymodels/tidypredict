# Predict ---------------------------------------

#' @export
tidypredict_fit.multinom <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_multinom(parsedmodel)
}

# Parse model --------------------------------------

#' @export
parse_model.multinom <- function(model) {
  acceptable_formula(model)

  classes <- model$lev
  vars <- names(attr(model$terms, "dataClasses"))

  coefs <- stats::coef(model)
  # Binary outcomes return a named vector of coefficients for the second level
  if (!is.matrix(coefs)) {
    coefs <- matrix(
      coefs,
      nrow = 1,
      dimnames = list(classes[-1], names(coefs))
    )
  }

  # The first level is the reference class, its linear predictor is 0
  class_terms <- c(
    list(multinom_reference_terms()),
    lapply(
      classes[-1],
      \(cl) multinom_terms(coefs[cl, ], colnames(coefs), vars)
    )
  )

  pm <- list()
  pm$general$model <- "multinom"
  pm$general$version <- 2
  pm$general$type <- "multiclass_regression"
  pm$general$family <- "multinomial"
  pm$classes <- classes
  pm$class_terms <- class_terms

  as_parsed_model(pm)
}

multinom_reference_terms <- function() {
  list(list(
    label = "(Intercept)",
    coef = 0,
    is_intercept = 1,
    fields = list(list(type = "ordinary", col = "(Intercept)"))
  ))
}

multinom_terms <- function(values, labels, vars) {
  map2(as.numeric(values), labels, function(value, label) {
    list(
      label = label,
      coef = value,
      is_intercept = as.integer(label == "(Intercept)"),
      fields = parse_label_lm(label, vars)
    )
  })
}

#' @export
acceptable_formula.multinom <- function(model) acceptable_lm(model)

# Test ---------------------------------------------

#' @export
tidypredict_test.multinom <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  cli::cli_abort(
    c(
      "{.fn tidypredict_test} does not support {.fn nnet::multinom} models.",
      "i" = "Use {.fn tidypredict_fit} directly for multiclass predictions."
    )
  )
}
