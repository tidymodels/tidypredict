# Predict ---------------------------------------

#' @export
tidypredict_fit.sda <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_multinom(parsedmodel)
}

# Parse model --------------------------------------

#' @export
parse_model.sda <- function(model) parse_model_sda(model)

parse_model_sda <- function(model, vars = character(0)) {
  # `predict.sda()` computes `Xtest %*% t(beta) + alpha` and takes the softmax of
  # the result, so each class is already a plain linear predictor. Only the
  # features that survived shrinkage appear in `beta`.
  classes <- names(model$alpha)
  labels <- colnames(model$beta)

  class_terms <- lapply(seq_along(classes), function(i) {
    sda_terms(
      c(model$alpha[[i]], model$beta[i, ]),
      c("(Intercept)", labels),
      vars
    )
  })

  pm <- list()
  pm$general$model <- "sda"
  pm$general$version <- 2
  pm$general$type <- "multiclass_regression"
  pm$general$family <- "multinomial"
  pm$classes <- classes
  pm$class_terms <- class_terms

  as_parsed_model(pm)
}

sda_terms <- function(values, labels, vars) {
  map2(as.numeric(values), labels, function(value, label) {
    list(
      label = label,
      coef = value,
      is_intercept = as.integer(label == "(Intercept)"),
      fields = parse_label_lm(label, vars)
    )
  })
}

# `sda()` is fit from a numeric matrix, so a bare model has no way of knowing
# that a feature such as `gear4` is a dummy variable. A parsnip fit does keep
# the formula around, which lets the dummy columns be expressed in terms of the
# original factors.
sda_parsnip_vars <- function(model) {
  terms <- model$preproc$terms
  if (is.null(terms)) {
    return(character(0))
  }
  names(attr(terms, "dataClasses"))
}

# Test ---------------------------------------------

#' @export
tidypredict_test.sda <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  cli::cli_abort(
    c(
      "{.fn tidypredict_test} does not support {.fn sda::sda} models.",
      "i" = "Use {.fn tidypredict_fit} directly for multiclass predictions."
    )
  )
}
