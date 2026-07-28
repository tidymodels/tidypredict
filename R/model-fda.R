# Predict ---------------------------------------

#' @export
tidypredict_fit.fda <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_multinom(parsedmodel)
}

# Parse model --------------------------------------

#' @export
parse_model.fda <- function(model) parse_model_fda(model)

parse_model_fda <- function(model, call = rlang::caller_env()) {
  if (inherits(model, "mda")) {
    cli::cli_abort(
      c(
        "{.fn mda::mda} models are not supported.",
        i = "Only {.fn mda::fda} discriminant models are supported."
      ),
      call = call
    )
  }
  acceptable_formula(model)

  coefs <- fda_regression_coefs(model$fit, call = call)
  classes <- rownames(model$means)
  prior <- as.vector(model$prior)
  vars <- names(attr(model$terms, "dataClasses"))

  # `predict.fda()` maps the regression fit onto the discriminant variates,
  # scales them, and then turns the squared distance to each class centroid into
  # a posterior probability. The squared length of the projected point is shared
  # by every class, so it cancels in the normalization and what is left is one
  # linear predictor per class, combined with a softmax.
  dimen <- min(length(classes) - 1, ncol(model$means))
  values <- model$values[seq_len(dimen)]
  scaling <- sqrt(1 - values) * sqrt(values)
  # `theta.mod` is what `predict.fda()` reaches for as `object$theta`.
  projection <- model$theta.mod[, seq_len(dimen), drop = FALSE] %*%
    diag(1 / scaling, dimen)
  centroids <- model$means[, seq_len(dimen), drop = FALSE]

  betas <- coefs$slopes %*% projection %*% t(centroids)
  intercepts <- as.vector(coefs$intercepts %*% projection %*% t(centroids)) -
    0.5 * rowSums(centroids^2) +
    log(prior)

  labels <- rownames(coefs$slopes)
  class_terms <- lapply(seq_along(classes), function(i) {
    fda_terms(
      c(intercepts[[i]], betas[, i]),
      c("(Intercept)", labels),
      vars
    )
  })

  pm <- list()
  pm$general$model <- "fda"
  pm$general$version <- 2
  pm$general$type <- "multiclass_regression"
  pm$general$family <- "multinomial"
  pm$classes <- classes
  pm$class_terms <- class_terms

  as_parsed_model(pm)
}

# The regression fit inside an `fda` object has to be linear in the predictors
# for the posterior probabilities to collapse into a softmax over linear
# predictors. `polyreg()` (the default) qualifies at `degree = 1`, and
# `gen.ridge()` centers the predictors before applying its coefficients.
fda_regression_coefs <- function(fit, call = rlang::caller_env()) {
  if (inherits(fit, "polyreg")) {
    if (fit$degree != 1) {
      cli::cli_abort(
        c(
          "Only {.fn mda::polyreg} fits with {.code degree = 1} are supported.",
          i = "This model was fit with {.code degree = {fit$degree}}."
        ),
        call = call
      )
    }
    coefs <- fit$coefficients
    return(list(
      intercepts = coefs[1, ],
      slopes = coefs[-1, , drop = FALSE]
    ))
  }

  if (inherits(fit, "gen.ridge")) {
    slopes <- fit$coefficients
    rownames(slopes) <- names(fit$xmeans)
    return(list(
      intercepts = -as.vector(fit$xmeans %*% slopes),
      slopes = slopes
    ))
  }

  cli::cli_abort(
    c(
      "The {.arg method} used to fit this {.pkg mda} model is not supported.",
      i = "Only {.fn mda::polyreg} and {.fn mda::gen.ridge} are supported, not
        {.cls {class(fit)}}."
    ),
    call = call
  )
}

fda_terms <- function(values, labels, vars) {
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
acceptable_formula.fda <- function(model) acceptable_lm(model)

# Test ---------------------------------------------

#' @export
tidypredict_test.fda <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  cli::cli_abort(
    c(
      "{.fn tidypredict_test} does not support {.fn mda::fda} models.",
      "i" = "Use {.fn tidypredict_fit} directly for multiclass predictions."
    )
  )
}
