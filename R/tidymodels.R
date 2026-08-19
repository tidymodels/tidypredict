# parsnip ----------------------------------------------------------------

# Models fit from a numeric model matrix have no way of knowing that a column
# such as `gear4` is a dummy variable. A parsnip fit does keep the formula
# around, which lets the dummy columns be expressed in terms of the original
# factors.
parsnip_vars <- function(model) {
  terms <- model$preproc$terms
  if (is.null(terms)) {
    return(character(0))
  }
  names(attr(terms, "dataClasses"))
}

# The exact decomposition of a set of model matrix column names, taken from the
# term structure a parsnip fit kept alongside the model. `NULL` when there is
# none, in which case `build_terms()` falls back to reading the names.
preproc_fields <- function(labels, preproc) {
  term_fields(labels, preproc$terms, xlevels = preproc$xlevels)
}

# `sda()`, `sparsediscrim` and `mixOmics` are fit from a numeric matrix, so they
# carry none of the contrast checks the formula models do, and none of them can
# be handed an ordered factor directly. parsnip builds the model matrix for the
# user, which is the one way `contr.poly` reaches these parsers: its columns are
# named `f.L` and `f.Q`, and the level recovered from either matches no row.
#
# The check belongs to the parsnip dispatch rather than to `preproc_fields()`,
# because a `sparsediscrim` formula fit supplies a `preproc` of its own, and its
# own expansion names every column after a level whether the factor is ordered
# or not (#393).
acceptable_preproc <- function(model) {
  acceptable_ordered(list(terms = model$preproc$terms))
}

#' @export
tidypredict_fit._xgb.Booster <- function(model) {
  tidypredict_fit(model$fit)
}

#' @export
tidypredict_fit.model_fit <- function(model) {
  model <- glmnet_set_lambda(model)

  # Special handling for CatBoost models with categorical features
  if (inherits(model$fit, "catboost.Model")) {
    return(tidypredict_fit_catboost_parsnip(model))
  }

  # `sda()` only sees the model matrix, so the formula is needed to map dummy
  # columns back onto the original factors
  if (inherits(model$fit, c("sda", sparsediscrim_classes))) {
    return(build_fit_formula_multinom(parse_model(model)))
  }

  if (inherits(model$fit, mixomics_classes)) {
    acceptable_preproc(model)
  }

  # {mixOmics} models only see the model matrix, so the formula is needed to map
  # dummy columns back onto the original factors
  if (inherits(model$fit, mixomics_classes)) {
    return(tidypredict_fit_mixomics(
      model$fit,
      parsnip_vars(model),
      model$preproc
    ))
  }

  # `mlp()` models need the extra softmax that parsnip applies to the class
  # probabilities. `multinom_reg()` also fits an object that inherits from
  # `"nnet"`, but its predictions are not post processed.
  if (inherits(model$fit, "nnet") && !inherits(model$fit, "multinom")) {
    return(tidypredict_fit_nnet_parsnip(model))
  }

  tidypredict_fit(model$fit)
}

#' @export
parse_model.model_fit <- function(model) {
  model <- glmnet_set_lambda(model)

  if (inherits(model$fit, c("sda", sparsediscrim_classes, mixomics_classes))) {
    acceptable_preproc(model)
  }

  if (inherits(model$fit, "sda")) {
    return(parse_model_sda(model$fit, parsnip_vars(model), model$preproc))
  }

  if (inherits(model$fit, sparsediscrim_classes)) {
    return(parse_model_sparsediscrim(
      model$fit,
      parsnip_vars(model),
      model$preproc
    ))
  }

  if (inherits(model$fit, mixomics_classes)) {
    return(parse_model_mixomics(
      model$fit,
      parsnip_vars(model),
      model$preproc
    ))
  }

  parse_model(model$fit)
}

# glmnet adjustment ------------------------------------------------------

glmnet_set_lambda <- function(model) {
  if (inherits(model$fit, "multnet")) {
    penalty <- model$spec$args$penalty
    coefs <- stats::coef(model$fit, s = penalty)

    classes <- names(coefs)
    a0 <- vapply(coefs, function(x) x["(Intercept)", 1], numeric(1))
    model$fit$a0 <- matrix(a0, ncol = 1, dimnames = list(classes, NULL))
    model$fit$beta <- lapply(coefs, function(x) {
      x["(Intercept)" != rownames(x), , drop = FALSE]
    })
    model$fit$lambda <- penalty
    return(model)
  }
  if (inherits(model$fit, "glmnet")) {
    penalty <- model$spec$args$penalty
    coef <- glmnet::predict.glmnet(
      model$fit,
      s = penalty,
      type = "coefficients"
    )

    if ("(Intercept)" %in% rownames(coef)) {
      model$fit$a0 <- coef["(Intercept)", ]
      coef <- coef["(Intercept)" != rownames(coef), ]
    }
    model$fit$lambda <- penalty
    model$fit$beta <- coef
  }
  model
}

# broom ------------------------------------------------------------------

#' @export
generics::tidy

#' Tidy the parsed model results
#'
#' @param x A parsed_model object
#' @param ...  Reserved for future use
#'
#' @returns A tibble with one row per term, containing the `term` name and its
#'   `estimate`.
#'
#' @examples
#' pm <- parse_model(lm(mpg ~ wt, data = mtcars))
#' tidy(pm)
#'
#' @export
tidy.pm_regression <- function(x, ...) {
  map_dfr(
    x$terms,
    ~ tibble::tibble(term = .x$label, estimate = .x$coef)
  )
}
