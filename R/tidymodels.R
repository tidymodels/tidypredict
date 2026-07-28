# parsnip ----------------------------------------------------------------

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
  if (inherits(model$fit, "sda")) {
    return(build_fit_formula_multinom(parse_model(model)))
  }

  tidypredict_fit(model$fit)
}

#' @export
parse_model.model_fit <- function(model) {
  model <- glmnet_set_lambda(model)

  if (inherits(model$fit, "sda")) {
    return(parse_model_sda(model$fit, sda_parsnip_vars(model)))
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
#' @export
tidy.pm_regression <- function(x, ...) {
  map_dfr(
    x$terms,
    ~ tibble::tibble(term = .x$label, estimate = .x$coef)
  )
}
