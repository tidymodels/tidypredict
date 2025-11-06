# parsnip ----------------------------------------------------------------

#' @export
tidypredict_fit._xgb.Booster <- function(model) {
  tidypredict_fit(model$fit)
}

#' @export
tidypredict_fit.model_fit <- function(model) {
  model <- glmnet_set_lambda(model)
  tidypredict_fit(model$fit)
}

#' @export
parse_model.model_fit <- function(model) {
  model <- glmnet_set_lambda(model)
  parse_model(model$fit)
}

# glmnet adjustment ------------------------------------------------------

glmnet_set_lambda <- function(model) {
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
