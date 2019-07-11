# parsnip ----------------------------------------------------------------

#' @export
tidypredict_fit._xgb.Booster <- function(model) {
  tidypredict_fit(model$fit)
}

#' @export
tidypredict_fit.model_fit <- function(model) {
  tidypredict_fit(model$fit)
}

#' @export
parse_model.model_fit <- function(model) {
  parse_model(model$fit)
}

# broom ------------------------------------------------------------------

#' @importFrom generics tidy
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
    ~tibble::tibble(term = .x$label, estimate = .x$coef)
    )
}
