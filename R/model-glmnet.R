# Predict ---------------------------------------

#' @export
tidypredict_fit.glmnet <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula(parsedmodel)
}

# Parse model --------------------------------------

#' @export
parse_model.glmnet <- function(model) {
  parse_model_glmnet(model)
}

parse_model_glmnet <- function(model, call = rlang::caller_env()) {
  if (length(model$lambda) != 1) {
    cli::cli_abort(
      "{.fn tidypredict_fit} requires that there are only 1 penalty seleted."
    )
  }

  names <- rownames(coefs)
  values <- as.vector(coefs)

  terms <- map2(values, names, \(value, name) {
    if (value == 0) {
      return(NULL)
    }
    list(
      label = name,
      coef = value,
      is_intercept = as.integer(name == "(Intercept)"),
      fields = list(list(type = "ordinary", col = name))
    )
  })

  terms <- purrr::discard(terms, is.null)

  pm <- list()
  pm$general$model <- class(model)[[2]]
  pm$general$version <- 1
  pm$general$type <- "regression"
  pm$general$is_glm <- 0
  pm$terms <- terms
  as_parsed_model(pm)
}
