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
      "{.fn tidypredict_fit} requires that there are only 1 penalty selected,
      {length(model$lambda)} were provided.",
      call = call
    )
  }
  if (inherits(model$beta, "dgCMatrix")) {
    model$beta <- setNames(as.numeric(model$beta), rownames(model$beta))
  }
  coefs <- c("(Intercept)" = unname(model$a0), model$beta)

  names <- names(coefs)
  values <- as.vector(coefs)

  terms <- map2(values, names, function(value, name) {
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
  pm$general$is_glm <- 1
  pm$terms <- terms

  if (inherits(model, "elnet")) {
    pm$general$family <- "gaussian"
    pm$general$link <- "identity"
  } else if (inherits(model, "lognet")) {
    pm$general$family <- "binomial"
    pm$general$link <- "logit"
  } else if (inherits(model, "fishnet")) {
    pm$general$family <- "poisson"
    pm$general$link <- "log"
  } else {
    cli::cli_abort(
      "Model fit with this {.arg family} is not supported."
    )
  }

  as_parsed_model(pm)
}
