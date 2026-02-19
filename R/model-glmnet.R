# Predict ---------------------------------------

#' @export
tidypredict_fit.glmnet <- function(model) {
  if (inherits(model, "multnet")) {
    cli::cli_abort(
      c(
        "Multinomial glmnet models are not supported.",
        "i" = "Models fit with {.code family = \"multinomial\"} have multiple
        outcome columns which is not supported."
      )
    )
  }
  if (inherits(model, "mrelnet")) {
    cli::cli_abort(
      c(
        "Multivariate gaussian glmnet models are not supported.",
        "i" = "Models fit with {.code family = \"mgaussian\"} have multiple
        outcome columns which is not supported."
      )
    )
  }
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
  } else if (inherits(model, "coxnet")) {
    pm$general$family <- "cox"
    pm$general$link <- "identity"
    pm$general$is_glm <- 0
  } else if (inherits(model, "glmnetfit")) {
    pm$general$family <- model$family$family
    pm$general$link <- model$family$link
  } else {
    cli::cli_abort(
      "Model fit with this {.arg family} is not supported."
    )
  }

  as_parsed_model(pm)
}

# For {orbital}
#' Build linear predictor expression from coefficient names and values
#'
#' Shared helper for building linear predictor strings from coefficients.
#' Used by orbital package for glmnet models.
#'
#' @param coef_names Character vector of coefficient names (including "(Intercept)")
#' @param coef_values Numeric vector of coefficient values
#' @keywords internal
#' @export
.build_linear_pred <- function(coef_names, coef_values) {
  terms <- character(0)
  for (i in seq_along(coef_names)) {
    if (coef_values[i] == 0) {
      next
    }

    if (coef_names[i] == "(Intercept)") {
      terms <- c(terms, as.character(coef_values[i]))
    } else {
      # Use backticks for variable names to handle special characters
      var_name <- paste0("`", coef_names[i], "`")
      terms <- c(terms, paste0("(", var_name, " * ", coef_values[i], ")"))
    }
  }

  if (length(terms) == 0) {
    return("0")
  }

  paste(terms, collapse = " + ")
}

#' Extract multiclass linear predictors for glmnet models
#'
#' For use in orbital package.
#' @param model A glmnet model object with class "multnet"
#' @param penalty The penalty value to use for coefficient extraction
#' @keywords internal
#' @export
.extract_glmnet_multiclass <- function(model, penalty = NULL) {
  if (!inherits(model, "multnet")) {
    cli::cli_abort(
      "{.arg model} must be {.cls multnet}, not {.obj_type_friendly {model}}."
    )
  }

  if (is.null(penalty)) {
    if (length(model$lambda) != 1) {
      cli::cli_abort(
        c(
          "glmnet model has multiple penalty values.",
          "i" = "Specify a single {.arg penalty} value."
        )
      )
    }
    penalty <- model$lambda
  }

  # Get coefficients for each class at the specified penalty
  coefs_list <- stats::coef(model, s = penalty)
  class_names <- names(coefs_list)

  # Build linear predictor expression for each class
  eqs <- lapply(coefs_list, function(coef_mat) {
    coef_names <- rownames(coef_mat)
    coef_values <- as.numeric(coef_mat)
    .build_linear_pred(coef_names, coef_values)
  })

  names(eqs) <- class_names
  eqs
}
