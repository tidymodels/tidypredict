# Predict ---------------------------------------

#' @export
tidypredict_fit.glmnet <- function(model) {
  if (inherits(model, "multnet")) {
    return(build_fit_formula_multinom(parse_model(model)))
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
  if (inherits(model, "multnet")) {
    return(parse_model_glmnet_multinom(model))
  }
  parse_model_glmnet(model)
}

# `glmnet` records only whether an offset was used, never the values, and
# `predict()` requires them again as `newoffset`. There is nothing on the model
# to rebuild the offset from, so the model cannot be reproduced at all.
glmnet_check_no_offset <- function(model, call = rlang::caller_env()) {
  if (isTRUE(model$offset)) {
    cli::cli_abort(
      c(
        "Models fit with an {.arg offset} are not supported for glmnet.",
        i = "{.pkg glmnet} stores only a flag, not the offset values, so the
        prediction cannot be reproduced."
      ),
      call = call
    )
  }
  invisible(model)
}

parse_model_glmnet <- function(model, call = rlang::caller_env()) {
  if (length(model$lambda) != 1) {
    cli::cli_abort(
      "{.fn tidypredict_fit} requires that there are only 1 penalty selected,
      {length(model$lambda)} were provided.",
      call = call
    )
  }
  glmnet_check_no_offset(model, call = call)
  if (inherits(model$beta, "dgCMatrix")) {
    model$beta <- setNames(as.numeric(model$beta), rownames(model$beta))
  }
  coefs <- c("(Intercept)" = unname(model$a0), model$beta)

  pm <- list()
  pm$general$model <- class(model)[[2]]
  pm$general$version <- 1
  pm$general$type <- "regression"
  pm$general$is_glm <- 1
  pm$terms <- glmnet_terms(coefs)

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
    # nocov start
    cli::cli_abort(
      "Model fit with this {.arg family} is not supported.",
      .internal = TRUE
    )
  } # nocov end

  as_parsed_model(pm)
}

# glmnet is fit from a numeric matrix, so each coefficient names a column
# directly and penalised-away coefficients are dropped.
glmnet_terms <- function(coefs) {
  build_terms(
    as.vector(coefs),
    names(coefs),
    vars = NULL,
    drop_zero = TRUE
  )
}

parse_model_glmnet_multinom <- function(model, call = rlang::caller_env()) {
  if (length(model$lambda) != 1) {
    cli::cli_abort(
      "{.fn tidypredict_fit} requires that there are only 1 penalty selected,
      {length(model$lambda)} were provided.",
      call = call
    )
  }
  glmnet_check_no_offset(model, call = call)

  classes <- model$classnames
  a0 <- model$a0

  class_terms <- lapply(classes, function(cl) {
    beta <- model$beta[[cl]]
    beta <- setNames(as.numeric(beta), rownames(beta))
    coefs <- c("(Intercept)" = unname(a0[cl, ]), beta)
    glmnet_terms(coefs)
  })

  new_multiclass_parsed_model(
    class(model)[[2]],
    classes,
    class_terms,
    version = 1
  )
}

build_fit_formula_multinom <- function(parsedmodel) {
  lps <- map(parsedmodel$class_terms, build_linear_predictor)
  expr_softmax(lps, parsedmodel$classes)
}

#' @export
tidypredict_fit.pm_multiclass_regression <- function(model) {
  build_fit_formula_multinom(model)
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

# Extractors --------------------------------------------------

#' @export
tidypredict_class_exprs.multnet <- function(x, ..., penalty = NULL) {
  rlang::check_dots_empty()

  if (is.null(penalty)) {
    if (length(x$lambda) != 1) {
      cli::cli_abort(
        c(
          "glmnet model has multiple penalty values.",
          "i" = "Specify a single {.arg penalty} value."
        )
      )
    }
    penalty <- x$lambda
  }

  # Get coefficients for each class at the specified penalty
  coefs_list <- stats::coef(x, s = penalty)
  class_names <- names(coefs_list)

  # Build linear predictor expression for each class
  eqs <- lapply(coefs_list, function(coef_mat) {
    coef_names <- rownames(coef_mat)
    coef_values <- as.numeric(coef_mat)
    # .build_linear_pred() returns a string; the generic promises a language
    # object. A model with every coefficient zero gives "0", which parses to a
    # bare numeric, consistent with how stumps are returned elsewhere.
    str2lang(.build_linear_pred(coef_names, coef_values))
  })

  names(eqs) <- class_names
  eqs
}
