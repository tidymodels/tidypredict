# Predict ---------------------------------------

#' @export
tidypredict_fit.nnet <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_nnet(parsedmodel)
}

#' @export
tidypredict_fit.pm_nnet <- function(model) {
  build_fit_formula_nnet(model)
}

# `nnet()` fits a feed-forward network with a single hidden layer. Each computed
# unit is the weighted sum of the units feeding into it, optionally passed
# through the logistic squashing function. Output units are left unsquashed when
# the network was fit with `linout = TRUE`, which is also what `softmax = TRUE`
# implies.
build_fit_formula_nnet <- function(parsedmodel) {
  units <- nnet_unit_exprs(parsedmodel)

  n_units <- parsedmodel$general$n_units
  n_outputs <- parsedmodel$general$n_outputs
  outputs <- units[seq(n_units - n_outputs + 1, n_units)]
  classes <- as.character(unlist(parsedmodel$classes))

  if (isTRUE(parsedmodel$general$softmax)) {
    return(nnet_softmax(outputs, classes))
  }

  if (length(classes) > 0) {
    # A two level factor is fit with a single logistic output unit that gives
    # the probability of the second level
    p <- outputs[[1]]
    res <- list(expr(1 - (!!p)), p)
    names(res) <- classes
    return(res)
  }

  outputs[[1]]
}

nnet_softmax <- function(outputs, classes) {
  expr_softmax(outputs, classes)
}

# `predict.model_fit()` runs the probabilities that `predict.nnet()` already
# normalized through a second softmax, so the parsnip path has to do the same to
# match `predict(model, type = "prob")`
tidypredict_fit_nnet_parsnip <- function(model) {
  res <- tidypredict_fit(model$fit)

  if (!is.list(res)) {
    return(res)
  }

  nnet_softmax(res, names(res))
}

# Builds one expression per unit, indexed by unit number plus one, so that
# `units[[1]]` is the bias unit. Units are visited in order, which means the
# expression of every unit feeding into a unit is available by the time it is
# needed.
nnet_unit_exprs <- function(parsedmodel) {
  units <- vector("list", parsedmodel$general$n_units)
  units[[1]] <- expr(1)

  for (i in seq_along(parsedmodel$inputs)) {
    input <- parsedmodel$inputs[[i]]
    cols <- map(input$fields, lm_constructor)
    units[[i + 1]] <- reduce_multiplication(cols)
  }

  for (unit in parsedmodel$units) {
    terms <- map(unit$weights, function(weight) {
      w <- as.numeric(weight$weight)
      from <- as.integer(weight$from)
      if (from == 0) {
        return(expr(!!w))
      }
      expr((!!units[[from + 1]]) * !!w)
    })
    f <- reduce_addition(terms)
    if (isTRUE(unit$squash)) {
      f <- nnet_squash(f)
    }
    units[[as.integer(unit$index) + 1]] <- f
  }

  units
}

# The logistic squashing function of `nnet()` saturates outside of `[-15, 15]`
# instead of evaluating the logistic function, so the same cut offs are needed
# here to match `predict()` exactly
nnet_squash <- function(f) {
  expr(case_when(
    (!!f) < -15 ~ 0,
    (!!f) > 15 ~ 1,
    .default = !!expr_logistic(f)
  ))
}

# Parse model --------------------------------------

#' @export
parse_model.nnet <- function(model) {
  acceptable_formula(model)

  n_units <- model$nunits
  n_inputs <- model$n[[1]]
  n_outputs <- model$n[[3]]

  if (n_outputs > 1 && length(model$lev) == 0) {
    cli::cli_abort(
      "{.fn tidypredict_fit} does not support {.fn nnet::nnet} models with
       multiple outputs that are not a classification."
    )
  }

  # `nnet.default()` fits keep neither `terms` nor `coefnames`, so the names of
  # the input columns are lost and the input units cannot be tied to columns of
  # the new data
  if (is.null(model$terms) && is.null(model$coefnames)) {
    cli::cli_abort(c(
      "{.fn tidypredict_fit} does not support {.fn nnet::nnet} models fit with
       the matrix interface.",
      "i" = "Refit the model with the formula interface, so that the names of
             the predictors are available."
    ))
  }

  vars <- names(attr(model$terms, "dataClasses")) %||% model$coefnames
  inputs <- map(
    model$coefnames,
    ~ list(label = .x, fields = parse_label_lm(.x, vars))
  )

  # Every unit with incoming weights, which is every unit but the bias and the
  # inputs. `nconn` holds the cumulative number of weights, so the weights of
  # unit `i` sit between `nconn[i + 1]` and `nconn[i + 2]`.
  units <- map(seq(n_inputs + 1, n_units - 1), function(i) {
    idx <- seq(model$nconn[[i + 1]] + 1, model$nconn[[i + 2]])
    list(
      index = i,
      squash = i < model$nsunits,
      weights = map(
        idx,
        ~ list(from = as.integer(model$conn[[.x]]), weight = model$wts[[.x]])
      )
    )
  })

  pm <- list()
  pm$general$model <- "nnet"
  pm$general$version <- 2
  pm$general$type <- "nnet"
  pm$general$n_units <- n_units
  pm$general$n_outputs <- n_outputs
  pm$general$softmax <- isTRUE(model$softmax)
  pm$classes <- model$lev
  pm$inputs <- inputs
  pm$units <- units

  as_parsed_model(pm)
}

#' @export
acceptable_formula.nnet <- function(model) {
  if (is.null(model$terms)) {
    return(invisible(NULL))
  }
  acceptable_lm(model)
}

# Test ---------------------------------------------

#' @export
tidypredict_test.nnet <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  if (length(model$lev) > 0) {
    abort_test_unsupported(
      "classification {.fn nnet::nnet} models",
      "class predictions"
    )
  }

  if (is.numeric(max_rows)) {
    df <- head(df, max_rows) # nocov
  }

  preds <- predict(model, df, type = "raw")
  base <- data.frame(fit = as.vector(preds), row.names = NULL)

  te <- tidypredict_to_column(
    df,
    model,
    add_interval = FALSE,
    vars = c("fit_te", "upr_te", "lwr_te")
  )
  test_results_numeric(
    base$fit,
    te[, "fit_te"],
    threshold,
    model$call
  )
}
