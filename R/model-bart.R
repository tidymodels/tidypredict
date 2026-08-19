# Predict ---------------------------------------

#' @export
tidypredict_fit.bart <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula_bart(parsedmodel)
}

#' @export
tidypredict_fit.pm_bart <- function(model) {
  build_fit_formula_bart(model)
}

# `bart()` fits a sum-of-trees model with a Bayesian back fitting MCMC sampler.
# A single posterior draw predicts the sum of the leaf values of all of its
# trees, and the fitted value is the average of those sums over every draw. The
# leaf values live on the scale that `bart()` centers and scales the outcome to,
# so the average is mapped back onto the scale of the outcome at the end.
build_fit_formula_bart <- function(parsedmodel) {
  tree_exprs <- map(parsedmodel$trees, build_bart_tree)

  res <- reduce_addition(tree_exprs)
  res <- expr_division(res, as.numeric(parsedmodel$general$n_draws))
  res <- expr_multiplication(res, as.numeric(parsedmodel$general$y_scale))
  expr_addition(res, as.numeric(parsedmodel$general$y_center))
}

# Nodes of a tree are stored depth first, so a node is followed by its complete
# left sub tree and then by its complete right sub tree
build_bart_tree <- function(nodes) {
  idx <- 0L

  build_node <- function() {
    idx <<- idx + 1L
    node <- nodes[[idx]]

    if (isTRUE(as.logical(node$terminal))) {
      return(as.numeric(node$value))
    }

    left_subtree <- build_node()
    right_subtree <- build_node()

    condition <- build_bart_split_condition(node)
    expr(case_when(!!condition ~ !!left_subtree, .default = !!right_subtree))
  }

  build_node()
}

build_bart_split_condition <- function(node) {
  col <- rlang::sym(node$col)

  # Factors are expanded into indicator columns before fitting, so a split on
  # one of those columns is a split on a single level of the original factor
  if (!is.null(node$level)) {
    return(expr(!!col != !!node$level))
  }

  expr(!!col <= !!as.numeric(node$value))
}

# Parse model --------------------------------------

#' @export
parse_model.bart <- function(model) {
  check_bart_supported(model)

  trees <- dbarts::extract(model, "trees")
  cols <- bart_column_map(model)

  pm <- list()
  pm$general$model <- "bart"
  pm$general$version <- 3
  pm$general$type <- "bart"
  pm$general$n_draws <- bart_n_draws(trees)
  pm$general$y_center <- mean(range(model$y))
  pm$general$y_scale <- diff(range(model$y))
  pm$trees <- map(bart_split_trees(trees), function(rows) {
    bart_tree_nodes(trees[rows, ], cols)
  })

  as_parsed_model(pm)
}

check_bart_supported <- function(model) {
  if (is.null(model$fit)) {
    cli::cli_abort(c(
      "{.fn tidypredict_fit} needs the trees of the {.fn dbarts::bart} model.",
      i = "Refit the model with {.code keeptrees = TRUE}."
    ))
  }

  if (isTRUE(model$fit$control@binary)) {
    cli::cli_abort(c(
      "Classification {.fn dbarts::bart} models are not supported.",
      i = "Only regression models can be converted to tidy formulas.",
      i = "Classification uses the probit link, which cannot be translated to
           SQL."
    ))
  }
}

bart_n_draws <- function(trees) {
  draws <- trees$sample
  if (!is.null(trees$chain)) {
    draws <- paste(trees$chain, draws)
  }
  length(unique(draws))
}

# One element per tree of every posterior draw. The order of the trees does not
# matter, since their predictions are summed
bart_split_trees <- function(trees) {
  keys <- paste(trees$chain %||% 1, trees$sample, trees$tree)
  unname(split(seq_len(nrow(trees)), factor(keys, levels = unique(keys))))
}

bart_tree_nodes <- function(tree, cols) {
  map(seq_len(nrow(tree)), function(i) {
    var <- tree$var[[i]]
    value <- tree$value[[i]]
    if (var == -1) {
      return(list(terminal = TRUE, value = value))
    }
    c(list(terminal = FALSE, value = value), cols[[var]])
  })
}

# Maps every column of the model matrix that `bart()` fit on back onto a column
# of the data, and onto a level of that column when the column is a factor
bart_column_map <- function(model) {
  x <- model$fit$data@x
  cols <- colnames(x)

  if (is.null(cols)) {
    cli::cli_abort(
      "{.fn tidypredict_fit} needs named predictors, the {.fn dbarts::bart}
       model was fit on an unnamed matrix."
    )
  }

  drop <- attr(x, "drop")

  # A matrix of predictors is used as is
  if (is.null(drop)) {
    return(map(cols, function(col) list(col = col)))
  }

  res <- list()
  for (var in names(drop)) {
    # A predictor that is constant in the training data is dropped by `bart()`,
    # so it has no column in the model matrix and is never split on
    if (isTRUE(drop[[var]])) {
      next
    }

    if (isFALSE(drop[[var]])) {
      res <- c(res, list(list(col = var)))
      next
    }

    prefix <- paste0(var, ".")
    levels <- cols[seq_along(cols) > length(res) & startsWith(cols, prefix)]
    res <- c(
      res,
      map(levels, function(level) {
        list(col = var, level = substr(level, nchar(prefix) + 1, nchar(level)))
      })
    )
  }

  if (length(res) != length(cols)) {
    cli::cli_abort(
      "Unable to map the predictors of the {.fn dbarts::bart} model onto the
       columns of the data.",
      .internal = TRUE
    )
  }

  res
}

# Test ---------------------------------------------

#' @export
tidypredict_test.bart <- function(
  model,
  df,
  threshold = 0.000000000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  if (is.numeric(max_rows)) {
    df <- head(df, max_rows) # nocov
  }

  # `predict()` returns one row per posterior draw
  preds <- colMeans(predict(model, df))
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

# For {orbital} -----------------------------------------------

#' Extract the trees of a bart model
#'
#' For use in orbital package.
#' @param model A `dbarts::bart()` model object
#' @keywords internal
#' @export
.extract_bart_trees <- function(model) {
  if (!inherits(model, "bart")) {
    cli::cli_abort(
      "{.arg model} must be {.cls bart}, not {.obj_type_friendly {model}}."
    )
  }

  parsedmodel <- parse_model(model)
  map(parsedmodel$trees, build_bart_tree)
}

#' Extract the outcome scaling of a bart model
#'
#' For use in orbital package. The trees of `.extract_bart_trees()` predict on
#' the scale that `dbarts::bart()` centers and scales the outcome to. Their sum,
#' divided by `n_draws` and multiplied by `y_scale`, plus `y_center`, gives the
#' fitted value.
#' @param model A `dbarts::bart()` model object
#' @keywords internal
#' @export
.extract_bart_scaling <- function(model) {
  if (!inherits(model, "bart")) {
    cli::cli_abort(
      "{.arg model} must be {.cls bart}, not {.obj_type_friendly {model}}."
    )
  }

  check_bart_supported(model)

  list(
    n_draws = bart_n_draws(dbarts::extract(model, "trees")),
    y_center = mean(range(model$y)),
    y_scale = diff(range(model$y))
  )
}
