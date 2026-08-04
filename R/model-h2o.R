# H2O gradient boosting models (via agua's "h2o_gbm" engine for boost_tree()).
#
# H2O models are handles into a running H2O cluster; the tree structure is
# pulled live with `h2o.getModelTree()`, so these functions require an active
# `h2o.init()` connection. Only GBM models (gaussian, bernoulli, multinomial
# distributions) are supported.

# Tree extraction ----------------------------------------------------

# Recursively turn an H2O tree node into a nested case_when() expression.
# `H2OSplitNode`s become case_when() and `H2OLeafNode`s become their
# prediction (the tree's contribution in link space).
build_h2o_node <- function(node) {
  if (inherits(node, "H2OLeafNode")) {
    return(node@prediction)
  }

  col <- rlang::sym(node@split_feature)
  na_left <- !is.na(node@na_direction) && node@na_direction == "LEFT"

  left <- build_h2o_node(node@left_child)
  right <- build_h2o_node(node@right_child)

  if (is.na(node@threshold)) {
    # Categorical split: left branch = value in left_levels
    lvls <- node@left_levels
    if (na_left) {
      condition <- expr(!!col %in% !!lvls | is.na(!!col))
    } else {
      condition <- expr(!!col %in% !!lvls)
    }
  } else {
    # Numeric split: left branch = (< threshold)
    threshold <- node@threshold
    if (na_left) {
      condition <- expr(!!col < !!threshold | is.na(!!col))
    } else {
      condition <- expr(!!col < !!threshold)
    }
  }

  expr(case_when(!!condition ~ !!left, .default = !!right))
}

h2o_n_trees <- function(model) {
  model@model$model_summary$number_of_trees
}

# Response domain (class labels) for classification models.
h2o_response_domain <- function(model) {
  y <- model@allparameters$y
  model@model$domains[[match(y, model@model$names)]]
}

# Build one nested expression per tree. For multinomial, `tree_class` selects
# the per-class tree at each iteration.
h2o_tree_exprs <- function(model, tree_class = NA) {
  map(
    seq_len(h2o_n_trees(model)),
    function(k) {
      tree <- h2o::h2o.getModelTree(
        model = model,
        tree_number = k,
        tree_class = tree_class
      )
      build_h2o_node(tree@root_node)
    }
  )
}

# Fit model --------------------------------------------------------

#' @export
tidypredict_fit.H2ORegressionModel <- function(model, ...) {
  if (identical(model@algorithm, "rulefit")) {
    return(tidypredict_fit_h2o_rulefit_regression(model))
  }
  f <- reduce_addition(h2o_tree_exprs(model))
  init_f <- model@model$init_f
  if (!is.null(init_f) && init_f != 0) {
    f <- expr_addition(init_f, f)
  }
  f
}

#' @export
tidypredict_fit.H2OBinomialModel <- function(model, ...) {
  if (identical(model@algorithm, "rulefit")) {
    return(tidypredict_fit_h2o_rulefit_binomial(model))
  }
  f <- reduce_addition(h2o_tree_exprs(model))
  init_f <- model@model$init_f %||% 0
  # Probability of the second (positive) domain level: logistic link.
  expr_logistic(expr(!!init_f + !!f))
}

#' @export
tidypredict_fit.H2OMultinomialModel <- function(model, ...) {
  if (identical(model@algorithm, "rulefit")) {
    return(tidypredict_fit_h2o_rulefit_multinomial(model))
  }
  domain <- h2o_response_domain(model)
  init_f <- model@model$init_f %||% 0

  raw_scores <- map(domain, function(class_label) {
    f <- reduce_addition(h2o_tree_exprs(model, tree_class = class_label))
    if (init_f != 0) {
      f <- expr_addition(init_f, f)
    }
    f
  })

  expr_softmax(raw_scores, domain)
}

# Test model -------------------------------------------------------
# H2O returns single-precision (float32) predictions, so the default
# threshold is looser than the package-wide 1e-12.

#' @export
tidypredict_test.H2ORegressionModel <- function(
  model,
  df,
  threshold = 0.000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  if (is.numeric(max_rows)) {
    df <- head(df, max_rows)
  }
  base <- as.data.frame(h2o::h2o.predict(model, h2o::as.h2o(df)))$predict
  te <- rlang::eval_tidy(tidypredict_fit(model), df)
  h2o_test_results(base, te, threshold)
}

#' @export
tidypredict_test.H2OBinomialModel <- function(
  model,
  df,
  threshold = 0.000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  if (is.numeric(max_rows)) {
    df <- head(df, max_rows)
  }
  preds <- as.data.frame(h2o::h2o.predict(model, h2o::as.h2o(df)))
  # Probability of the second (positive) domain level; H2O orders the
  # probability columns after the "predict" column in domain order.
  prob_cols <- setdiff(names(preds), "predict")
  base <- preds[[prob_cols[2]]]
  te <- rlang::eval_tidy(tidypredict_fit(model), df)
  h2o_test_results(base, te, threshold)
}

#' @export
tidypredict_test.H2OMultinomialModel <- function(
  model,
  df,
  threshold = 0.000001,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
) {
  df <- maybe_head(df, max_rows)

  domain <- h2o_response_domain(model)
  preds <- as.data.frame(h2o::h2o.predict(model, h2o::as.h2o(df)))
  formulas <- tidypredict_fit(model)

  te_matrix <- do.call(
    cbind,
    map(formulas, \(f) rlang::eval_tidy(f, df))
  )
  base_matrix <- as.matrix(preds[, domain, drop = FALSE])

  test_results_multiclass(base_matrix, te_matrix, threshold, domain)
}

h2o_test_results <- function(base, te, threshold) {
  test_results_numeric(base, te, threshold)
}
