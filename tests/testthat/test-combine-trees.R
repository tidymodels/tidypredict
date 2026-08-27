# Tree expressions call `case_when()` unqualified, which only resolves if dplyr
# is on the search path. `tests/testthat.R` attaches testthat and tidypredict
# only, so evaluate against dplyr's namespace explicitly rather than relying on
# some earlier test file having attached it. Without this the tests pass under
# `devtools::test()` and fail under `R CMD check`.
eval_tree_expr <- function(expr, data) {
  rlang::eval_tidy(expr, data, env = asNamespace("dplyr"))
}

# The identity every method has to satisfy: recombining the per-tree
# expressions computes what the whole-model fit computes. Compared by value
# rather than textually, since the two may legitimately arrange the arithmetic
# differently.
expect_combine_matches_fit <- function(model, data) {
  trees <- tidypredict_trees(model)

  expect_equal(
    eval_tree_expr(tidypredict_combine_trees(model, trees), data),
    eval_tree_expr(tidypredict_fit(model), data)
  )
}

# The same identity, but built from symbols naming columns the trees were
# written to first. This is the case the generic exists for.
expect_combine_symbols_match_fit <- function(model, data) {
  trees <- tidypredict_trees(model)
  names <- paste0("tree_", seq_along(trees))
  values <- lapply(trees, eval_tree_expr, data = data)
  # Recycled so that a forest of stumps, whose trees evaluate to a scalar,
  # still yields one row per observation.
  cols <- as.data.frame(stats::setNames(values, names))

  combined <- tidypredict_combine_trees(model, rlang::syms(names))

  expect_equal(
    eval_tree_expr(combined, cbind(cols, data)),
    eval_tree_expr(tidypredict_fit(model), data)
  )
}

test_that("check_trees_arg() rejects a non-list and an empty list", {
  expect_snapshot(error = TRUE, {
    check_trees_arg(1:3)
    check_trees_arg(list())
  })
})

test_that("tidypredict_combine_trees() has no default combination", {
  expect_error(
    tidypredict_combine_trees(lm(mpg ~ wt, data = mtcars), list(1)),
    class = "tidypredict_no_combiner"
  )
})

test_that("randomForest trees recombine into the fit", {
  skip_if_not_installed("randomForest")

  set.seed(1)
  model <- randomForest::randomForest(mpg ~ wt + cyl + disp, mtcars, ntree = 3)

  expect_combine_matches_fit(model, mtcars)
  expect_combine_symbols_match_fit(model, mtcars)
})

test_that("randomForest refuses to combine a corr.bias fit", {
  skip_if_not_installed("randomForest")

  set.seed(1)
  model <- randomForest::randomForest(
    mpg ~ wt + cyl + disp,
    mtcars,
    ntree = 3,
    corr.bias = TRUE
  )

  expect_error(
    tidypredict_combine_trees(model, list(1)),
    class = "tidypredict_no_combiner"
  )
})

test_that("randomForest refuses to combine a classification fit", {
  skip_if_not_installed("randomForest")

  set.seed(1)
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- randomForest::randomForest(vs ~ wt + cyl + disp, df, ntree = 3)

  expect_error(
    tidypredict_combine_trees(model, list(1)),
    class = "tidypredict_no_combiner"
  )
})

test_that("ranger trees recombine into the fit", {
  skip_if_not_installed("ranger")

  set.seed(1)
  model <- ranger::ranger(mpg ~ wt + cyl + disp, mtcars, num.trees = 3)

  expect_combine_matches_fit(model, mtcars)
  expect_combine_symbols_match_fit(model, mtcars)
})

test_that("xgboost trees recombine into the fit", {
  skip_if_not_installed("xgboost")

  data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )
  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "reg:squarederror",
      base_score = 0.5
    ),
    data = data,
    nrounds = 3L,
    verbose = 0
  )

  expect_combine_matches_fit(model, mtcars)
  expect_combine_symbols_match_fit(model, mtcars)
})

test_that("xgboost trees recombine under a non-identity objective", {
  skip_if_not_installed("xgboost")

  data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )
  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "binary:logistic",
      base_score = 0.5
    ),
    data = data,
    nrounds = 3L,
    verbose = 0
  )

  expect_combine_matches_fit(model, mtcars)
  expect_combine_symbols_match_fit(model, mtcars)
})

test_that("xgboost DART weights are not applied twice", {
  skip_if_not_installed("xgboost")

  # Shifted off the observed values so that a 32-bit split boundary cannot
  # route a row differently than the booster does.
  df <- mtcars
  df[, -9] <- df[, -9] + 0.1

  data <- xgboost::xgb.DMatrix(as.matrix(df[, -9]), label = df$am)
  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "reg:squarederror",
      base_score = 0.5,
      booster = "dart",
      rate_drop = 0.3,
      seed = 123
    ),
    data = data,
    nrounds = 4L,
    verbose = 0
  )

  expect_combine_matches_fit(model, df)
  expect_combine_symbols_match_fit(model, df)
})

test_that("lightgbm trees recombine into the fit", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  cols <- c("mpg", "cyl", "disp")
  dtrain <- lightgbm::lgb.Dataset(
    data.matrix(mtcars[, cols]),
    label = mtcars$hp,
    colnames = cols
  )
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 1.0,
      objective = "regression",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 3L,
    verbose = -1L
  )

  expect_combine_matches_fit(model, mtcars)
  expect_combine_symbols_match_fit(model, mtcars)
})

test_that("lightgbm refuses to combine a multiclass fit", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  cols <- c("Sepal.Length", "Sepal.Width", "Petal.Length")
  dtrain <- lightgbm::lgb.Dataset(
    data.matrix(iris[, cols]),
    label = as.integer(iris$Species) - 1L,
    colnames = cols
  )
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      objective = "multiclass",
      num_class = 3L,
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 2L,
    verbose = -1L
  )

  expect_snapshot(
    error = TRUE,
    tidypredict_combine_trees(model, tidypredict_trees(model))
  )
  expect_error(
    tidypredict_combine_trees(model, tidypredict_trees(model)),
    class = "tidypredict_no_combiner"
  )
})

test_that("catboost trees recombine into the fit", {
  skip_if_not_installed("catboost")

  set.seed(123)
  cols <- c("mpg", "cyl", "disp")
  pool <- catboost_catboost.load_pool(
    data.matrix(mtcars[, cols]),
    label = mtcars$hp,
    feature_names = as.list(cols)
  )
  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 3L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "RMSE",
      logging_level = "Silent"
    )
  )

  expect_combine_matches_fit(model, mtcars)
  expect_combine_symbols_match_fit(model, mtcars)
})

test_that("cforest trees recombine into the fit", {
  skip_if_not_installed("partykit")

  set.seed(1)
  model <- partykit::cforest(mpg ~ wt + cyl, data = mtcars, ntree = 3)

  expect_combine_matches_fit(model, mtcars)
  expect_combine_symbols_match_fit(model, mtcars)
})

test_that("blackboost trees recombine into the fit", {
  skip_if_not_installed("mboost")

  model <- mboost::blackboost(
    mpg ~ wt + cyl,
    data = mtcars,
    control = mboost::boost_control(mstop = 3)
  )

  expect_combine_matches_fit(model, mtcars)
  expect_combine_symbols_match_fit(model, mtcars)
})

test_that("aorsf trees recombine into the fit", {
  skip_if_not_installed("aorsf")

  set.seed(1)
  model <- aorsf::orsf(mtcars, mpg ~ wt + cyl + disp, n_tree = 3)

  expect_combine_matches_fit(model, mtcars)
  expect_combine_symbols_match_fit(model, mtcars)
})

test_that("boosted C5.0 refuses to combine its trees", {
  skip_if_not_installed("C50")

  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs, trials = 3)

  expect_snapshot(error = TRUE, tidypredict_combine_trees(model, list(1)))
  expect_error(
    tidypredict_combine_trees(model, list(1)),
    class = "tidypredict_no_combiner"
  )
})

# orbital writes each tree to its own column and then adds them in batches, so
# it hands back one subtotal per batch rather than one element per tree. A
# method that averaged over `length(trees)` would divide by the number of
# batches.
test_that("averaging methods take the divisor from the model", {
  skip_if_not_installed("partykit")

  set.seed(1)
  model <- partykit::cforest(mpg ~ wt + cyl, data = mtcars, ntree = 4)

  trees <- tidypredict_trees(model)
  batched <- list(
    rlang::expr(!!trees[[1]] + !!trees[[2]]),
    rlang::expr(!!trees[[3]] + !!trees[[4]])
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_combine_trees(model, batched), mtcars),
    rlang::eval_tidy(tidypredict_combine_trees(model, trees), mtcars)
  )
})

test_that("aorsf averaging takes the divisor from the model", {
  skip_if_not_installed("aorsf")

  set.seed(1)
  model <- aorsf::orsf(mtcars, mpg ~ wt + cyl + disp, n_tree = 4)

  trees <- tidypredict_trees(model)
  batched <- list(
    rlang::expr(!!trees[[1]] + !!trees[[2]]),
    rlang::expr(!!trees[[3]] + !!trees[[4]])
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_combine_trees(model, batched), mtcars),
    rlang::eval_tidy(tidypredict_combine_trees(model, trees), mtcars)
  )
})
