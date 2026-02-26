skip_if_not_installed("randomForest")

test_that("Tester returns warning", {
  t <- tidypredict_test(
    lm(mpg ~ wt, data = mtcars),
    threshold = 1
  )
  expect_false(t$alert)
})

test_that("Intervals returns list", {
  expect_s3_class(
    tidypredict_test(lm(mpg ~ ., data = mtcars), include_intervals = TRUE),
    "tidypredict_test"
  )
})

test_that("alert triggered with threshold = 0", {
  t <- tidypredict_test(
    lm(mpg ~ wt + cyl, data = mtcars),
    threshold = 0
  )
  expect_true(t$alert)
  expect_snapshot(cat(t$message))
})

test_that("alert with intervals", {
  t <- tidypredict_test(
    lm(mpg ~ wt + cyl + disp + hp + drat, data = mtcars),
    threshold = 0,
    include_intervals = TRUE
  )
  expect_true(t$alert)
  expect_snapshot(cat(t$message))
})

test_that("max_rows limits data", {
  t <- tidypredict_test(
    lm(mpg ~ wt, data = mtcars),
    max_rows = 5
  )
  expect_equal(nrow(t$raw_results), 5)
})

test_that("print method works", {
  t <- tidypredict_test(lm(mpg ~ wt, data = mtcars))
  expect_output(print(t), "tidypredict test results")
})

test_that("knit_print method works", {
  t <- tidypredict_test(lm(mpg ~ wt, data = mtcars))
  expect_match(knitr::knit_print(t), "tidypredict test results")
})

test_that("offset handling in tidypredict_test", {
  model <- lm(mpg ~ wt + cyl, offset = am, data = mtcars)
  t <- tidypredict_test(model)
  expect_false(t$alert)
})

test_that("xgboost alert branch", {
  skip_if_not_installed("xgboost")
  df <- mtcars[, c("wt", "cyl", "disp")]
  xg_mat <- xgboost::xgb.DMatrix(as.matrix(df), label = mtcars$mpg)
  model <- xgboost::xgb.train(
    params = list(max_depth = 2, eta = 0.5, objective = "reg:squarederror"),
    data = xg_mat,
    nrounds = 3,
    verbose = 0
  )
  t <- tidypredict_test(model, df = df, threshold = 0, xg_df = xg_mat)
  expect_true(t$alert)
  expect_snapshot(cat(t$message))
})

test_that("glmnet alert branch (mocked)", {
  skip_if_not_installed("glmnet")
  df <- mtcars[, c("wt", "cyl", "disp")]
  model <- glmnet::glmnet(as.matrix(df), mtcars$mpg)
  model$lambda <- model$lambda[5]

  local_mocked_bindings(
    tidypredict_to_column = function(df, model, ...) {
      df$fit_te <- rep(999, nrow(df))
      df
    }
  )

  t <- tidypredict_test(model, df = df, threshold = 0)
  expect_true(t$alert)
  expect_snapshot(cat(t$message))
})

test_that("lightgbm alert branch (mocked)", {
  skip_if_not_installed("lightgbm")
  df <- mtcars[, c("wt", "cyl", "disp")]
  lgb_mat <- as.matrix(df)
  dtrain <- lightgbm::lgb.Dataset(
    lgb_mat,
    label = mtcars$mpg,
    colnames = colnames(df)
  )
  model <- lightgbm::lgb.train(
    params = list(
      objective = "regression",
      num_leaves = 4L,
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 10L,
    verbose = -1L
  )

  local_mocked_bindings(
    tidypredict_to_column = function(df, model, ...) {
      df$fit_te <- rep(999, nrow(df))
      df
    }
  )

  t <- tidypredict_test(model, df = df, threshold = 0, xg_df = lgb_mat)
  expect_true(t$alert)
  expect_snapshot(cat(t$message))
})

test_that("catboost max_rows", {
  skip_if_not_installed("catboost")
  df <- mtcars[, c("wt", "cyl", "disp")]
  cb_mat <- as.matrix(df)
  pool <- catboost::catboost.load_pool(
    cb_mat,
    label = mtcars$mpg,
    feature_names = as.list(colnames(df))
  )
  model <- catboost::catboost.train(
    pool,
    params = list(
      iterations = 3,
      depth = 2,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )
  t <- tidypredict_test(model, df = df, xg_df = cb_mat, max_rows = 5)
  expect_equal(nrow(t$raw_results), 5)
})


test_that("catboost alert branch (mocked)", {
  skip_if_not_installed("catboost")
  df <- mtcars[, c("wt", "cyl", "disp")]
  cb_mat <- as.matrix(df)
  pool <- catboost::catboost.load_pool(
    cb_mat,
    label = mtcars$mpg,
    feature_names = as.list(colnames(df))
  )
  model <- catboost::catboost.train(
    pool,
    params = list(
      iterations = 3,
      depth = 2,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )

  local_mocked_bindings(
    tidypredict_to_column = function(df, model, ...) {
      df$fit_te <- rep(999, nrow(df))
      df
    }
  )

  t <- tidypredict_test(model, df = df, threshold = 0, xg_df = cb_mat)
  expect_true(t$alert)
  expect_snapshot(cat(t$message))
})
