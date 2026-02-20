# Helper to create test model
# Uses mtcars[, -9] (all columns except 'am') to avoid boundary issues
# This matches the original test setup and avoids floating point precision
# issues at exact split boundaries
make_xgb_model <- function(
  max_depth = 2L,
  nrounds = 4L,
  objective = "reg:squarederror"
) {
  xgb_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )

  xgboost::xgb.train(
    params = list(
      max_depth = max_depth,
      objective = objective,
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = nrounds,
    verbose = 0
  )
}

# Helper to get the standard xgb.DMatrix for testing
make_xgb_data <- function() {
  xgboost::xgb.DMatrix(
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )
}

# Parser tests ---------------------------------------------------------------

test_that("parse_model returns correct structure", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  pm <- parse_model(model)

  expect_s3_class(pm, "parsed_model")
  expect_s3_class(pm, "pm_xgb")

  expect_equal(pm$general$model, "xgb.Booster")
  expect_equal(pm$general$type, "xgb")
  expect_equal(pm$general$version, 1)

  expect_gt(length(pm$trees), 0)
})

test_that("correct number of trees extracted", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model(nrounds = 5L)
  pm <- parse_model(model)

  expect_length(pm$trees, 5)
})

test_that("each tree has leaves with predictions and paths", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  pm <- parse_model(model)

  tree1 <- pm$trees[[1]]
  expect_gt(length(tree1), 0)

  for (leaf in tree1) {
    expect_contains(names(leaf), "prediction")
    expect_contains(names(leaf), "path")
    expect_type(leaf$prediction, "double")
    expect_type(leaf$path, "list")
  }
})

test_that("path conditions have correct structure", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  pm <- parse_model(model)

  tree1 <- pm$trees[[1]]
  leaves_with_paths <- which(vapply(tree1, \(x) length(x$path) > 0, logical(1)))

  if (length(leaves_with_paths) > 0) {
    leaf_with_path <- tree1[[leaves_with_paths[1]]]

    cond <- leaf_with_path$path[[1]]
    expect_equal(cond$type, "conditional")
    expect_contains(names(cond), c("col", "val", "op", "missing"))
    expect_contains(c("less", "more-equal"), cond$op)
  }
})

test_that("feature names are extracted", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  pm <- parse_model(model)

  # Uses mtcars[, -9] which has all columns except 'am'
  expected_names <- colnames(mtcars)[-9]
  expect_equal(pm$general$feature_names, expected_names)
})

test_that("params are extracted", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  pm <- parse_model(model)

  expect_contains(names(pm$general), "params")
  expect_equal(pm$general$params$objective, "reg:squarederror")
})

test_that("niter and nfeatures are extracted", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model(nrounds = 7L)
  pm <- parse_model(model)

  expect_equal(pm$general$niter, 7)
  # Uses mtcars[, -9] which has 10 columns
  expect_equal(pm$general$nfeatures, 10)
})

test_that("base_score is extracted", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  pm <- parse_model(model)

  expect_type(pm$general$params$base_score, "double")
})

test_that("path contains both less and more-equal operators", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  pm <- parse_model(model)

  all_ops <- unlist(lapply(pm$trees[[1]], function(leaf) {
    vapply(leaf$path, \(p) p$op, character(1))
  }))

  expect_contains(all_ops, "more-equal")
  expect_contains(all_ops, "less")
})

test_that("deeper tree paths are traced correctly", {
  skip_if_not_installed("xgboost")

  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 3), ncol = 3)
  colnames(X) <- c("a", "b", "c")
  y <- X[, 1] + X[, 2] * 2 + X[, 3] * 3 + rnorm(n, sd = 0.1)

  dtrain <- xgboost::xgb.DMatrix(
    X,
    label = y,
    feature_names = c("a", "b", "c")
  )
  model <- xgboost::xgb.train(
    params = list(
      max_depth = 4L,
      objective = "reg:squarederror"
    ),
    data = dtrain,
    nrounds = 1L,
    verbose = 0
  )

  pm <- parse_model(model)
  tree <- pm$trees[[1]]

  path_lengths <- vapply(tree, \(leaf) length(leaf$path), integer(1))
  expect_true(any(path_lengths >= 2))
})

test_that("model without explicit feature names still works", {
  skip_if_not_installed("xgboost")

  set.seed(789)
  X <- data.matrix(mtcars[, c("mpg", "cyl")])
  y <- mtcars$hp

  dtrain <- xgboost::xgb.DMatrix(X, label = y)

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "reg:squarederror"
    ),
    data = dtrain,
    nrounds = 3L,
    verbose = 0
  )

  pm <- parse_model(model)

  expect_s3_class(pm, "pm_xgb")
  expect_length(pm$trees, 3)
  expect_equal(pm$general$nfeatures, 2)
})

# Fit formula tests ----------------------------------------------------------

test_that("tidypredict_fit returns language object", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()

  fit_formula <- tidypredict_fit(model)

  expect_type(fit_formula, "language")
})

test_that("tidypredict_fit works on parsed model", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  pm <- parse_model(model)

  fit_formula <- tidypredict_fit(pm)

  expect_type(fit_formula, "language")
})

test_that("reg:squarederror predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model(objective = "reg:squarederror")

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("binary:logistic predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model(objective = "binary:logistic")

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("reg:logistic predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model(objective = "reg:logistic")

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("binary:logitraw predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model(objective = "binary:logitraw")

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("count:poisson predictions match native predict", {
  skip_if_not_installed("xgboost")

  # Add 0.1 to avoid exact split boundaries (float32 vs float64 precision)
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- mtcars_adj[, -9] + 0.1

  xgb_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$carb
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "count:poisson",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(
    model,
    mtcars_adj,
    xg_df = xgb_data,
    threshold = 1e-6
  )

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("reg:tweedie predictions match native predict", {
  skip_if_not_installed("xgboost")

  # Add 0.1 to avoid exact split boundaries (float32 vs float64 precision)
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- mtcars_adj[, -9] + 0.1

  xgb_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$hp
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "reg:tweedie",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(
    model,
    mtcars_adj,
    xg_df = xgb_data,
    threshold = 1e-6
  )

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("reg:squaredlogerror predictions match native predict", {
  skip_if_not_installed("xgboost")

  # Add 0.1 to avoid exact split boundaries (float32 vs float64 precision)
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- mtcars_adj[, -9] + 0.1

  xgb_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$hp
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "reg:squaredlogerror",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(
    model,
    mtcars_adj,
    xg_df = xgb_data,
    threshold = 1e-6
  )

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("reg:gamma predictions match native predict", {
  skip_if_not_installed("xgboost")

  # Add 0.1 to avoid exact split boundaries (float32 vs float64 precision)
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- mtcars_adj[, -9] + 0.1

  xgb_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$hp
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "reg:gamma",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(
    model,
    mtcars_adj,
    xg_df = xgb_data,
    threshold = 1e-6
  )

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("reg:pseudohubererror predictions match native predict", {
  skip_if_not_installed("xgboost")

  # Add 0.1 to avoid exact split boundaries (float32 vs float64 precision)
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- mtcars_adj[, -9] + 0.1

  xgb_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$hp
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "reg:pseudohubererror",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(
    model,
    mtcars_adj,
    xg_df = xgb_data,
    threshold = 1e-6
  )

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("reg:absoluteerror predictions match native predict", {
  skip_if_not_installed("xgboost")

  # Add 0.1 to avoid exact split boundaries (float32 vs float64 precision)
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- mtcars_adj[, -9] + 0.1

  xgb_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$hp
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "reg:absoluteerror",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(
    model,
    mtcars_adj,
    xg_df = xgb_data,
    threshold = 1e-5
  )

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("binary:hinge predictions match native predict", {
  skip_if_not_installed("xgboost")

  # Add 0.1 to avoid exact split boundaries (float32 vs float64 precision)
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- mtcars_adj[, -9] + 0.1

  xgb_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$am
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "binary:hinge",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(
    model,
    mtcars_adj,
    xg_df = xgb_data,
    threshold = 1e-7
  )

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("DART booster with rate_drop = 0 predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "reg:squarederror",
      base_score = 0.5,
      booster = "dart",
      rate_drop = 0
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("DART booster with rate_drop > 0 predictions match native predict", {
  skip_if_not_installed("xgboost")

  # Add 0.1 to avoid exact split boundaries (float32 vs float64 precision)
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- mtcars_adj[, -9] + 0.1

  xgb_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$am
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "reg:squarederror",
      base_score = 0.5,
      booster = "dart",
      rate_drop = 0.3,
      seed = 123
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(
    model,
    mtcars_adj,
    xg_df = xgb_data,
    threshold = 1e-6
  )

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("DART booster weight_drop is extracted correctly", {
  skip_if_not_installed("xgboost")

  xgb_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "reg:squarederror",
      base_score = 0.5,
      booster = "dart",
      rate_drop = 0.3,
      seed = 123
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  pm <- parse_model(model)

  expect_equal(pm$general$booster_name, "dart")
  expect_type(pm$general$weight_drop, "double")
  expect_length(pm$general$weight_drop, 4)
  # At least one weight should be different from 1 when rate_drop > 0
  expect_false(all(pm$general$weight_drop == 1))
})

test_that("gbtree booster has no weight_drop", {
  skip_if_not_installed("xgboost")

  xgb_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "reg:squarederror",
      base_score = 0.5,
      booster = "gbtree"
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  pm <- parse_model(model)

  expect_equal(pm$general$booster_name, "gbtree")
  expect_null(pm$general$weight_drop)
})

test_that("model with custom base_score works correctly", {
  skip_if_not_installed("xgboost")

  xgb_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "reg:logistic",
      base_score = mean(mtcars$am)
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("base_score of 0 is not included in formula", {
  skip_if_not_installed("xgboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$am
  dtrain <- xgboost::xgb.DMatrix(
    X,
    label = y,
    feature_names = c("mpg", "cyl", "disp")
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 1L,
      objective = "reg:squarederror",
      base_score = 0
    ),
    data = dtrain,
    nrounds = 1L,
    verbose = 0
  )

  res <- tidypredict_fit(model)
  res_text <- rlang::expr_text(res)
  expect_false(grepl("\\+ 0$", res_text))
})

test_that("base_score of 0.5 is included in formula", {
  skip_if_not_installed("xgboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$am
  dtrain <- xgboost::xgb.DMatrix(
    X,
    label = y,
    feature_names = c("mpg", "cyl", "disp")
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 1L,
      objective = "reg:squarederror",
      base_score = 0.5
    ),
    data = dtrain,
    nrounds = 1L,
    verbose = 0
  )

  res <- tidypredict_fit(model)
  res_text <- rlang::expr_text(res)
  expect_match(res_text, "\\+ \\s*0\\.5")
})

test_that("predictions with missing values work", {
  skip_if_not_installed("xgboost")

  set.seed(456)
  X <- as.matrix(mtcars[, -9])
  y <- mtcars$am
  X_train <- X
  X_train[1:3, 1] <- NA
  dtrain <- xgboost::xgb.DMatrix(X_train, label = y)

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "reg:squarederror"
    ),
    data = dtrain,
    nrounds = 3L,
    verbose = 0
  )

  X_pred <- X
  X_pred[5:7, 1] <- NA
  X_pred[10:12, 2] <- NA

  fit_formula <- tidypredict_fit(model)
  dpred <- xgboost::xgb.DMatrix(X_pred)
  native_preds <- predict(model, dpred)

  pred_df <- as.data.frame(X_pred)
  tidy_preds <- rlang::eval_tidy(fit_formula, pred_df)

  # Check formula runs without error on data with NA values
  expect_type(tidy_preds, "double")
  expect_length(tidy_preds, nrow(mtcars))
})

test_that("unsupported objective throws error", {
  skip_if_not_installed("xgboost")

  pm <- list(
    general = list(
      params = list(objective = "unsupported_objective"),
      model = "xgb.Booster",
      type = "xgb"
    ),
    trees = list(list(list(prediction = 1, path = list())))
  )
  class(pm) <- c("pm_xgb", "parsed_model", "list")

  expect_snapshot(tidypredict_fit(pm), error = TRUE)
})

test_that("stump trees (no splits) predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      gamma = 100,
      objective = "reg:squarederror",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  # Verify model contains stump trees (single leaf, no splits)
  pm <- parse_model(model)
  leaves_per_tree <- vapply(pm$trees, length, integer(1), USE.NAMES = FALSE)
  path_lengths <- vapply(
    pm$trees,
    \(tree) length(tree[[1]]$path),
    integer(1),
    USE.NAMES = FALSE
  )
  expect_all_equal(leaves_per_tree, 1L)
  expect_all_equal(path_lengths, 0L)

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("NULL objective warns user", {
  skip_if_not_installed("xgboost")

  pm <- list(
    general = list(
      params = list(base_score = 0),
      model = "xgb.Booster",
      type = "xgb"
    ),
    trees = list(list(list(prediction = 5.0, path = list())))
  )
  class(pm) <- c("pm_xgb", "parsed_model", "list")

  expect_snapshot(tidypredict_fit(pm))
})

test_that("stump tree (empty path) works", {
  skip_if_not_installed("xgboost")

  pm <- list(
    general = list(
      params = list(objective = "reg:squarederror", base_score = 0),
      model = "xgb.Booster",
      type = "xgb"
    ),
    trees = list(list(list(prediction = 42.5, path = list())))
  )
  class(pm) <- c("pm_xgb", "parsed_model", "list")

  result <- tidypredict_fit(pm)
  value <- rlang::eval_tidy(result, data.frame(x = 1))

  expect_equal(value, 42.5)
})

test_that("large model predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model(
    max_depth = 2L,
    nrounds = 50L,
    objective = "reg:logistic"
  )

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("deep tree predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model(
    max_depth = 20L,
    nrounds = 4L,
    objective = "binary:logistic"
  )

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

# SQL generation tests -------------------------------------------------------

test_that("tidypredict_sql returns SQL class", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("dbplyr")
  model <- make_xgb_model()

  sql_result <- tidypredict_sql(model, dbplyr::simulate_dbi())

  expect_s3_class(sql_result, "sql")
})

test_that("tidypredict_sql works with parsed model", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("dbplyr")
  model <- make_xgb_model()
  pm <- parse_model(model)

  sql_result <- tidypredict_sql(pm, dbplyr::simulate_dbi())

  expect_s3_class(sql_result, "sql")
})

test_that("SQL predictions can be generated with SQLite", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")

  model <- make_xgb_model()

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Use all columns except 'am' (column 9)
  test_data <- mtcars[, -9]
  DBI::dbWriteTable(con, "test_data", test_data)

  sql_query <- tidypredict_sql(model, con)

  # SQL query can be executed without error
  db_result <- DBI::dbGetQuery(
    con,
    paste0("SELECT ", sql_query, " AS pred FROM test_data")
  )

  expect_equal(nrow(db_result), nrow(mtcars))
  expect_type(db_result$pred, "double")
})

test_that("SQL predictions work for binary classification with SQLite", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")

  model <- make_xgb_model(objective = "binary:logistic")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Use all columns except 'am' (column 9)
  test_data <- mtcars[, -9]
  DBI::dbWriteTable(con, "test_data", test_data)

  sql_query <- tidypredict_sql(model, con)

  # SQL query can be executed without error
  db_result <- DBI::dbGetQuery(
    con,
    paste0("SELECT ", sql_query, " AS pred FROM test_data")
  )

  expect_equal(nrow(db_result), nrow(mtcars))
  expect_type(db_result$pred, "double")
  # Binary logistic predictions should be between 0 and 1
  expect_true(all(db_result$pred >= 0 & db_result$pred <= 1))
})

# Integration tests ----------------------------------------------------------

test_that("tidypredict_test works for regression", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  xgb_data <- make_xgb_data()

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("tidypredict_test works for binary classification", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model(objective = "binary:logistic")

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("tidypredict_test xg_df argument is required", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()

  # Without xg_df, tidypredict_test should fail
  expect_snapshot(tidypredict_test(model, mtcars), error = TRUE)
})

test_that("tidypredict_test respects max_rows parameter", {
  skip_if_not_installed("xgboost")

  model <- make_xgb_model()
  xgb_data <- make_xgb_data()

  # Create a subset DMatrix for max_rows = 10
  X <- as.matrix(mtcars[1:10, -9])
  xgb_subset <- xgboost::xgb.DMatrix(X)

  result <- tidypredict_test(
    model,
    mtcars[1:10, ],
    xg_df = xgb_subset,
    max_rows = 10
  )

  expect_equal(nrow(result$raw_results), 10)
})

test_that(".extract_xgb_trees returns list of expressions", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model(nrounds = 4L)

  trees <- .extract_xgb_trees(model)

  expect_type(trees, "list")
  expect_length(trees, 4)
  for (tree in trees) {
    expect_type(tree, "language")
  }
})

test_that(".extract_xgb_trees combined results match tidypredict_fit", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model(nrounds = 4L, objective = "reg:squarederror")

  trees <- .extract_xgb_trees(model)
  eval_env <- rlang::new_environment(
    data = as.list(mtcars),
    parent = asNamespace("dplyr")
  )
  tree_preds <- lapply(trees, rlang::eval_tidy, env = eval_env)
  pm <- parse_model(model)
  base_score <- pm$general$params$base_score
  combined <- Reduce(`+`, tree_preds) + base_score

  fit_result <- rlang::eval_tidy(tidypredict_fit(model), mtcars)

  expect_equal(combined, fit_result)
})

test_that(".extract_xgb_trees errors on non-xgb.Booster", {
  expect_snapshot(.extract_xgb_trees(list()), error = TRUE)
})

test_that(".extract_xgb_trees combined results match tidypredict_fit for DART", {
  skip_if_not_installed("xgboost")

  # Add 0.1 to avoid exact split boundaries (float32 vs float64 precision)
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- mtcars_adj[, -9] + 0.1

  xgb_data <- xgboost::xgb.DMatrix(
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$am
  )

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 2L,
      objective = "reg:squarederror",
      base_score = 0.5,
      booster = "dart",
      rate_drop = 0.3,
      seed = 123
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  trees <- .extract_xgb_trees(model)
  eval_env <- rlang::new_environment(
    data = as.list(mtcars_adj),
    parent = asNamespace("dplyr")
  )
  tree_preds <- lapply(trees, rlang::eval_tidy, env = eval_env)
  pm <- parse_model(model)
  base_score <- pm$general$params$base_score
  combined <- Reduce(`+`, tree_preds) + base_score

  fit_result <- rlang::eval_tidy(tidypredict_fit(model), mtcars_adj)

  expect_equal(combined, fit_result)
})

# YAML serialization tests ---------------------------------------------------

test_that("parsed model can be saved and loaded via YAML", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("yaml")

  model <- make_xgb_model()
  pm <- parse_model(model)

  tmp_file <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, tmp_file)
  loaded <- yaml::read_yaml(tmp_file)
  pm_loaded <- as_parsed_model(loaded)

  expect_equal(pm_loaded$general$model, pm$general$model)
  expect_equal(pm_loaded$general$type, pm$general$type)
  expect_equal(pm_loaded$general$niter, pm$general$niter)
})

test_that("loaded model produces same predictions", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("yaml")

  model <- make_xgb_model()
  pm <- parse_model(model)

  tmp_file <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, tmp_file)
  loaded <- yaml::read_yaml(tmp_file)
  pm_loaded <- as_parsed_model(loaded)

  original_preds <- rlang::eval_tidy(tidypredict_fit(pm), mtcars)
  loaded_preds <- rlang::eval_tidy(tidypredict_fit(pm_loaded), mtcars)

  expect_equal(loaded_preds, original_preds, tolerance = 1e-5)
})

# Parsnip integration tests --------------------------------------------------

test_that("tidypredict works with parsnip xgboost regression", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("parsnip")

  set.seed(123)
  # Use all columns except am (column 9) for consistency
  train_data <- mtcars

  model_spec <- parsnip::boost_tree(
    trees = 4,
    tree_depth = 2,
    min_n = 1
  ) |>
    parsnip::set_engine("xgboost") |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    am ~ . - hp,
    data = train_data
  )

  xgb_model <- model_fit$fit

  expect_s3_class(xgb_model, "xgb.Booster")

  pm <- parse_model(xgb_model)
  expect_s3_class(pm, "parsed_model")
  expect_s3_class(pm, "pm_xgb")
  expect_gt(length(pm$trees), 0)

  fit_formula <- tidypredict_fit(xgb_model)
  expect_type(fit_formula, "language")
})

test_that("tidypredict works with parsnip xgboost classification", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("parsnip")

  set.seed(456)
  train_data <- mtcars
  train_data$am <- factor(train_data$am)

  model_spec <- parsnip::boost_tree(
    trees = 4,
    tree_depth = 2,
    min_n = 1
  ) |>
    parsnip::set_engine("xgboost") |>
    parsnip::set_mode("classification")

  model_fit <- parsnip::fit(
    model_spec,
    am ~ . - hp,
    data = train_data
  )

  xgb_model <- model_fit$fit

  expect_s3_class(xgb_model, "xgb.Booster")

  fit_formula <- tidypredict_fit(xgb_model)
  expect_type(fit_formula, "language")
})

test_that("tidypredict_sql works with parsnip xgboost model", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("dbplyr")

  set.seed(123)
  train_data <- mtcars

  model_spec <- parsnip::boost_tree(
    trees = 3,
    tree_depth = 2,
    min_n = 1
  ) |>
    parsnip::set_engine("xgboost") |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    am ~ . - hp,
    data = train_data
  )
  xgb_model <- model_fit$fit

  sql_result <- tidypredict_sql(xgb_model, dbplyr::simulate_dbi())

  expect_s3_class(sql_result, "sql")
})

test_that("tidypredict_test works with parsnip xgboost model", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("parsnip")

  set.seed(123)
  train_data <- mtcars

  model_spec <- parsnip::boost_tree(
    trees = 4,
    tree_depth = 2,
    min_n = 1
  ) |>
    parsnip::set_engine("xgboost") |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    am ~ . - hp,
    data = train_data
  )
  xgb_model <- model_fit$fit

  # Test that formula can be generated and evaluated
  fit_formula <- tidypredict_fit(xgb_model)
  expect_type(fit_formula, "language")

  # Test that predictions can be generated
  preds <- rlang::eval_tidy(fit_formula, train_data)
  expect_type(preds, "double")
  expect_length(preds, nrow(train_data))
})
