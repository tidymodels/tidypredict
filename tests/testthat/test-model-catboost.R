# Helper to create test model
make_catboost_model <- function(
  iterations = 10L,
  depth = 3L,
  loss_function = "RMSE"
) {
  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  catboost_catboost.train(
    pool,
    params = list(
      iterations = iterations,
      depth = depth,
      learning_rate = 0.5,
      loss_function = loss_function,
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )
}

make_multiclass_model <- function(objective = "MultiClass") {
  set.seed(42)
  X <- data.matrix(iris[, 1:4])
  y <- as.integer(iris$Species) - 1L

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(colnames(iris)[1:4])
  )

  catboost_catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = objective,
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )
}

make_categorical_model <- function() {
  set.seed(42)
  df <- data.frame(
    num_feat = rnorm(100),
    cat_feat = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
    target = rnorm(100)
  )

  X <- df[, c("num_feat", "cat_feat")]
  y <- df$target

  pool <- catboost_catboost.load_pool(X, label = y)

  catboost_catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      one_hot_max_size = 10
    )
  )
}

# Parser tests ---------------------------------------------------------------

test_that("parse_model returns correct structure", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  expect_s3_class(pm, "parsed_model")
  expect_s3_class(pm, "pm_catboost")

  expect_equal(pm$general$model, "catboost.Model")
  expect_equal(pm$general$type, "catboost")
  expect_equal(pm$general$version, 3)

  expect_gt(length(pm$trees), 0)
})

test_that("correct number of trees extracted", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  expect_length(pm$trees, 10)
})

test_that("each tree has leaves with predictions and paths", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  tree1 <- pm$trees[[1]]
  expect_gt(length(tree1), 0)

  has_required_names <- vapply(
    tree1,
    \(x) all(c("prediction", "path") %in% names(x)),
    logical(1)
  )
  expect_all_equal(has_required_names, TRUE)

  predictions <- vapply(tree1, \(x) x$prediction, double(1))
  expect_type(predictions, "double")

  path_is_list <- vapply(tree1, \(x) is.list(x$path), logical(1))
  expect_all_equal(path_is_list, TRUE)
})

test_that("symmetric trees have 2^n_splits leaves", {
  skip_if_not_installed("catboost")

  model <- make_catboost_model()
  pm <- parse_model(model)

  n_leaves <- vapply(pm$trees, length, integer(1))
  n_splits <- vapply(pm$trees, \(tree) length(tree[[1]]$path), integer(1))

  expect_equal(n_leaves, 2L^n_splits)
})

test_that("path conditions have correct structure", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  tree1 <- pm$trees[[1]]
  has_path <- vapply(tree1, function(x) length(x$path) > 0, logical(1))
  leaves_with_paths <- which(has_path)

  if (length(leaves_with_paths) > 0) {
    leaf_with_path <- tree1[[leaves_with_paths[1]]]

    cond <- leaf_with_path$path[[1]]
    expect_equal(cond$type, "conditional")
    expect_contains(names(cond), c("col", "val", "op", "missing"))
    expect_contains(c("less-equal", "more"), cond$op)
  }
})

test_that("feature names are extracted", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  expect_equal(pm$general$feature_names, c("mpg", "cyl", "disp"))
})

test_that("params are extracted", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  expect_contains(names(pm$general), "params")
  expect_equal(pm$general$params$objective, "RMSE")
})

test_that("niter and nfeatures are extracted", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  expect_equal(pm$general$niter, 10)
  expect_equal(pm$general$nfeatures, 3)
})

test_that("scale and bias are extracted correctly", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  expect_type(pm$general$scale, "integer")
  expect_type(pm$general$bias, "double")
})

test_that("path contains both more and less-equal operators", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  all_ops <- unlist(lapply(pm$trees[[1]], function(leaf) {
    vapply(leaf$path, function(p) p$op, character(1))
  }))

  expect_contains(all_ops, "more")
  expect_contains(all_ops, "less-equal")
})

test_that("leaf 0 has all less-equal conditions (binary 00...0)", {
  skip_if_not_installed("catboost")

  set.seed(42)
  X <- matrix(c(1, 1, 10, 10, 1, 10, 1, 10), ncol = 2)
  y <- c(100, 200, 300, 400)

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("x1", "x2"))
  )
  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 1L,
      depth = 2L,
      learning_rate = 1.0,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      min_data_in_leaf = 1L
    )
  )

  pm <- parse_model(model)
  leaf_0 <- pm$trees[[1]][[1]]

  # Leaf 0 (binary 00) should have all less-equal operators
  ops <- vapply(leaf_0$path, \(p) p$op, character(1))
  expect_all_equal(ops, "less-equal")
})

test_that("last leaf has all more conditions (binary 11...1)", {
  skip_if_not_installed("catboost")

  set.seed(42)
  X <- matrix(c(1, 1, 10, 10, 1, 10, 1, 10), ncol = 2)
  y <- c(100, 200, 300, 400)

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("x1", "x2"))
  )
  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 1L,
      depth = 2L,
      learning_rate = 1.0,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      min_data_in_leaf = 1L
    )
  )

  pm <- parse_model(model)
  n_leaves <- length(pm$trees[[1]])
  last_leaf <- pm$trees[[1]][[n_leaves]]

  # Last leaf (binary 11) should have all more operators
  ops <- vapply(last_leaf$path, \(p) p$op, character(1))
  expect_all_equal(ops, "more")
})

test_that("leaf index binary representation determines operator pattern", {
  skip_if_not_installed("catboost")

  set.seed(42)
  X <- matrix(c(1, 1, 10, 10, 1, 10, 1, 10), ncol = 2)
  y <- c(100, 200, 300, 400)

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("x1", "x2"))
  )
  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 1L,
      depth = 2L,
      learning_rate = 1.0,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      min_data_in_leaf = 1L
    )
  )

  pm <- parse_model(model)
  tree <- pm$trees[[1]]

  # With 2 splits, we have 4 leaves (indices 0-3)
  # Leaf index binary -> operator pattern:
  # 0 (00): [<=, <=]
  # 1 (01): [>, <=]
  # 2 (10): [<=, >]
  # 3 (11): [>, >]

  for (leaf_idx in 0:3) {
    leaf <- tree[[leaf_idx + 1]]
    n_splits <- length(leaf$path)

    for (split_idx in seq_len(n_splits)) {
      bit_val <- bitwAnd(bitwShiftR(leaf_idx, split_idx - 1L), 1L)
      expected_op <- if (bit_val == 1L) "more" else "less-equal"
      expect_equal(
        leaf$path[[split_idx]]$op,
        expected_op,
        info = sprintf("leaf %d, split %d", leaf_idx, split_idx)
      )
    }
  }
})

test_that("single split tree produces correct paths", {
  skip_if_not_installed("catboost")

  set.seed(42)
  X <- matrix(c(1, 10), ncol = 1)
  y <- c(0, 100)

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("x"))
  )
  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 1L,
      depth = 1L,
      learning_rate = 1.0,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      min_data_in_leaf = 1L
    )
  )

  pm <- parse_model(model)

  expect_length(pm$trees[[1]], 2) # 2^1 = 2 leaves

  # Leaf 0: x <= border
  expect_equal(pm$trees[[1]][[1]]$path[[1]]$op, "less-equal")
  expect_equal(pm$trees[[1]][[1]]$path[[1]]$col, "x")

  # Leaf 1: x > border
  expect_equal(pm$trees[[1]][[2]]$path[[1]]$op, "more")
  expect_equal(pm$trees[[1]][[2]]$path[[1]]$col, "x")
})

test_that("stump trees (depth=0) are parsed correctly", {
  skip_if_not_installed("catboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 3L,
      depth = 0L,
      learning_rate = 0.5,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )

  pm <- parse_model(model)

  # Depth 0 trees should have no splits
  expect_equal(length(pm$trees[[1]][[1]]$path), 0)
})

test_that("model without explicit feature names still works", {
  skip_if_not_installed("catboost")

  set.seed(789)
  X <- data.matrix(mtcars[, c("mpg", "cyl")])
  y <- mtcars$hp

  # Create pool WITHOUT specifying feature_names
  pool <- catboost_catboost.load_pool(X, label = y)

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 3L,
      depth = 2L,
      learning_rate = 1.0,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      min_data_in_leaf = 1L
    )
  )

  pm <- parse_model(model)

  expect_s3_class(pm, "pm_catboost")
  expect_length(pm$trees, 3)
  expect_equal(pm$general$nfeatures, 2)
  expect_type(pm$general$feature_names, "character")
})

test_that("deeper tree paths are traced correctly", {
  skip_if_not_installed("catboost")

  # Create data that forces a 3-split tree
  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 3), ncol = 3)
  colnames(X) <- c("a", "b", "c")
  y <- X[, 1] + X[, 2] * 2 + X[, 3] * 3 + rnorm(n, sd = 0.1)

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("a", "b", "c"))
  )
  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 1L,
      depth = 3L,
      learning_rate = 1.0,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      min_data_in_leaf = 1L
    )
  )

  pm <- parse_model(model)
  tree <- pm$trees[[1]]

  # All leaves should have 3 conditions in their path (if tree is full depth)
  n_splits <- length(tree[[1]]$path)
  expect_lte(n_splits, 3)

  # Verify leaf count matches 2^n_splits
  expect_length(tree, 2^n_splits)
})

test_that("num_class is extracted for multiclass model", {
  skip_if_not_installed("catboost")

  model <- make_multiclass_model()
  pm <- parse_model(model)

  expect_equal(pm$general$num_class, 3)
})

test_that("num_class is 1 for regression model", {
  skip_if_not_installed("catboost")

  model <- make_catboost_model()
  pm <- parse_model(model)

  expect_equal(pm$general$num_class, 1)
})

test_that("categorical features are extracted from model", {
  skip_if_not_installed("catboost")

  model <- make_categorical_model()
  pm <- parse_model(model)

  expect_length(pm$general$cat_features, 1)
  expect_equal(pm$general$cat_feature_names, "cat_feat")
  expect_type(pm$general$cat_features[[1]]$hash_values, "integer")
})

# Fit formula tests -------------------------------------------------------

test_that("tidypredict_fit returns language object", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()

  result <- tidypredict_fit(model)

  expect_type(result, "language")
})

test_that("tidypredict_fit works on parsed model", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  result <- tidypredict_fit(pm)

  expect_type(result, "language")
})

test_that("produced case_when uses .default", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()

  fit <- tidypredict_fit(model)
  fit_text <- rlang::expr_text(fit)

  expect_match(fit_text, "\\.default")
})

test_that("regression predictions match catboost.predict", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()

  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  pool <- catboost_catboost.load_pool(X)

  native_preds <- catboost_catboost.predict(model, pool)
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("model with custom scale works correctly", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  # Simulate a calibrated model with scale != 1
  # (set_scale_and_bias is Python-only, so we modify the parsed model directly)
  original_scale <- pm$general$scale
  bias <- pm$general$bias
  pm$general$scale <- 0.5

  formula <- tidypredict_fit(pm)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  # Formula is: scale * tree_sum + bias
  # With scale=0.5: 0.5 * tree_sum + bias
  # With scale=1: tree_sum + bias (unscaled)
  # So: scaled = 0.5 * (unscaled - bias) + bias = 0.5 * unscaled + 0.5 * bias
  pm$general$scale <- original_scale
  formula_unscaled <- tidypredict_fit(pm)
  unscaled_preds <- rlang::eval_tidy(formula_unscaled, mtcars)

  expected <- 0.5 * unscaled_preds + 0.5 * bias
  expect_equal(tidy_preds, expected, tolerance = 1e-10)
})

test_that("Logloss predictions match catboost.predict", {
  skip_if_not_installed("catboost")

  set.seed(456)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- as.numeric(mtcars$am)

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "Logloss",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )

  native_preds <- catboost_catboost.predict(
    model,
    pool,
    prediction_type = "Probability"
  )
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("MAE predictions match catboost.predict", {
  skip_if_not_installed("catboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "MAE",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )

  native_preds <- catboost_catboost.predict(model, pool)
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("Quantile predictions match catboost.predict", {
  skip_if_not_installed("catboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "Quantile:alpha=0.5",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )

  native_preds <- catboost_catboost.predict(model, pool)
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("MAPE predictions match catboost.predict", {
  skip_if_not_installed("catboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "MAPE",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )

  native_preds <- catboost_catboost.predict(model, pool)
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("Poisson predictions match catboost.predict", {
  skip_if_not_installed("catboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$carb

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "Poisson",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )

  native_preds <- catboost_catboost.predict(model, pool)
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("Huber predictions match catboost.predict (#188)", {
  skip_if_not_installed("catboost")

  model <- make_catboost_model(loss_function = "Huber:delta=1.0")
  pool <- catboost_catboost.load_pool(
    data.matrix(mtcars[, c("mpg", "cyl", "disp")]),
    label = mtcars$hp,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  native_preds <- catboost_catboost.predict(model, pool)
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds)
})

test_that("LogCosh predictions match catboost.predict (#188)", {
  skip_if_not_installed("catboost")

  model <- make_catboost_model(loss_function = "LogCosh")
  pool <- catboost_catboost.load_pool(
    data.matrix(mtcars[, c("mpg", "cyl", "disp")]),
    label = mtcars$hp,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  native_preds <- catboost_catboost.predict(model, pool)
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds)
})

test_that("Expectile predictions match catboost.predict (#188)", {
  skip_if_not_installed("catboost")

  model <- make_catboost_model(loss_function = "Expectile:alpha=0.5")
  pool <- catboost_catboost.load_pool(
    data.matrix(mtcars[, c("mpg", "cyl", "disp")]),
    label = mtcars$hp,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  native_preds <- catboost_catboost.predict(model, pool)
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds)
})

test_that("Tweedie predictions match catboost.predict (#188)", {
  skip_if_not_installed("catboost")

  model <- make_catboost_model(loss_function = "Tweedie:variance_power=1.5")
  pool <- catboost_catboost.load_pool(
    data.matrix(mtcars[, c("mpg", "cyl", "disp")]),
    label = mtcars$hp,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  native_preds <- catboost_catboost.predict(model, pool)
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds)
})

test_that("CrossEntropy predictions match catboost.predict", {
  skip_if_not_installed("catboost")

  set.seed(456)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- as.numeric(mtcars$am)

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "CrossEntropy",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )

  native_preds <- catboost_catboost.predict(
    model,
    pool,
    prediction_type = "Probability"
  )
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("stump trees (depth=0) predictions work correctly", {
  skip_if_not_installed("catboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 3L,
      depth = 0L,
      learning_rate = 0.5,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )

  formula <- tidypredict_fit(model)
  expect_type(formula, "language")

  native_preds <- catboost_catboost.predict(model, pool)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  # Stump trees produce constant predictions
  # The formula returns a scalar when no data columns are referenced
  expect_equal(tidy_preds[[1]], native_preds[[1]], tolerance = 1e-10)
})

test_that("model with NaN values handles missing correctly with nan_mode Min", {
  skip_if_not_installed("catboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  X[1, 1] <- NA
  y <- mtcars$hp

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 5L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      nan_mode = "Min"
    )
  )

  formula <- tidypredict_fit(model)
  expect_type(formula, "language")

  test_data <- mtcars
  test_data$mpg[1] <- NA

  native_preds <- catboost_catboost.predict(model, pool)
  tidy_preds <- rlang::eval_tidy(formula, test_data)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("model with NaN values handles missing correctly with nan_mode Max", {
  skip_if_not_installed("catboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  X[1, 1] <- NA
  y <- mtcars$hp

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 5L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      nan_mode = "Max"
    )
  )

  formula <- tidypredict_fit(model)
  expect_type(formula, "language")

  test_data <- mtcars
  test_data$mpg[1] <- NA

  native_preds <- catboost_catboost.predict(model, pool)
  tidy_preds <- rlang::eval_tidy(formula, test_data)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("unsupported objective throws error", {
  skip_if_not_installed("catboost")

  pm <- list(
    general = list(
      params = list(objective = "UnsupportedObjective"),
      model = "catboost.Model",
      type = "catboost"
    ),
    trees = list(list(list(prediction = 1, path = list())))
  )
  class(pm) <- c("pm_catboost", "parsed_model", "list")

  expect_snapshot(tidypredict_fit(pm), error = TRUE)
})

test_that("empty trees throws error", {
  skip_if_not_installed("catboost")

  pm <- list(
    general = list(
      params = list(objective = "RMSE"),
      model = "catboost.Model",
      type = "catboost"
    ),
    trees = list()
  )
  class(pm) <- c("pm_catboost", "parsed_model", "list")

  expect_snapshot(tidypredict_fit(pm), error = TRUE)
})

# SQL generation tests ----------------------------------------------------

test_that("tidypredict_sql returns SQL class", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()

  sql <- tidypredict_sql(model, dbplyr::simulate_sqlite())

  expect_s3_class(sql, "sql")
})

test_that("SQL predictions match native predictions with SQLite", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  model <- make_catboost_model()

  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  pool <- catboost_catboost.load_pool(X)
  native_preds <- catboost_catboost.predict(model, pool)

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbWriteTable(con, "mtcars", mtcars)

  sql <- tidypredict_sql(model, con)
  query <- paste0("SELECT ", sql, " AS pred FROM mtcars")
  sql_preds <- DBI::dbGetQuery(con, query)$pred

  expect_equal(sql_preds, native_preds, tolerance = 1e-10)
})

# Integration tests -------------------------------------------------------

test_that("tidypredict_test works for regression", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])

  result <- tidypredict_test(model, xg_df = X)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("tidypredict_test works for binary classification", {
  skip_if_not_installed("catboost")

  set.seed(456)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- as.numeric(mtcars$am)

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "Logloss",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )

  result <- tidypredict_test(model, xg_df = X)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("tidypredict_test requires matrix", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()

  expect_snapshot(tidypredict_test(model), error = TRUE)
})

test_that(".extract_catboost_trees returns list of expressions", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()

  trees <- .extract_catboost_trees(model)

  expect_type(trees, "list")
  expect_length(trees, 10)
  expect_type(trees[[1]], "language")
})

test_that(".extract_catboost_trees combined results match tidypredict_fit", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  test_data <- mtcars[, c("mpg", "cyl", "disp")]

  trees <- .extract_catboost_trees(model)
  eval_env <- rlang::new_environment(
    data = as.list(test_data),
    parent = asNamespace("dplyr")
  )
  tree_preds <- lapply(trees, rlang::eval_tidy, env = eval_env)
  pm <- parse_model(model)
  scale <- pm$general$scale %||% 1
  bias <- pm$general$bias %||% 0
  combined <- Reduce(`+`, tree_preds) * scale + bias

  fit_result <- rlang::eval_tidy(tidypredict_fit(model), test_data)

  expect_equal(combined, fit_result)
})

test_that(".extract_catboost_trees errors on non-catboost model", {
  expect_snapshot(
    .extract_catboost_trees(lm(mpg ~ wt, data = mtcars)),
    error = TRUE
  )
})

# YAML serialization tests ------------------------------------------------

test_that("parsed model can be saved and loaded via YAML", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("yaml")

  model <- make_catboost_model()
  pm <- parse_model(model)

  tmp_file <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, tmp_file)
  loaded <- yaml::read_yaml(tmp_file)
  class(loaded) <- class(pm)

  expect_equal(loaded$general$model, pm$general$model)
  expect_equal(loaded$general$type, pm$general$type)
  expect_equal(loaded$general$niter, pm$general$niter)
})

test_that("loaded model produces same predictions", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("yaml")

  model <- make_catboost_model()
  pm <- parse_model(model)

  tmp_file <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, tmp_file)
  loaded <- yaml::read_yaml(tmp_file)
  class(loaded) <- class(pm)

  original_preds <- rlang::eval_tidy(tidypredict_fit(pm), mtcars)
  loaded_preds <- rlang::eval_tidy(tidypredict_fit(loaded), mtcars)

  expect_equal(loaded_preds, original_preds, tolerance = 1e-6)
})

# Multiclass tests ---------------------------------------------------------

test_that("MultiClass predictions match catboost.predict", {
  skip_if_not_installed("catboost")

  model <- make_multiclass_model("MultiClass")
  X <- data.matrix(iris[, 1:4])
  pool <- catboost_catboost.load_pool(X)

  native_preds <- catboost_catboost.predict(
    model,
    pool,
    prediction_type = "Probability"
  )

  formulas <- tidypredict_fit(model)
  tidy_preds <- lapply(formulas, function(f) rlang::eval_tidy(f, iris))
  tidy_matrix <- do.call(cbind, tidy_preds)

  expect_equal(unname(tidy_matrix), unname(native_preds), tolerance = 1e-10)
})

test_that("MultiClassOneVsAll predictions match catboost.predict", {
  skip_if_not_installed("catboost")

  model <- make_multiclass_model("MultiClassOneVsAll")
  X <- data.matrix(iris[, 1:4])
  pool <- catboost_catboost.load_pool(X)

  native_preds <- catboost_catboost.predict(
    model,
    pool,
    prediction_type = "Probability"
  )

  formulas <- tidypredict_fit(model)
  tidy_preds <- lapply(formulas, function(f) rlang::eval_tidy(f, iris))
  tidy_matrix <- do.call(cbind, tidy_preds)

  expect_equal(unname(tidy_matrix), unname(native_preds), tolerance = 1e-10)
})

test_that("multiclass output is list with correct names", {
  skip_if_not_installed("catboost")

  model <- make_multiclass_model()
  formulas <- tidypredict_fit(model)

  expect_type(formulas, "list")
  expect_length(formulas, 3)
  expect_named(formulas, c("class_0", "class_1", "class_2"))
})

test_that("MultiClass probabilities sum to 1", {
  skip_if_not_installed("catboost")

  model <- make_multiclass_model("MultiClass")
  formulas <- tidypredict_fit(model)

  tidy_preds <- lapply(formulas, function(f) rlang::eval_tidy(f, iris))
  row_sums <- Reduce(`+`, tidy_preds)

  expect_equal(row_sums, rep(1, nrow(iris)), tolerance = 1e-10)
})

test_that("tidypredict_test works for multiclass", {
  skip_if_not_installed("catboost")

  model <- make_multiclass_model()
  X <- data.matrix(iris[, 1:4])

  result <- tidypredict_test(model, xg_df = X)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("multiclass model requires num_class >= 2", {
  skip_if_not_installed("catboost")

  pm <- list(
    general = list(
      params = list(objective = "MultiClass"),
      model = "catboost.Model",
      type = "catboost",
      num_class = 1
    ),
    trees = list(list(list(prediction = 1, path = list())))
  )
  class(pm) <- c("pm_catboost", "parsed_model", "list")

  expect_snapshot(tidypredict_fit(pm), error = TRUE)
})

test_that("multiclass stump trees (depth=0) work correctly", {
  skip_if_not_installed("catboost")

  set.seed(42)
  X <- data.matrix(iris[, 1:4])
  y <- as.integer(iris$Species) - 1L

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(colnames(iris)[1:4])
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 3L,
      depth = 0L,
      learning_rate = 0.5,
      loss_function = "MultiClass",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )

  pm <- parse_model(model)
  expect_equal(pm$general$num_class, 3)

  formulas <- tidypredict_fit(model)
  expect_type(formulas, "list")
  expect_length(formulas, 3)
})

# Categorical feature tests -----------------------------------------------

test_that("set_catboost_categories adds hash mapping", {
  skip_if_not_installed("catboost")

  set.seed(42)
  df <- data.frame(
    num_feat = rnorm(100),
    cat_feat = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
    target = rnorm(100)
  )

  X <- df[, c("num_feat", "cat_feat")]
  y <- df$target

  pool <- catboost_catboost.load_pool(X, label = y)

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      one_hot_max_size = 10
    )
  )

  pm <- parse_model(model)
  pm <- set_catboost_categories(pm, model, df)

  expect_type(pm$general$cat_features[[1]]$hash_to_category, "list")
  expect_contains(
    unlist(pm$general$cat_features[[1]]$hash_to_category),
    c("A", "B", "C")
  )
})

test_that("set_catboost_categories validates parsed_model argument", {
  skip_if_not_installed("catboost")

  model <- make_catboost_model()

  expect_snapshot(
    set_catboost_categories("not a parsed model", model, data.frame()),
    error = TRUE
  )
})

test_that("set_catboost_categories validates model argument", {
  skip_if_not_installed("catboost")

  model <- make_catboost_model()
  pm <- parse_model(model)

  expect_snapshot(
    set_catboost_categories(pm, "not a model", data.frame()),
    error = TRUE
  )
})

test_that("set_catboost_categories returns early when no categorical features", {
  skip_if_not_installed("catboost")

  model <- make_catboost_model()
  pm <- parse_model(model)

  result <- set_catboost_categories(pm, model, mtcars)

  expect_identical(result, pm)
})

test_that("set_catboost_categories errors when column not found in data", {
  skip_if_not_installed("catboost")

  model <- make_categorical_model()
  pm <- parse_model(model)

  wrong_data <- data.frame(num_feat = 1, other_col = factor("A"))

  expect_snapshot(set_catboost_categories(pm, model, wrong_data), error = TRUE)
})

test_that("set_catboost_categories errors when column is not a factor", {
  skip_if_not_installed("catboost")

  model <- make_categorical_model()
  pm <- parse_model(model)

  wrong_data <- data.frame(num_feat = 1, cat_feat = "A")

  expect_snapshot(set_catboost_categories(pm, model, wrong_data), error = TRUE)
})

test_that("categorical predictions match catboost.predict", {
  skip_if_not_installed("catboost")
  skip("one_hot_max_size categorical handling needs investigation")

  set.seed(42)
  df <- data.frame(
    num_feat = rnorm(100),
    cat_feat = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
    target = rnorm(100)
  )

  X <- df[, c("num_feat", "cat_feat")]
  y <- df$target

  pool <- catboost_catboost.load_pool(X, label = y)

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      one_hot_max_size = 10
    )
  )

  pm <- parse_model(model)
  pm <- set_catboost_categories(pm, model, df)

  formula <- tidypredict_fit(pm)
  tidy_preds <- rlang::eval_tidy(formula, df)

  native_preds <- catboost_catboost.predict(model, pool)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("categorical features without mapping throws error", {
  skip_if_not_installed("catboost")

  model <- make_categorical_model()
  pm <- parse_model(model)

  expect_snapshot(tidypredict_fit(pm), error = TRUE)
})

test_that("categorical features SQL generation works", {
  skip_if_not_installed("catboost")

  set.seed(42)
  df <- data.frame(
    num_feat = rnorm(100),
    cat_feat = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
    target = rnorm(100)
  )

  X <- df[, c("num_feat", "cat_feat")]
  y <- df$target

  pool <- catboost_catboost.load_pool(X, label = y)

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      one_hot_max_size = 10
    )
  )

  pm <- parse_model(model)
  pm <- set_catboost_categories(pm, model, df)

  sql <- tidypredict_sql(pm, dbplyr::simulate_dbi())

  expect_s3_class(sql, "sql")
  sql_str <- as.character(sql)
  expect_match(sql_str, "cat_feat")
  expect_match(sql_str, "=")
})

# Parsnip/bonsai tests -----------------------------------------------

test_that("tidypredict works with parsnip/bonsai catboost regression", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("bonsai")

  set.seed(123)
  train_data <- mtcars[, c("hp", "mpg", "cyl", "disp")]

  model_spec <- parsnip::boost_tree(
    trees = 5,
    tree_depth = 3,
    min_n = 1
  ) |>
    parsnip::set_engine("catboost", logging_level = "Silent") |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    hp ~ mpg + cyl + disp,
    data = train_data
  )

  cb_model <- model_fit$fit

  expect_s3_class(cb_model, "catboost.Model")

  pm <- parse_model(cb_model)
  expect_s3_class(pm, "parsed_model")
  expect_s3_class(pm, "pm_catboost")
  expect_gt(length(pm$trees), 0)

  fit_formula <- tidypredict_fit(cb_model)
  expect_type(fit_formula, "language")

  X <- data.matrix(train_data[, c("mpg", "cyl", "disp")])
  pool <- catboost_catboost.load_pool(X)
  native_preds <- catboost_catboost.predict(cb_model, pool)
  tidy_preds <- rlang::eval_tidy(fit_formula, train_data)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("tidypredict works with parsnip/bonsai catboost classification", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("bonsai")

  set.seed(456)
  train_data <- mtcars[, c("am", "mpg", "cyl", "disp")]
  train_data$am <- factor(train_data$am)

  model_spec <- parsnip::boost_tree(
    trees = 5,
    tree_depth = 3,
    min_n = 1
  ) |>
    parsnip::set_engine("catboost", logging_level = "Silent") |>
    parsnip::set_mode("classification")

  model_fit <- parsnip::fit(
    model_spec,
    am ~ mpg + cyl + disp,
    data = train_data
  )

  cb_model <- model_fit$fit

  expect_s3_class(cb_model, "catboost.Model")

  fit_formula <- tidypredict_fit(cb_model)
  expect_type(fit_formula, "language")

  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  pool <- catboost_catboost.load_pool(X)
  native_preds <- catboost_catboost.predict(
    cb_model,
    pool,
    prediction_type = "Probability"
  )
  tidy_preds <- rlang::eval_tidy(fit_formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("tidypredict_sql works with parsnip/bonsai catboost model", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("bonsai")
  skip_if_not_installed("dbplyr")

  set.seed(123)
  train_data <- mtcars[, c("hp", "mpg", "cyl", "disp")]

  model_spec <- parsnip::boost_tree(
    trees = 3,
    tree_depth = 2,
    min_n = 1
  ) |>
    parsnip::set_engine("catboost", logging_level = "Silent") |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    hp ~ mpg + cyl + disp,
    data = train_data
  )
  cb_model <- model_fit$fit

  sql_result <- tidypredict_sql(cb_model, dbplyr::simulate_dbi())

  expect_s3_class(sql_result, "sql")
})

test_that("parsnip/bonsai catboost with categorical features works automatically", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("bonsai")
  # TODO: one_hot_max_size handling needs investigation with nested case_when
  skip("one_hot_max_size categorical handling needs investigation")

  set.seed(42)
  train_data <- data.frame(
    num_feat = rnorm(100),
    cat_feat = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
    target = rnorm(100)
  )

  model_spec <- parsnip::boost_tree(
    trees = 5,
    tree_depth = 3,
    min_n = 1
  ) |>
    parsnip::set_engine(
      "catboost",
      logging_level = "Silent",
      one_hot_max_size = 10
    ) |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    target ~ num_feat + cat_feat,
    data = train_data
  )

  fit_formula <- tidypredict_fit(model_fit)
  expect_type(fit_formula, "language")

  parsnip_preds <- predict(model_fit, train_data)$.pred
  tidy_preds <- rlang::eval_tidy(fit_formula, train_data)

  expect_equal(tidy_preds, parsnip_preds, tolerance = 1e-6)
})

test_that("parsnip/bonsai catboost categorical SQL generation works automatically", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("bonsai")
  skip_if_not_installed("dbplyr")

  set.seed(42)
  train_data <- data.frame(
    num_feat = rnorm(100),
    cat_feat = factor(sample(c("X", "Y", "Z"), 100, replace = TRUE)),
    target = rnorm(100)
  )

  model_spec <- parsnip::boost_tree(
    trees = 5,
    tree_depth = 3,
    min_n = 1
  ) |>
    parsnip::set_engine(
      "catboost",
      logging_level = "Silent",
      one_hot_max_size = 10
    ) |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    target ~ num_feat + cat_feat,
    data = train_data
  )

  sql <- tidypredict_sql(model_fit, dbplyr::simulate_dbi())

  expect_s3_class(sql, "sql")
  sql_str <- as.character(sql)
  expect_match(sql_str, "cat_feat")
})

test_that("multiple categorical features work correctly", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("bonsai")
  # TODO: one_hot_max_size handling needs investigation with nested case_when
  skip("one_hot_max_size categorical handling needs investigation")

  set.seed(42)
  df <- data.frame(
    num_feat = rnorm(100),
    cat_feat1 = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
    cat_feat2 = factor(sample(c("X", "Y", "Z"), 100, replace = TRUE)),
    target = rnorm(100)
  )

  model_spec <- parsnip::boost_tree(
    trees = 10,
    tree_depth = 3,
    min_n = 1
  ) |>
    parsnip::set_engine(
      "catboost",
      logging_level = "Silent",
      one_hot_max_size = 10
    ) |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    target ~ num_feat + cat_feat1 + cat_feat2,
    data = df
  )

  pm <- parse_model(model_fit$fit)

  expect_length(pm$general$cat_features, 2)
  expect_equal(pm$general$cat_feature_names, c("cat_feat1", "cat_feat2"))

  formula <- tidypredict_fit(model_fit)
  tidy_preds <- rlang::eval_tidy(formula, df)

  parsnip_preds <- predict(model_fit, df)$.pred

  expect_equal(tidy_preds, parsnip_preds, tolerance = 1e-6)
})

test_that("parsnip categorical predictions match parsnip predictions", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("bonsai")
  # TODO: one_hot_max_size handling needs investigation with nested case_when
  skip("one_hot_max_size categorical handling needs investigation")

  set.seed(42)
  train_data <- data.frame(
    num_feat = rnorm(100),
    cat_feat = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
    target = rnorm(100)
  )

  model_spec <- parsnip::boost_tree(
    trees = 5,
    tree_depth = 3,
    min_n = 1
  ) |>
    parsnip::set_engine(
      "catboost",
      logging_level = "Silent",
      one_hot_max_size = 10
    ) |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    target ~ num_feat + cat_feat,
    data = train_data
  )

  formula <- tidypredict_fit(model_fit)
  tidy_preds <- rlang::eval_tidy(formula, train_data)

  parsnip_preds <- predict(model_fit, train_data)$.pred

  expect_equal(tidy_preds, parsnip_preds, tolerance = 1e-6)
})

test_that("parsnip model without xlevels throws error", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("bonsai")

  set.seed(42)
  train_data <- data.frame(
    num_feat = rnorm(100),
    cat_feat = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
    target = rnorm(100)
  )

  model_spec <- parsnip::boost_tree(
    trees = 5,
    tree_depth = 3,
    min_n = 1
  ) |>
    parsnip::set_engine(
      "catboost",
      logging_level = "Silent",
      one_hot_max_size = 10
    ) |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    target ~ num_feat + cat_feat,
    data = train_data
  )

  model_fit$preproc$xlevels <- NULL

  expect_snapshot(tidypredict_fit(model_fit), error = TRUE)
})

test_that("model with only categorical features works", {
  skip_if_not_installed("catboost")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("bonsai")
  # TODO: one_hot_max_size handling needs investigation with nested case_when
  skip("one_hot_max_size categorical handling needs investigation")

  set.seed(42)
  df <- data.frame(
    cat_feat1 = factor(sample(c("A", "B", "C"), 100, replace = TRUE)),
    cat_feat2 = factor(sample(c("X", "Y", "Z"), 100, replace = TRUE)),
    target = rnorm(100)
  )

  model_spec <- parsnip::boost_tree(
    trees = 5,
    tree_depth = 3,
    min_n = 1
  ) |>
    parsnip::set_engine(
      "catboost",
      logging_level = "Silent",
      one_hot_max_size = 10
    ) |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    target ~ cat_feat1 + cat_feat2,
    data = df
  )

  pm <- parse_model(model_fit$fit)

  expect_length(pm$general$feature_names, 0)
  expect_length(pm$general$cat_features, 2)

  formula <- tidypredict_fit(model_fit)
  tidy_preds <- rlang::eval_tidy(formula, df)

  parsnip_preds <- predict(model_fit, df)$.pred

  expect_equal(tidy_preds, parsnip_preds, tolerance = 1e-6)
})

# Non-oblivious tree tests (Depthwise/Lossguide) -----------------------------

test_that("Depthwise regression predictions match (#187)", {
  skip_if_not_installed("catboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 5L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "RMSE",
      grow_policy = "Depthwise",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )

  pm <- parse_model(model)
  expect_equal(pm$general$tree_type, "nonoblivious")

  native_preds <- catboost_catboost.predict(model, pool)
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("non-oblivious stump trees (depth=0) work correctly", {
  skip_if_not_installed("catboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 3L,
      depth = 0L,
      learning_rate = 0.5,
      loss_function = "RMSE",
      grow_policy = "Depthwise",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )

  pm <- parse_model(model)
  expect_equal(pm$general$tree_type, "nonoblivious")

  formula <- tidypredict_fit(model)
  expect_type(formula, "language")

  native_preds <- catboost_catboost.predict(model, pool)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)
  expect_equal(tidy_preds[[1]], native_preds[[1]], tolerance = 1e-10)
})

test_that("Lossguide regression predictions match (#187)", {
  skip_if_not_installed("catboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 5L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "RMSE",
      grow_policy = "Lossguide",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )

  pm <- parse_model(model)
  expect_equal(pm$general$tree_type, "nonoblivious")

  native_preds <- catboost_catboost.predict(model, pool)
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})

test_that("Depthwise multiclass predictions match (#187)", {
  skip_if_not_installed("catboost")

  set.seed(123)
  X <- data.matrix(iris[, 1:4])
  colnames(X) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  y <- as.integer(iris$Species) - 1L

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(colnames(X))
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 3L,
      depth = 2L,
      learning_rate = 0.5,
      loss_function = "MultiClass",
      grow_policy = "Depthwise",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )

  pm <- parse_model(model)
  expect_equal(pm$general$tree_type, "nonoblivious")
  expect_equal(pm$general$num_class, 3)

  native_preds <- catboost_catboost.predict(
    model,
    pool,
    prediction_type = "Probability"
  )

  formulas <- tidypredict_fit(model)
  tidy_preds <- lapply(formulas, function(f) rlang::eval_tidy(f, iris))
  tidy_matrix <- do.call(cbind, tidy_preds)

  expect_equal(unname(tidy_matrix), unname(native_preds), tolerance = 1e-10)
})

test_that("Depthwise binary classification predictions match (#187)", {
  skip_if_not_installed("catboost")

  set.seed(456)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- as.numeric(mtcars$am)

  pool <- catboost_catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  model <- catboost_catboost.train(
    pool,
    params = list(
      iterations = 5L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "Logloss",
      grow_policy = "Depthwise",
      logging_level = "Silent",
      allow_writing_files = FALSE
    )
  )

  native_preds <- catboost_catboost.predict(
    model,
    pool,
    prediction_type = "Probability"
  )
  formula <- tidypredict_fit(model)
  tidy_preds <- rlang::eval_tidy(formula, mtcars)

  expect_equal(tidy_preds, native_preds, tolerance = 1e-10)
})
