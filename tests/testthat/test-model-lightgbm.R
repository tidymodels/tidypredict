# Helper to create test model
make_lgb_model <- function() {
  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp
  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )
  lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 1.0,
      objective = "regression",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 10L,
    verbose = -1L
  )
}

test_that("parse_model returns correct structure", {
  skip_if_not_installed("lightgbm")
  model <- make_lgb_model()
  pm <- parse_model(model)

  expect_s3_class(pm, "parsed_model")
  expect_s3_class(pm, "pm_lgb")

  expect_equal(pm$general$model, "lgb.Booster")
  expect_equal(pm$general$type, "lgb")
  expect_equal(pm$general$version, 1)

  expect_gt(length(pm$trees), 0)
})

test_that("correct number of trees extracted", {
  skip_if_not_installed("lightgbm")
  model <- make_lgb_model()
  pm <- parse_model(model)

  expect_equal(length(pm$trees), 10)
})

test_that("each tree has leaves with predictions and paths", {
  skip_if_not_installed("lightgbm")
  model <- make_lgb_model()
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
  skip_if_not_installed("lightgbm")
  model <- make_lgb_model()
  pm <- parse_model(model)

  tree1 <- pm$trees[[1]]
  leaves_with_paths <- which(sapply(tree1, function(x) length(x$path) > 0))

  if (length(leaves_with_paths) > 0) {
    leaf_with_path <- tree1[[leaves_with_paths[1]]]

    cond <- leaf_with_path$path[[1]]
    expect_equal(cond$type, "conditional")
    expect_contains(names(cond), c("col", "val", "op", "missing"))
    expect_contains(c("less-equal", "more"), cond$op)
  }
})

test_that("feature names are extracted", {
  skip_if_not_installed("lightgbm")
  model <- make_lgb_model()
  pm <- parse_model(model)

  expect_equal(pm$general$feature_names, c("mpg", "cyl", "disp"))
})

test_that("params are extracted", {
  skip_if_not_installed("lightgbm")
  model <- make_lgb_model()
  pm <- parse_model(model)

  expect_contains(names(pm$general), "params")
  expect_equal(pm$general$params$objective, "regression")
})

test_that("niter and nfeatures are extracted", {
  skip_if_not_installed("lightgbm")
  model <- make_lgb_model()
  pm <- parse_model(model)

  expect_equal(pm$general$niter, 10)
  expect_equal(pm$general$nfeatures, 3)
})

test_that("model without explicit colnames still works", {
  skip_if_not_installed("lightgbm")

  set.seed(789)
  X <- data.matrix(mtcars[, c("mpg", "cyl")])
  y <- mtcars$hp

  # Create dataset WITHOUT specifying colnames
  dtrain <- lightgbm::lgb.Dataset(X, label = y)

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

  pm <- parse_model(model)

  expect_s3_class(pm, "pm_lgb")
  expect_length(pm$trees, 3)
  # Feature names should be auto-generated (Column_0, Column_1, etc.)
  expect_equal(pm$general$nfeatures, 2)
  expect_type(pm$general$feature_names, "character")
})

# Edge case tests using hand-crafted tree data frames
# These test the internal parsing functions directly

test_that("children map correctly identifies left and right children", {
  # Hand-crafted tree: simple binary tree

  #        split_0
  #       /       \
  #    leaf_0   leaf_1
  #
  # Pre-order: split_0 (row 1), leaf_0 (row 2), leaf_1 (row 3)
  tree_df <- data.frame(
    tree_index = c(0L, 0L, 0L),
    split_index = c(0L, NA, NA),
    split_feature = c("x1", NA, NA),
    node_parent = c(NA, NA, NA),
    leaf_index = c(NA, 0L, 1L),
    leaf_parent = c(NA, 0L, 0L),
    threshold = c(5.0, NA, NA),
    decision_type = c("<=", NA, NA),
    default_left = c("TRUE", NA, NA),
    leaf_value = c(NA, 10.0, 20.0),
    stringsAsFactors = FALSE
  )

  children_map <- tidypredict:::get_lgb_children_map(tree_df)

  expect_equal(children_map[["0"]], c(2L, 3L))
})

test_that("left child gets less-equal operator", {
  # Simple tree where we trace from leaf_0 (left child of root)
  tree_df <- data.frame(
    tree_index = c(0L, 0L, 0L),
    split_index = c(0L, NA, NA),
    split_feature = c("feature_a", NA, NA),
    node_parent = c(NA, NA, NA),
    leaf_index = c(NA, 0L, 1L),
    leaf_parent = c(NA, 0L, 0L),
    threshold = c(10.0, NA, NA),
    decision_type = c("<=", NA, NA),
    default_left = c("TRUE", NA, NA),
    leaf_value = c(NA, 100.0, 200.0),
    stringsAsFactors = FALSE
  )

  tree_result <- tidypredict:::get_lgb_tree(tree_df)

  leaf_0 <- tree_result[[1]]
  expect_equal(leaf_0$prediction, 100.0)
  expect_length(leaf_0$path, 1)
  expect_equal(leaf_0$path[[1]]$op, "less-equal")
  expect_equal(leaf_0$path[[1]]$col, "feature_a")
  expect_equal(leaf_0$path[[1]]$val, 10.0)
})

test_that("right child gets more operator", {
  # Simple tree where we trace from leaf_1 (right child of root)
  tree_df <- data.frame(
    tree_index = c(0L, 0L, 0L),
    split_index = c(0L, NA, NA),
    split_feature = c("feature_a", NA, NA),
    node_parent = c(NA, NA, NA),
    leaf_index = c(NA, 0L, 1L),
    leaf_parent = c(NA, 0L, 0L),
    threshold = c(10.0, NA, NA),
    decision_type = c("<=", NA, NA),
    default_left = c("TRUE", NA, NA),
    leaf_value = c(NA, 100.0, 200.0),
    stringsAsFactors = FALSE
  )

  tree_result <- tidypredict:::get_lgb_tree(tree_df)

  leaf_1 <- tree_result[[2]]
  expect_equal(leaf_1$prediction, 200.0)
  expect_length(leaf_1$path, 1)
  expect_equal(leaf_1$path[[1]]$op, "more")
  expect_equal(leaf_1$path[[1]]$col, "feature_a")
  expect_equal(leaf_1$path[[1]]$val, 10.0)
})

test_that("default_left TRUE assigns missing to left child path", {
  tree_df <- data.frame(
    tree_index = c(0L, 0L, 0L),
    split_index = c(0L, NA, NA),
    split_feature = c("x", NA, NA),
    node_parent = c(NA, NA, NA),
    leaf_index = c(NA, 0L, 1L),
    leaf_parent = c(NA, 0L, 0L),
    threshold = c(5.0, NA, NA),
    decision_type = c("<=", NA, NA),
    default_left = c("TRUE", NA, NA),
    leaf_value = c(NA, 1.0, 2.0),
    stringsAsFactors = FALSE
  )

  tree_result <- tidypredict:::get_lgb_tree(tree_df)

  # Left child (leaf_0): missing = TRUE (default_left is TRUE)
  expect_equal(tree_result[[1]]$path[[1]]$missing, TRUE)

  # Right child (leaf_1): missing = FALSE (default_left is TRUE, but we went right)
  expect_equal(tree_result[[2]]$path[[1]]$missing, FALSE)
})

test_that("default_left FALSE assigns missing to right child path", {
  tree_df <- data.frame(
    tree_index = c(0L, 0L, 0L),
    split_index = c(0L, NA, NA),
    split_feature = c("x", NA, NA),
    node_parent = c(NA, NA, NA),
    leaf_index = c(NA, 0L, 1L),
    leaf_parent = c(NA, 0L, 0L),
    threshold = c(5.0, NA, NA),
    decision_type = c("<=", NA, NA),
    default_left = c("FALSE", NA, NA),
    leaf_value = c(NA, 1.0, 2.0),
    stringsAsFactors = FALSE
  )

  tree_result <- tidypredict:::get_lgb_tree(tree_df)

  # Left child (leaf_0): missing = FALSE (default_left is FALSE)
  expect_equal(tree_result[[1]]$path[[1]]$missing, FALSE)

  # Right child (leaf_1): missing = TRUE (default_left is FALSE, we went right)
  expect_equal(tree_result[[2]]$path[[1]]$missing, TRUE)
})

test_that("deeper tree paths are traced correctly", {
  # Tree structure:
  #           split_0 (x1 <= 10)
  #          /                 \
  #      leaf_0            split_1 (x2 <= 5)
  #                       /              \
  #                   leaf_1           leaf_2
  #
  # Pre-order traversal: split_0, leaf_0, split_1, leaf_1, leaf_2
  tree_df <- data.frame(
    tree_index = c(0L, 0L, 0L, 0L, 0L),
    split_index = c(0L, NA, 1L, NA, NA),
    split_feature = c("x1", NA, "x2", NA, NA),
    node_parent = c(NA, NA, 0L, NA, NA),
    leaf_index = c(NA, 0L, NA, 1L, 2L),
    leaf_parent = c(NA, 0L, NA, 1L, 1L),
    threshold = c(10.0, NA, 5.0, NA, NA),
    decision_type = c("<=", NA, "<=", NA, NA),
    default_left = c("TRUE", NA, "FALSE", NA, NA),
    leaf_value = c(NA, 100.0, NA, 200.0, 300.0),
    stringsAsFactors = FALSE
  )

  tree_result <- tidypredict:::get_lgb_tree(tree_df)

  # leaf_0: path is just [x1 <= 10] (left of root)
  expect_equal(tree_result[[1]]$prediction, 100.0)
  expect_length(tree_result[[1]]$path, 1)
  expect_equal(tree_result[[1]]$path[[1]]$col, "x1")
  expect_equal(tree_result[[1]]$path[[1]]$op, "less-equal")
  expect_equal(tree_result[[1]]$path[[1]]$missing, TRUE) # default_left TRUE at root

  # leaf_1: path is [x1 > 10, x2 <= 5] (right of root, left of split_1)
  expect_equal(tree_result[[2]]$prediction, 200.0)
  expect_length(tree_result[[2]]$path, 2)
  # First condition: x1 > 10 (right of root)
  expect_equal(tree_result[[2]]$path[[1]]$col, "x1")
  expect_equal(tree_result[[2]]$path[[1]]$op, "more")
  expect_equal(tree_result[[2]]$path[[1]]$missing, FALSE) # went right, default_left TRUE
  # Second condition: x2 <= 5 (left of split_1)
  expect_equal(tree_result[[2]]$path[[2]]$col, "x2")
  expect_equal(tree_result[[2]]$path[[2]]$op, "less-equal")
  expect_equal(tree_result[[2]]$path[[2]]$missing, FALSE) # default_left FALSE at split_1

  # leaf_2: path is [x1 > 10, x2 > 5] (right of root, right of split_1)
  expect_equal(tree_result[[3]]$prediction, 300.0)
  expect_length(tree_result[[3]]$path, 2)
  expect_equal(tree_result[[3]]$path[[1]]$op, "more")
  expect_equal(tree_result[[3]]$path[[2]]$op, "more")
  expect_equal(tree_result[[3]]$path[[2]]$missing, TRUE) # went right, default_left FALSE
})

test_that("single leaf tree (stump) has empty path", {
  # Edge case: tree with only a root leaf (no splits)
  tree_df <- data.frame(
    tree_index = 0L,
    split_index = NA_integer_,
    split_feature = NA_character_,
    node_parent = NA_integer_,
    leaf_index = 0L,
    leaf_parent = NA_integer_,
    threshold = NA_real_,
    decision_type = NA_character_,
    default_left = NA_character_,
    leaf_value = 42.0,
    stringsAsFactors = FALSE
  )

  tree_result <- tidypredict:::get_lgb_tree(tree_df)

  expect_length(tree_result, 1)
  expect_equal(tree_result[[1]]$prediction, 42.0)
  expect_length(tree_result[[1]]$path, 0) # No conditions for root leaf
})

test_that("mixed default_left values in same tree are handled correctly", {
  # Tree where different splits have different default_left values
  #           split_0 (x1 <= 10, default_left=TRUE)
  #          /                 \
  #      split_1              leaf_2
  #  (x2 <= 5, default_left=FALSE)
  #     /        \
  #  leaf_0    leaf_1
  #
  # Pre-order: split_0, split_1, leaf_0, leaf_1, leaf_2
  tree_df <- data.frame(
    tree_index = c(0L, 0L, 0L, 0L, 0L),
    split_index = c(0L, 1L, NA, NA, NA),
    split_feature = c("x1", "x2", NA, NA, NA),
    node_parent = c(NA, 0L, NA, NA, NA),
    leaf_index = c(NA, NA, 0L, 1L, 2L),
    leaf_parent = c(NA, NA, 1L, 1L, 0L),
    threshold = c(10.0, 5.0, NA, NA, NA),
    decision_type = c("<=", "<=", NA, NA, NA),
    default_left = c("TRUE", "FALSE", NA, NA, NA),
    leaf_value = c(NA, NA, 100.0, 200.0, 300.0),
    stringsAsFactors = FALSE
  )

  tree_result <- tidypredict:::get_lgb_tree(tree_df)

  # leaf_0: path [x1 <= 10, x2 <= 5]
  # x1: left child, default_left=TRUE -> missing=TRUE
  # x2: left child, default_left=FALSE -> missing=FALSE
  expect_equal(tree_result[[1]]$path[[1]]$missing, TRUE)
  expect_equal(tree_result[[1]]$path[[2]]$missing, FALSE)

  # leaf_1: path [x1 <= 10, x2 > 5]
  # x1: left child, default_left=TRUE -> missing=TRUE
  # x2: right child, default_left=FALSE -> missing=TRUE
  expect_equal(tree_result[[2]]$path[[1]]$missing, TRUE)
  expect_equal(tree_result[[2]]$path[[2]]$missing, TRUE)

  # leaf_2: path [x1 > 10]
  # x1: right child, default_left=TRUE -> missing=FALSE
  expect_equal(tree_result[[3]]$path[[1]]$missing, FALSE)
})

test_that("model with missing values produces valid parse", {
  skip_if_not_installed("lightgbm")

  # Create data with missing values
  set.seed(456)
  X <- data.matrix(mtcars[, c("mpg", "cyl")])
  y <- mtcars$hp

  # Introduce NAs
  X_with_na <- X
  X_with_na[1:5, 1] <- NA
  X_with_na[10:15, 2] <- NA

  dtrain <- lightgbm::lgb.Dataset(
    X_with_na,
    label = y,
    colnames = c("mpg", "cyl")
  )

  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 1.0,
      objective = "regression",
      min_data_in_leaf = 1L,
      use_missing = TRUE
    ),
    data = dtrain,
    nrounds = 3L,
    verbose = -1L
  )

  pm <- parse_model(model)

  expect_s3_class(pm, "pm_lgb")
  expect_length(pm$trees, 3)

  # Verify all paths have valid missing flags (TRUE or FALSE, not NA)
  for (tree in pm$trees) {
    for (leaf in tree) {
      for (cond in leaf$path) {
        expect_type(cond$missing, "logical")
        expect_equal(is.na(cond$missing), FALSE)
      }
    }
  }
})

# Fit formula tests -----------------------------------------------------------

test_that("tidypredict_fit returns language object", {
  skip_if_not_installed("lightgbm")
  model <- make_lgb_model()

  fit_formula <- tidypredict_fit(model)

  expect_type(fit_formula, "language")
})

test_that("tidypredict_fit works on parsed model", {
  skip_if_not_installed("lightgbm")
  model <- make_lgb_model()
  pm <- parse_model(model)

  fit_formula <- tidypredict_fit(pm)

  expect_type(fit_formula, "language")
})

test_that("regression predictions match native predict", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp
  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("mpg", "cyl", "disp"))

  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.3,
      objective = "regression",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)
  tidy_preds <- dplyr::mutate(mtcars, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("binary classification predictions match native predict", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$am
  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("mpg", "cyl", "disp"))

  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.3,
      objective = "binary",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)
  tidy_preds <- dplyr::mutate(mtcars, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("poisson predictions match native predict", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "disp")])
  y <- mtcars$carb # count variable
  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("mpg", "disp"))

  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.3,
      objective = "poisson",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)
  tidy_preds <- dplyr::mutate(mtcars, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("regression_l1 predictions match native predict", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp
  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("mpg", "cyl", "disp"))

  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.3,
      objective = "regression_l1",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)
  tidy_preds <- dplyr::mutate(mtcars, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("regression_l2 predictions match native predict", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp
  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("mpg", "cyl", "disp"))

  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.3,
      objective = "regression_l2",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)
  tidy_preds <- dplyr::mutate(mtcars, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("mape predictions match native predict", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp # positive values required for mape
  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("mpg", "cyl", "disp"))

  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.3,
      objective = "mape",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)
  tidy_preds <- dplyr::mutate(mtcars, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("huber predictions match native predict", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp
  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("mpg", "cyl", "disp"))

  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.3,
      objective = "huber",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)
  tidy_preds <- dplyr::mutate(mtcars, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("fair predictions match native predict", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp
  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("mpg", "cyl", "disp"))

  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.3,
      objective = "fair",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)
  tidy_preds <- dplyr::mutate(mtcars, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("quantile predictions match native predict", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp
  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("mpg", "cyl", "disp"))

  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.3,
      objective = "quantile",
      alpha = 0.5,
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)
  tidy_preds <- dplyr::mutate(mtcars, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("gamma predictions match native predict", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp # positive values
  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("mpg", "cyl", "disp"))

  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.3,
      objective = "gamma",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)
  tidy_preds <- dplyr::mutate(mtcars, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("tweedie predictions match native predict", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp # positive values
  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("mpg", "cyl", "disp"))

  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.3,
      objective = "tweedie",
      tweedie_variance_power = 1.5,
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)
  tidy_preds <- dplyr::mutate(mtcars, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("cross_entropy predictions match native predict", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$am
  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("mpg", "cyl", "disp"))

  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.3,
      objective = "cross_entropy",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)
  tidy_preds <- dplyr::mutate(mtcars, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("predictions with missing values match", {
  skip_if_not_installed("lightgbm")

  set.seed(456)
  X <- data.matrix(mtcars[, c("mpg", "cyl")])
  y <- mtcars$hp

  # Training data with NAs
  X_train <- X
  X_train[1:3, 1] <- NA
  dtrain <- lightgbm::lgb.Dataset(
    X_train,
    label = y,
    colnames = c("mpg", "cyl")
  )

  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.5,
      objective = "regression",
      min_data_in_leaf = 1L,
      use_missing = TRUE
    ),
    data = dtrain,
    nrounds = 3L,
    verbose = -1L
  )

  # Prediction data with NAs
  X_pred <- X
  X_pred[5:7, 1] <- NA
  X_pred[10:12, 2] <- NA

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X_pred)

  pred_df <- as.data.frame(X_pred)
  tidy_preds <- dplyr::mutate(pred_df, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("unsupported objective throws error", {
  skip_if_not_installed("lightgbm")
  model <- make_lgb_model()
  pm <- parse_model(model)

  # Modify the objective to an unsupported one
  pm$general$params$objective <- "unsupported_objective"

  expect_error(
    tidypredict_fit(pm),
    "Unsupported objective"
  )
})
