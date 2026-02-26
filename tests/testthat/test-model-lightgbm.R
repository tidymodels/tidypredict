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
  expect_equal(pm$general$version, 3)

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
  all_missing_flags <- unlist(lapply(pm$trees, function(tree) {
    lapply(tree, function(leaf) {
      vapply(leaf$path, function(cond) cond$missing, logical(1))
    })
  }))

  expect_type(all_missing_flags, "logical")
  expect_false(anyNA(all_missing_flags))
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

test_that("produced case_when uses .default", {
  skip_if_not_installed("lightgbm")
  model <- make_lgb_model()

  fit <- tidypredict_fit(model)
  fit_text <- rlang::expr_text(fit)

  expect_match(fit_text, "\\.default")
})

test_that("regression predictions match native predict", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp
  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )

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
  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )

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
  y <- mtcars$carb
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
  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )

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
  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )

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
  y <- mtcars$hp
  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )

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
  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )

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
  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )

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
  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )

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
  y <- mtcars$hp
  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )

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
  y <- mtcars$hp
  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )

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
  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )

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

test_that("RF boosting predictions match native predict (#185)", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp
  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )

  model <- lightgbm::lgb.train(
    params = list(
      boosting = "rf",
      num_leaves = 4L,
      objective = "regression",
      min_data_in_leaf = 1L,
      bagging_freq = 1,
      bagging_fraction = 0.8
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

  # Training data with NAs
  set.seed(456)
  X <- data.matrix(mtcars[, c("mpg", "cyl")])
  y <- mtcars$hp
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

  pm$general$params$objective <- "unsupported_objective"

  expect_snapshot(tidypredict_fit(pm), error = TRUE)
})

# SQL generation tests ------------------------------------------------------

test_that("tidypredict_sql returns SQL class", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("dbplyr")
  model <- make_lgb_model()

  sql_result <- tidypredict_sql(model, dbplyr::simulate_dbi())

  expect_s3_class(sql_result, "sql")
})

test_that("tidypredict_sql works with parsed model", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("dbplyr")
  model <- make_lgb_model()
  pm <- parse_model(model)

  sql_result <- tidypredict_sql(pm, dbplyr::simulate_dbi())

  expect_s3_class(sql_result, "sql")
})

test_that("SQL predictions match native predictions with SQLite", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")

  model <- make_lgb_model()

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  test_data <- mtcars[, c("mpg", "cyl", "disp")]
  DBI::dbWriteTable(con, "test_data", test_data)

  sql_query <- tidypredict_sql(model, con)
  db_result <- DBI::dbGetQuery(
    con,
    paste0("SELECT ", sql_query, " AS pred FROM test_data")
  )

  X <- data.matrix(test_data)
  native_preds <- predict(model, X)

  expect_equal(db_result$pred, unname(native_preds), tolerance = 1e-10)
})

test_that("SQL predictions match for binary classification with SQLite", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")

  set.seed(456)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- as.integer(mtcars$am)
  dtrain <- lightgbm::lgb.Dataset(X, label = y)
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.5,
      objective = "binary",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  test_data <- mtcars[, c("mpg", "cyl", "disp")]
  DBI::dbWriteTable(con, "test_data", test_data)

  sql_query <- tidypredict_sql(model, con)
  db_result <- DBI::dbGetQuery(
    con,
    paste0("SELECT ", sql_query, " AS pred FROM test_data")
  )

  native_preds <- predict(model, X)

  expect_equal(db_result$pred, unname(native_preds), tolerance = 1e-10)
})

# Multiclass tests ----------------------------------------------------------

test_that("parse_model extracts num_class for multiclass", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(iris[, 1:4])
  colnames(X) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  y <- as.integer(iris$Species) - 1L

  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = colnames(X))
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.5,
      objective = "multiclass",
      num_class = 3L,
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 2L,
    verbose = -1L
  )

  pm <- parse_model(model)

  expect_equal(pm$general$num_class, 3)
  expect_equal(pm$general$num_tree_per_iteration, 3)
  # 2 rounds * 3 classes = 6 trees
  expect_equal(length(pm$trees), 6)
})

test_that("tidypredict_fit returns list for multiclass", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(iris[, 1:4])
  colnames(X) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  y <- as.integer(iris$Species) - 1L

  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = colnames(X))
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.5,
      objective = "multiclass",
      num_class = 3L,
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 3L,
    verbose = -1L
  )

  fit_formulas <- tidypredict_fit(model)

  expect_type(fit_formulas, "list")
  expect_length(fit_formulas, 3)
  expect_named(fit_formulas, c("class_0", "class_1", "class_2"))

  types <- vapply(fit_formulas, typeof, character(1))
  expect_all_equal(types, "language")
})

test_that("multiclass predictions match native predictions", {
  skip_if_not_installed("lightgbm")

  set.seed(789)
  X <- data.matrix(iris[, 1:4])
  colnames(X) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  y <- as.integer(iris$Species) - 1L

  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = colnames(X))
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 8L,
      learning_rate = 0.3,
      objective = "multiclass",
      num_class = 3L,
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formulas <- tidypredict_fit(model)
  native_preds <- predict(model, X)

  test_df <- as.data.frame(X)
  tidy_preds <- dplyr::mutate(
    test_df,
    class_0 = !!fit_formulas$class_0,
    class_1 = !!fit_formulas$class_1,
    class_2 = !!fit_formulas$class_2
  )
  tidy_mat <- as.matrix(tidy_preds[, c("class_0", "class_1", "class_2")])

  expect_equal(unname(tidy_mat), unname(native_preds), tolerance = 1e-10)
})

test_that("multiclass probabilities sum to 1", {
  skip_if_not_installed("lightgbm")

  set.seed(321)
  X <- data.matrix(iris[, 1:4])
  colnames(X) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  y <- as.integer(iris$Species) - 1L

  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = colnames(X))
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.5,
      objective = "multiclass",
      num_class = 3L,
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 3L,
    verbose = -1L
  )

  fit_formulas <- tidypredict_fit(model)

  test_df <- as.data.frame(X)
  tidy_preds <- dplyr::mutate(
    test_df,
    class_0 = !!fit_formulas$class_0,
    class_1 = !!fit_formulas$class_1,
    class_2 = !!fit_formulas$class_2
  )

  row_sums <- tidy_preds$class_0 + tidy_preds$class_1 + tidy_preds$class_2
  expect_equal(row_sums, rep(1, nrow(X)), tolerance = 1e-10)
})

test_that("multiclassova predictions match native predictions", {
  skip_if_not_installed("lightgbm")

  set.seed(654)
  X <- data.matrix(iris[, 1:4])
  colnames(X) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  y <- as.integer(iris$Species) - 1L

  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = colnames(X))
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 8L,
      learning_rate = 0.3,
      objective = "multiclassova",
      num_class = 3L,
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formulas <- tidypredict_fit(model)
  native_preds <- predict(model, X)

  expect_named(fit_formulas, c("class_0", "class_1", "class_2"))

  test_df <- as.data.frame(X)
  tidy_preds <- dplyr::mutate(
    test_df,
    class_0 = !!fit_formulas$class_0,
    class_1 = !!fit_formulas$class_1,
    class_2 = !!fit_formulas$class_2
  )
  tidy_mat <- as.matrix(tidy_preds[, c("class_0", "class_1", "class_2")])

  expect_equal(unname(tidy_mat), unname(native_preds), tolerance = 1e-10)
})

test_that("multiclass SQL generation returns list of SQL", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("dbplyr")

  set.seed(123)
  X <- data.matrix(iris[, 1:4])
  colnames(X) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  y <- as.integer(iris$Species) - 1L

  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = colnames(X))
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.5,
      objective = "multiclass",
      num_class = 3L,
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 2L,
    verbose = -1L
  )

  sql_result <- tidypredict_sql(model, dbplyr::simulate_dbi())

  expect_type(sql_result, "list")
  expect_length(sql_result, 3)
  is_sql <- vapply(sql_result, \(x) inherits(x, "sql"), logical(1))
  expect_all_equal(is_sql, TRUE)
})

# Edge case tests -----------------------------------------------------------

test_that("empty trees throws error", {
  pm <- list()
  pm$general$model <- "lgb.Booster"
  pm$general$type <- "lgb"
  pm$general$version <- 3
  pm$general$params <- list(objective = "regression")
  pm$trees <- list()
  class(pm) <- c("pm_lgb", "parsed_model", "list")

  expect_snapshot(tidypredict_fit(pm), error = TRUE)
})

test_that("multiclass with num_class < 2 throws error", {
  pm <- list()
  pm$general$model <- "lgb.Booster"
  pm$general$type <- "lgb"
  pm$general$version <- 3
  pm$general$params <- list(objective = "multiclass")
  pm$general$num_class <- 1
  pm$trees <- list(list(list(prediction = 1, path = list())))
  class(pm) <- c("pm_lgb", "parsed_model", "list")

  expect_snapshot(tidypredict_fit(pm), error = TRUE)
})

test_that("multiclass with NULL num_class throws error", {
  pm <- list()
  pm$general$model <- "lgb.Booster"
  pm$general$type <- "lgb"
  pm$general$version <- 3
  pm$general$params <- list(objective = "multiclass")
  pm$general$num_class <- NULL
  pm$trees <- list(list(list(prediction = 1, path = list())))
  class(pm) <- c("pm_lgb", "parsed_model", "list")

  expect_snapshot(tidypredict_fit(pm), error = TRUE)
})

test_that("RF boosting in from_parsed averages trees", {
  pm <- list()
  pm$general$model <- "lgb.Booster"
  pm$general$type <- "lgb"
  pm$general$version <- 3
  pm$general$params <- list(objective = "regression", boosting = "rf")
  pm$trees <- list(
    list(
      list(
        prediction = 10,
        path = list(list(
          type = "conditional",
          col = "x",
          val = 5,
          op = "less-equal",
          missing = FALSE
        ))
      ),
      list(
        prediction = 20,
        path = list(list(
          type = "conditional",
          col = "x",
          val = 5,
          op = "more",
          missing = FALSE
        ))
      )
    ),
    list(
      list(
        prediction = 30,
        path = list(list(
          type = "conditional",
          col = "x",
          val = 5,
          op = "less-equal",
          missing = FALSE
        ))
      ),
      list(
        prediction = 40,
        path = list(list(
          type = "conditional",
          col = "x",
          val = 5,
          op = "more",
          missing = FALSE
        ))
      )
    )
  )
  class(pm) <- c("pm_lgb", "parsed_model", "list")

  fit_formula <- tidypredict_fit(pm)

  formula_str <- paste(deparse(fit_formula), collapse = "")
  expect_match(formula_str, "/2")
})

test_that("from_parsed handles set type with missing", {
  pm <- list()
  pm$general$model <- "lgb.Booster"
  pm$general$type <- "lgb"
  pm$general$version <- 3
  pm$general$params <- list(objective = "regression")
  pm$trees <- list(
    list(
      list(
        prediction = 10,
        path = list(list(
          type = "set",
          col = "cat_feat",
          vals = c(0L, 1L),
          op = "in",
          missing = TRUE
        ))
      ),
      list(
        prediction = 20,
        path = list(list(
          type = "set",
          col = "cat_feat",
          vals = c(0L, 1L),
          op = "not-in",
          missing = FALSE
        ))
      )
    )
  )
  class(pm) <- c("pm_lgb", "parsed_model", "list")

  fit_formula <- tidypredict_fit(pm)

  formula_str <- deparse(fit_formula)
  expect_match(formula_str, "%in%")
  expect_match(formula_str, "is.na")
})

test_that("from_parsed handles set type without missing", {
  pm <- list()
  pm$general$model <- "lgb.Booster"
  pm$general$type <- "lgb"
  pm$general$version <- 3
  pm$general$params <- list(objective = "regression")
  pm$trees <- list(
    list(
      list(
        prediction = 10,
        path = list(list(
          type = "set",
          col = "cat_feat",
          vals = c(0L, 1L),
          op = "in",
          missing = FALSE
        ))
      ),
      list(
        prediction = 20,
        path = list(list(
          type = "set",
          col = "cat_feat",
          vals = c(0L, 1L),
          op = "not-in",
          missing = FALSE
        ))
      )
    )
  )
  class(pm) <- c("pm_lgb", "parsed_model", "list")

  fit_formula <- tidypredict_fit(pm)

  formula_str <- deparse(fit_formula)
  expect_match(formula_str, "%in%")
  expect_no_match(formula_str, "is.na")
})

test_that("from_parsed handles conditional without missing", {
  pm <- list()
  pm$general$model <- "lgb.Booster"
  pm$general$type <- "lgb"
  pm$general$version <- 3
  pm$general$params <- list(objective = "regression")
  pm$trees <- list(
    list(
      list(
        prediction = 10,
        path = list(list(
          type = "conditional",
          col = "x",
          val = 5,
          op = "less-equal",
          missing = FALSE
        ))
      ),
      list(
        prediction = 20,
        path = list(list(
          type = "conditional",
          col = "x",
          val = 5,
          op = "more",
          missing = FALSE
        ))
      )
    )
  )
  class(pm) <- c("pm_lgb", "parsed_model", "list")

  fit_formula <- tidypredict_fit(pm)

  formula_str <- deparse(fit_formula)
  expect_match(formula_str, "<=")
  expect_no_match(formula_str, "is.na")
})

test_that("build_lgb_nested_condition errors on unknown type", {
  condition <- list(
    type = "unknown_type",
    col = "x",
    val = 1,
    op = "less-equal",
    missing = FALSE
  )

  expect_snapshot(
    tidypredict:::build_lgb_nested_condition(condition),
    error = TRUE
  )
})

# Categorical feature tests -------------------------------------------------

test_that("parse_model handles categorical splits", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  n <- 200
  cat_int <- sample(0:3, n, replace = TRUE)
  y <- ifelse(cat_int %in% c(0, 1), 10, -10) + rnorm(n, sd = 0.3)

  X <- matrix(cat_int, ncol = 1)
  colnames(X) <- "cat_feat"

  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    categorical_feature = "cat_feat"
  )
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 1.0,
      objective = "regression",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 1L,
    verbose = -1L
  )

  pm <- parse_model(model)

  expect_s3_class(pm, "parsed_model")
  expect_gt(length(pm$trees), 0)

  first_leaf <- pm$trees[[1]][[1]]
  expect_gt(length(first_leaf$path), 0)
  expect_equal(first_leaf$path[[1]]$type, "set")
  expect_equal(first_leaf$path[[1]]$op, "in")
  expect_type(first_leaf$path[[1]]$vals, "integer")
})

test_that("categorical predictions match native predictions", {
  skip_if_not_installed("lightgbm")

  set.seed(457)
  n <- 200
  cat_int <- sample(0:3, n, replace = TRUE)
  y <- ifelse(cat_int %in% c(0, 1), 10, -10) + rnorm(n, sd = 2)

  X <- matrix(cat_int, ncol = 1)
  colnames(X) <- "cat_feat"

  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    categorical_feature = "cat_feat"
  )
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 1.0,
      objective = "regression",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 2L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)

  test_df <- data.frame(cat_feat = cat_int)
  tidy_preds <- dplyr::mutate(test_df, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("mixed numerical + categorical predictions match", {
  skip_if_not_installed("lightgbm")

  set.seed(789)
  n <- 300
  cat_int <- sample(0:3, n, replace = TRUE)
  num_feat <- rnorm(n)
  y <- ifelse(cat_int %in% c(0, 1), 5, -5) + num_feat * 2 + rnorm(n, sd = 0.3)

  X <- cbind(num_feat = num_feat, cat_feat = cat_int)

  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    categorical_feature = "cat_feat"
  )
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 8L,
      learning_rate = 0.5,
      objective = "regression",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 3L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)

  test_df <- as.data.frame(X)
  tidy_preds <- dplyr::mutate(test_df, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("categorical with missing values predictions match", {
  skip_if_not_installed("lightgbm")

  set.seed(321)
  n <- 300
  cat_int <- sample(0:3, n, replace = TRUE)
  y <- ifelse(cat_int %in% c(0, 1), 10, -10) + rnorm(n, sd = 0.3)

  # Add NAs
  na_idx <- sample(n, 30)
  cat_int[na_idx] <- NA
  y[na_idx] <- 10 + rnorm(30, sd = 0.3)

  X <- matrix(as.numeric(cat_int), ncol = 1)
  colnames(X) <- "cat_feat"

  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    categorical_feature = "cat_feat"
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
    nrounds = 1L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)

  test_X <- matrix(c(0, 1, 2, 3, NA), ncol = 1)
  colnames(test_X) <- "cat_feat"
  test_df <- data.frame(cat_feat = c(0, 1, 2, 3, NA))

  native_preds <- predict(model, test_X)
  tidy_preds <- dplyr::mutate(test_df, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("categorical SQL generation works", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  set.seed(123)
  n <- 200
  cat_int <- sample(0:3, n, replace = TRUE)
  y <- ifelse(cat_int %in% c(0, 1), 10, -10) + rnorm(n, sd = 2)

  X <- matrix(cat_int, ncol = 1)
  colnames(X) <- "cat_feat"

  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    categorical_feature = "cat_feat"
  )
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 1.0,
      objective = "regression",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 1L,
    verbose = -1L
  )

  sql_result <- tidypredict_sql(model, dbplyr::simulate_dbi())
  expect_s3_class(sql_result, "sql")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  test_data <- data.frame(cat_feat = 0:3)
  DBI::dbWriteTable(con, "test_data", test_data)

  sql_query <- tidypredict_sql(model, con)
  db_result <- DBI::dbGetQuery(
    con,
    paste0("SELECT ", sql_query, " AS pred FROM test_data")
  )

  test_X <- matrix(0:3, ncol = 1)
  colnames(test_X) <- "cat_feat"
  native_preds <- predict(model, test_X)

  expect_equal(db_result$pred, unname(native_preds), tolerance = 1e-10)
})

test_that("parse_lgb_categorical_threshold handles various formats", {
  expect_equal(
    parse_lgb_categorical_threshold("0||1"),
    c(0L, 1L)
  )

  expect_equal(
    parse_lgb_categorical_threshold("0||1||3"),
    c(0L, 1L, 3L)
  )

  expect_equal(
    parse_lgb_categorical_threshold("2"),
    2L
  )
})

test_that("categorical path contains both in and not-in operators", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  n <- 200
  cat_int <- sample(0:3, n, replace = TRUE)
  y <- ifelse(cat_int %in% c(0, 1), 10, -10) + rnorm(n, sd = 0.3)

  X <- matrix(cat_int, ncol = 1)
  colnames(X) <- "cat_feat"

  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    categorical_feature = "cat_feat"
  )
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 1.0,
      objective = "regression",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 1L,
    verbose = -1L
  )

  pm <- parse_model(model)

  # Collect all operators from paths
  all_ops <- unlist(lapply(pm$trees[[1]], function(leaf) {
    sapply(leaf$path, function(p) p$op)
  }))

  expect_contains(all_ops, "in")
  expect_contains(all_ops, "not-in")
})

test_that("categorical with many categories works", {
  skip_if_not_installed("lightgbm")

  set.seed(555)
  n <- 400
  cat_int <- sample(0:7, n, replace = TRUE)
  y <- ifelse(cat_int < 4, 10, -10) + rnorm(n, sd = 2)

  X <- matrix(cat_int, ncol = 1)
  colnames(X) <- "cat_feat"

  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    categorical_feature = "cat_feat"
  )
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 1.0,
      objective = "regression",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 2L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)

  test_df <- data.frame(cat_feat = cat_int)
  tidy_preds <- dplyr::mutate(test_df, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("parsed model can be saved and loaded via YAML", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("yaml")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.5,
      objective = "regression",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  pm <- parse_model(model)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  l <- yaml::read_yaml(mp)
  pm_loaded <- as_parsed_model(l)

  fit_original <- tidypredict_fit(pm)
  fit_loaded <- tidypredict_fit(pm_loaded)

  test_df <- as.data.frame(X)
  preds_original <- dplyr::mutate(test_df, pred = !!fit_original)$pred
  preds_loaded <- dplyr::mutate(test_df, pred = !!fit_loaded)$pred

  expect_equal(preds_original, preds_loaded, tolerance = 1e-6)
})

test_that("parsed multiclass model can be saved and loaded via YAML", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("yaml")

  set.seed(123)
  X <- data.matrix(iris[, 1:4])
  colnames(X) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  y <- as.integer(iris$Species) - 1L

  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = colnames(X))
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.5,
      objective = "multiclass",
      num_class = 3L,
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 3L,
    verbose = -1L
  )

  pm <- parse_model(model)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  l <- yaml::read_yaml(mp)
  pm_loaded <- as_parsed_model(l)

  fit_original <- tidypredict_fit(pm)
  fit_loaded <- tidypredict_fit(pm_loaded)

  test_df <- as.data.frame(X)
  preds_original <- lapply(fit_original, \(f) {
    dplyr::mutate(test_df, pred = !!f)$pred
  })
  preds_loaded <- lapply(fit_loaded, \(f) {
    dplyr::mutate(test_df, pred = !!f)$pred
  })

  expect_equal(preds_original, preds_loaded, tolerance = 1e-6)
})

test_that("tidypredict_test works for regression model", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.5,
      objective = "regression",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  result <- tidypredict_test(model, xg_df = X)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
  expect_equal(max(result$raw_results$fit_diff), 0, tolerance = 1e-10)
})

test_that("tidypredict_test works for binary classification model", {
  skip_if_not_installed("lightgbm")

  set.seed(456)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- as.integer(mtcars$am)

  dtrain <- lightgbm::lgb.Dataset(X, label = y)
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.5,
      objective = "binary",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  result <- tidypredict_test(model, xg_df = X)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("tidypredict_test errors for multiclass model", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(iris[, 1:4])
  colnames(X) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  y <- as.integer(iris$Species) - 1L

  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = colnames(X))
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.5,
      objective = "multiclass",
      num_class = 3L,
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 3L,
    verbose = -1L
  )

  expect_snapshot(tidypredict_test(model, xg_df = X), error = TRUE)
})

test_that("tidypredict_test errors when matrix not provided", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.5,
      objective = "regression",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  expect_snapshot(tidypredict_test(model), error = TRUE)
})

test_that("tidypredict_test respects max_rows parameter", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.5,
      objective = "regression",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  result <- tidypredict_test(model, xg_df = X, max_rows = 10)

  expect_equal(nrow(result$raw_results), 10)
})

test_that(".extract_lgb_trees returns list of tree expressions", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  dtrain <- lightgbm::lgb.Dataset(
    X,
    label = y,
    colnames = c("mpg", "cyl", "disp")
  )
  model <- lightgbm::lgb.train(
    params = list(
      num_leaves = 4L,
      learning_rate = 0.5,
      objective = "regression",
      min_data_in_leaf = 1L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  trees <- .extract_lgb_trees(model)

  expect_type(trees, "list")
  expect_length(trees, 5)

  types <- vapply(trees, typeof, character(1))
  expect_all_equal(types, "language")
})

test_that(".extract_lgb_trees combined results match tidypredict_fit", {
  skip_if_not_installed("lightgbm")
  model <- make_lgb_model()
  test_data <- mtcars[, c("mpg", "cyl", "disp")]

  trees <- .extract_lgb_trees(model)
  eval_env <- rlang::new_environment(
    data = as.list(test_data),
    parent = asNamespace("dplyr")
  )
  tree_preds <- lapply(trees, rlang::eval_tidy, env = eval_env)
  combined <- Reduce(`+`, tree_preds)

  fit_result <- rlang::eval_tidy(tidypredict_fit(model), test_data)

  expect_equal(combined, fit_result)
})

test_that(".extract_lgb_trees errors on non-lgb.Booster", {
  expect_snapshot(.extract_lgb_trees(list()), error = TRUE)
})

test_that("tidypredict works with parsnip/bonsai lightgbm model", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("bonsai")

  set.seed(123)
  train_data <- mtcars[, c("hp", "mpg", "cyl", "disp")]

  model_spec <- parsnip::boost_tree(
    trees = 5,
    tree_depth = 3,
    min_n = 1
  ) |>
    parsnip::set_engine("lightgbm") |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    hp ~ mpg + cyl + disp,
    data = train_data
  )

  lgb_model <- model_fit$fit

  expect_s3_class(lgb_model, "lgb.Booster")

  pm <- parse_model(lgb_model)
  expect_s3_class(pm, "parsed_model")
  expect_s3_class(pm, "pm_lgb")
  expect_gt(length(pm$trees), 0)

  fit_formula <- tidypredict_fit(lgb_model)
  expect_type(fit_formula, "language")

  X <- data.matrix(train_data[, c("mpg", "cyl", "disp")])
  native_preds <- predict(lgb_model, X)
  tidy_preds <- dplyr::mutate(train_data, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("tidypredict works with parsnip/bonsai binary classification", {
  skip_if_not_installed("lightgbm")
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
    parsnip::set_engine("lightgbm") |>
    parsnip::set_mode("classification")

  model_fit <- parsnip::fit(
    model_spec,
    am ~ mpg + cyl + disp,
    data = train_data
  )

  lgb_model <- model_fit$fit

  expect_s3_class(lgb_model, "lgb.Booster")

  fit_formula <- tidypredict_fit(lgb_model)
  expect_type(fit_formula, "language")

  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  native_preds <- predict(lgb_model, X)
  tidy_preds <- dplyr::mutate(mtcars, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("tidypredict_sql works with parsnip/bonsai model", {
  skip_if_not_installed("lightgbm")
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
    parsnip::set_engine("lightgbm") |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    hp ~ mpg + cyl + disp,
    data = train_data
  )
  lgb_model <- model_fit$fit

  sql_result <- tidypredict_sql(lgb_model, dbplyr::simulate_dbi())

  expect_s3_class(sql_result, "sql")
})

test_that("tidypredict_test works with parsnip/bonsai model", {
  skip_if_not_installed("lightgbm")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("bonsai")

  set.seed(123)
  train_data <- mtcars[, c("hp", "mpg", "cyl", "disp")]

  model_spec <- parsnip::boost_tree(
    trees = 5,
    tree_depth = 3,
    min_n = 1
  ) |>
    parsnip::set_engine("lightgbm") |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    hp ~ mpg + cyl + disp,
    data = train_data
  )
  lgb_model <- model_fit$fit

  X <- data.matrix(train_data[, c("mpg", "cyl", "disp")])
  result <- tidypredict_test(lgb_model, xg_df = X)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

# Linear tree tests ----------------------------------------------------------

test_that("linear tree regression predictions match native predict (#186)", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  n <- 100
  X <- cbind(x1 = rnorm(n), x2 = rnorm(n))
  y <- 2 * X[, 1] + 3 * X[, 2] + rnorm(n, sd = 0.1)

  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("x1", "x2"))
  model <- lightgbm::lgb.train(
    params = list(
      objective = "regression",
      linear_tree = TRUE,
      num_leaves = 4L,
      min_data_in_leaf = 10L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)

  test_df <- as.data.frame(X)
  tidy_preds <- dplyr::mutate(test_df, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("linear tree binary classification predictions match (#186)", {
  skip_if_not_installed("lightgbm")

  set.seed(456)
  n <- 200
  X <- cbind(x1 = rnorm(n), x2 = rnorm(n))
  y <- as.numeric((2 * X[, 1] + 3 * X[, 2] + rnorm(n)) > 0)

  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("x1", "x2"))
  model <- lightgbm::lgb.train(
    params = list(
      objective = "binary",
      linear_tree = TRUE,
      num_leaves = 4L,
      min_data_in_leaf = 20L
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)

  test_df <- as.data.frame(X)
  tidy_preds <- dplyr::mutate(test_df, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("linear tree multiclass predictions match (#186)", {
  skip_if_not_installed("lightgbm")

  set.seed(789)
  n <- 300
  X <- cbind(x1 = rnorm(n), x2 = rnorm(n))
  y <- as.integer(cut(X[, 1] + X[, 2] + rnorm(n, sd = 0.5), breaks = 3)) - 1L

  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("x1", "x2"))
  model <- lightgbm::lgb.train(
    params = list(
      objective = "multiclass",
      num_class = 3L,
      linear_tree = TRUE,
      num_leaves = 4L,
      min_data_in_leaf = 20L
    ),
    data = dtrain,
    nrounds = 3L,
    verbose = -1L
  )

  fit_formulas <- tidypredict_fit(model)
  native_preds <- predict(model, X)
  native_mat <- matrix(native_preds, ncol = 3, byrow = FALSE)

  test_df <- as.data.frame(X)
  tidy_preds <- dplyr::mutate(
    test_df,
    class_0 = !!fit_formulas$class_0,
    class_1 = !!fit_formulas$class_1,
    class_2 = !!fit_formulas$class_2
  )
  tidy_mat <- as.matrix(tidy_preds[, c("class_0", "class_1", "class_2")])

  expect_equal(unname(tidy_mat), unname(native_mat), tolerance = 1e-10)
})

test_that("linear tree with RF boosting predictions match (#186)", {
  skip_if_not_installed("lightgbm")

  set.seed(321)
  n <- 100
  X <- cbind(x1 = rnorm(n), x2 = rnorm(n))
  y <- 2 * X[, 1] + 3 * X[, 2] + rnorm(n, sd = 0.1)

  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("x1", "x2"))
  model <- lightgbm::lgb.train(
    params = list(
      boosting = "rf",
      objective = "regression",
      linear_tree = TRUE,
      num_leaves = 4L,
      min_data_in_leaf = 10L,
      bagging_freq = 1,
      bagging_fraction = 0.8
    ),
    data = dtrain,
    nrounds = 5L,
    verbose = -1L
  )

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, X)

  test_df <- as.data.frame(X)
  tidy_preds <- dplyr::mutate(test_df, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})

test_that("linear tree parsed model has correct structure", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  n <- 100
  X <- cbind(x1 = rnorm(n), x2 = rnorm(n))
  y <- 2 * X[, 1] + 3 * X[, 2] + rnorm(n, sd = 0.1)

  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("x1", "x2"))
  model <- lightgbm::lgb.train(
    params = list(
      objective = "regression",
      linear_tree = TRUE,
      num_leaves = 4L,
      min_data_in_leaf = 10L
    ),
    data = dtrain,
    nrounds = 3L,
    verbose = -1L
  )

  pm <- parse_model(model)

  expect_s3_class(pm, "parsed_model")
  expect_length(pm$trees, 3)

  # Check that leaves have linear info instead of constant predictions
  first_tree <- pm$trees[[1]]
  first_leaf <- first_tree[[1]]

  expect_contains(names(first_leaf), c("prediction", "linear", "path"))

  # Either prediction is NULL (linear) or linear is NULL (constant)
  has_linear <- !is.null(first_leaf$linear)
  if (has_linear) {
    expect_null(first_leaf$prediction)
    expect_contains(
      names(first_leaf$linear),
      c("intercept", "feature_names", "coefficients")
    )
    expect_type(first_leaf$linear$intercept, "double")
    expect_type(first_leaf$linear$feature_names, "character")
    expect_type(first_leaf$linear$coefficients, "double")
  }
})

test_that("linear tree handles NA values correctly when trained with NAs (#186)", {
  skip_if_not_installed("lightgbm")

  set.seed(123)
  n <- 100
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- 2 + 0.5 * x1 + 0.3 * x2 + rnorm(n, sd = 0.1)

  # Add NAs to training data so model learns NA handling

  x1[c(5, 15, 25)] <- NA
  x2[c(10, 20, 30)] <- NA

  X <- cbind(x1 = x1, x2 = x2)
  dtrain <- lightgbm::lgb.Dataset(X, label = y, colnames = c("x1", "x2"))
  model <- lightgbm::lgb.train(
    params = list(
      objective = "regression",
      linear_tree = TRUE,
      num_leaves = 4L,
      min_data_in_leaf = 10L
    ),
    data = dtrain,
    nrounds = 3L,
    verbose = -1L
  )

  # Test data with various NA patterns
  test_X <- rbind(
    c(0.5, 0.5), # No NAs
    c(NA, 0.5), # NA in x1
    c(0.5, NA), # NA in x2
    c(NA, NA) # Both NA
  )
  colnames(test_X) <- c("x1", "x2")

  fit_formula <- tidypredict_fit(model)
  native_preds <- predict(model, test_X)

  test_df <- as.data.frame(test_X)
  tidy_preds <- dplyr::mutate(test_df, pred = !!fit_formula)$pred

  expect_equal(unname(tidy_preds), unname(native_preds), tolerance = 1e-10)
})
