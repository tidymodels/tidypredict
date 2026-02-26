test_that("returns the right output", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    num.trees = 3,
    max.depth = 2,
    seed = 100,
    num.threads = 2
  )

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(length(pm$tree_info_list), 3)
  expect_equal(pm$general$model, "ranger")
  expect_equal(pm$general$version, 3)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("tidypredict_fit produces correct predictions", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    num.trees = 3,
    max.depth = 2,
    seed = 100,
    num.threads = 2
  )

  fit_expr <- tidypredict_fit(model)
  fit_pred <- dplyr::mutate(mtcars, pred = !!fit_expr)$pred
  original_pred <- predict(model, mtcars)$predictions

  expect_equal(fit_pred, original_pred)
})

test_that("formulas produces correct predictions", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  # regression
  expect_snapshot(
    tidypredict_test(
      ranger::ranger(
        mpg ~ .,
        data = mtcars,
        num.trees = 3,
        max.depth = 2,
        seed = 100,
        num.threads = 2
      ),
      mtcars
    )
  )
})

test_that("split operator uses <= for left child (#189)", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    num.trees = 2,
    max.depth = 3,
    seed = 123,
    num.threads = 2
  )

  native <- predict(model, mtcars)$predictions
  fit <- tidypredict_fit(model)
  tidy <- rlang::eval_tidy(fit, mtcars)

  expect_equal(native, tidy)
})

test_that("predictions are averaged not summed (#190)", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    num.trees = 5,
    max.depth = 3,
    seed = 123,
    num.threads = 2
  )

  native <- predict(model, mtcars)$predictions
  fit <- tidypredict_fit(model)
  tidy <- rlang::eval_tidy(fit, mtcars)

  expect_equal(native, tidy)
})

test_that("produced case_when uses .default", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    num.trees = 3,
    max.depth = 2,
    seed = 100,
    num.threads = 2
  )

  fit <- tidypredict_fit(model)
  fit_text <- rlang::expr_text(fit)

  expect_match(fit_text, "\\.default")
})

test_that("classification models error with clear message (#191)", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    num.trees = 3,
    max.depth = 2,
    seed = 123,
    num.threads = 2
  )

  expect_snapshot(tidypredict_fit(model), error = TRUE)
})

# Tests for .extract_ranger_classprob()

test_that(".extract_ranger_classprob returns correct structure", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    num.trees = 3,
    max.depth = 2,
    seed = 123,
    num.threads = 2,
    probability = TRUE
  )

  result <- .extract_ranger_classprob(model)

  expect_type(result, "list")
  expect_length(result, 3)
  expect_named(result, levels(iris$Species))
  # Each class should have num.trees expressions
  expect_length(result[[1]], 3)
})

test_that(".extract_ranger_classprob errors on non-ranger model", {
  model <- lm(mpg ~ ., data = mtcars)

  expect_snapshot(error = TRUE, .extract_ranger_classprob(model))
})

test_that(".extract_ranger_classprob errors without probability = TRUE", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    num.trees = 3,
    max.depth = 2,
    seed = 123,
    num.threads = 2,
    probability = FALSE
  )

  expect_snapshot(error = TRUE, .extract_ranger_classprob(model))
})

test_that(".extract_ranger_classprob works with binary classification", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  mtcars$vs <- factor(mtcars$vs)
  model <- ranger::ranger(
    vs ~ disp + hp,
    data = mtcars,
    num.trees = 3,
    max.depth = 2,
    seed = 123,
    num.threads = 2,
    probability = TRUE
  )

  result <- .extract_ranger_classprob(model)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("0", "1"))
})

test_that(".extract_ranger_classprob produces correct probabilities", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    Species ~ .,
    data = iris,
    num.trees = 5,
    max.depth = 3,
    seed = 123,
    num.threads = 2,
    probability = TRUE
  )

  class_trees <- .extract_ranger_classprob(model)
  n_trees <- model$num.trees

  # Sum probabilities for each class
  prob_sums <- sapply(names(class_trees), function(cls) {
    trees <- class_trees[[cls]]
    tree_vals <- sapply(trees, function(e) {
      rlang::eval_tidy(e, iris)
    })
    if (is.matrix(tree_vals)) rowSums(tree_vals) else tree_vals
  })

  # Calculate averaged probabilities
  probs <- prob_sums / n_trees

  # Compare to native predictions
  native <- predict(model, iris)$predictions

  expect_equal(unname(probs), unname(native), tolerance = 1e-10)
})

test_that(".extract_ranger_classprob works with single tree", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    Species ~ .,
    data = iris,
    num.trees = 1,
    max.depth = 3,
    seed = 123,
    num.threads = 2,
    probability = TRUE
  )

  result <- .extract_ranger_classprob(model)

  expect_type(result, "list")
  expect_length(result, 3)
  # Each class should have 1 expression
  expect_length(result[[1]], 1)
})

# Tests for .extract_ranger_trees() (regression)

test_that(".extract_ranger_trees returns correct structure", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    num.trees = 5,
    max.depth = 2,
    seed = 100,
    num.threads = 2
  )

  result <- .extract_ranger_trees(model)

  expect_type(result, "list")
  expect_length(result, 5)
  expect_all_true(vapply(result, is.language, logical(1)))
})

test_that(".extract_ranger_trees errors on non-ranger model", {
  model <- lm(mpg ~ ., data = mtcars)

  expect_snapshot(error = TRUE, .extract_ranger_trees(model))
})

test_that(".extract_ranger_trees errors on classification model", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    num.trees = 3,
    max.depth = 2,
    seed = 123,
    num.threads = 2
  )

  expect_snapshot(error = TRUE, .extract_ranger_trees(model))
})

test_that(".extract_ranger_trees produces correct predictions when averaged", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    num.trees = 5,
    max.depth = 3,
    seed = 123,
    num.threads = 2
  )

  trees <- .extract_ranger_trees(model)
  n_trees <- length(trees)

  tree_preds <- sapply(trees, function(e) rlang::eval_tidy(e, mtcars))
  avg_pred <- rowMeans(tree_preds)

  native <- predict(model, mtcars)$predictions

  expect_equal(avg_pred, native)
})

# Backwards compatibility tests for v2 parsed models

test_that("v2 parsed ranger model can be loaded and used", {
  pm <- readRDS(test_path("backwards-compat", "ranger-v2-parsed.rds"))

  expect_equal(pm$general$version, 2)
  expect_equal(pm$general$model, "ranger")
  expect_true(!is.null(pm$trees))

  fit <- tidypredict_fit(pm)
  expect_type(fit, "language")

  preds <- rlang::eval_tidy(fit, mtcars)
  expect_length(preds, nrow(mtcars))
})

test_that("v2 parsed ranger model produces expected predictions", {
  pm <- readRDS(test_path("backwards-compat", "ranger-v2-parsed.rds"))

  fit <- tidypredict_fit(pm)
  preds <- rlang::eval_tidy(fit, mtcars)

  # Expected values from the v2 model (2 trees, mpg ~ cyl + disp)
  # mtcars[1,] has disp = 160, which falls in middle branch of both trees
  # Tree 1: 20.7125, Tree 2: 20.2471 -> avg = 20.47978
  expect_equal(preds[1], (20.7125 + 20.2470588235294) / 2, tolerance = 0.001)
})

test_that("v2 parsed classification model errors", {
  pm <- readRDS(test_path("backwards-compat", "ranger-v2-classification.rds"))

  expect_equal(pm$general$version, 2)
  # First prediction is a string (classification)
  expect_type(pm$trees[[1]][[1]]$prediction, "character")

  expect_snapshot(tidypredict_fit(pm), error = TRUE)
})

test_that("legacy get_ra_trees extracts correct structure", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    mpg ~ cyl + disp,
    data = mtcars,
    num.trees = 2,
    max.depth = 2,
    seed = 100,
    num.threads = 1
  )

  trees <- tidypredict:::get_ra_trees(model)

  expect_type(trees, "list")
  expect_length(trees, 2)
  # Each tree should have leaf nodes with prediction and path

  expect_true(all(vapply(
    trees[[1]],
    function(x) "prediction" %in% names(x),
    logical(1)
  )))
  expect_true(all(vapply(
    trees[[1]],
    function(x) "path" %in% names(x),
    logical(1)
  )))
})

test_that("legacy get_ra_tree extracts single tree", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    mpg ~ cyl + disp,
    data = mtcars,
    num.trees = 2,
    max.depth = 2,
    seed = 100,
    num.threads = 1
  )

  tree <- tidypredict:::get_ra_tree(1, model)

  expect_type(tree, "list")
  expect_true(length(tree) > 0)
  expect_true("prediction" %in% names(tree[[1]]))
  expect_true("path" %in% names(tree[[1]]))
})

test_that("legacy get_child_info builds parent map", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    mpg ~ cyl + disp,
    data = mtcars,
    num.trees = 1,
    max.depth = 2,
    seed = 100,
    num.threads = 1
  )

  tree <- ranger::treeInfo(model, 1)
  child_info <- tidypredict:::get_child_info(tree)

  expect_type(child_info, "double")
  # child_info maps each node to its parent
  expect_true(length(child_info) > 0)
})

test_that("legacy get_ra_path handles stump trees", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  # Create a stump tree (single node, no splits)
  model <- ranger::ranger(
    mpg ~ cyl,
    data = mtcars,
    num.trees = 1,
    max.depth = 0,
    seed = 100,
    num.threads = 1
  )

  tree <- ranger::treeInfo(model, 1)
  child_info <- tidypredict:::get_child_info(tree)

  # Stump has no children, so path should be empty

  path <- tidypredict:::get_ra_path(0, tree, child_info, FALSE)
  expect_equal(path, list())
})

test_that("legacy get_ra_path with default_op = TRUE", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    mpg ~ cyl + disp,
    data = mtcars,
    num.trees = 1,
    max.depth = 2,
    seed = 100,
    num.threads = 1
  )

  tree <- ranger::treeInfo(model, 1)
  child_info <- tidypredict:::get_child_info(tree)
  terminal_nodes <- tree$nodeID[tree$terminal]

  # Test with default_op = TRUE (uses "less" and "more-equal")
  # Test all terminal nodes to exercise both left and right child paths
  all_ops <- character(0)
  for (node in terminal_nodes) {
    path <- tidypredict:::get_ra_path(node, tree, child_info, TRUE)
    expect_type(path, "list")
    if (length(path) > 0) {
      ops <- vapply(path, function(x) x$op, character(1))
      expect_true(all(ops %in% c("less", "more-equal")))
      all_ops <- c(all_ops, ops)
    }
  }
  # Ensure both operators are used across all paths
  expect_true("less" %in% all_ops)
  expect_true("more-equal" %in% all_ops)
})

test_that("parse_model.ranger errors on classification", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    num.trees = 3,
    max.depth = 2,
    seed = 123,
    num.threads = 2
  )

  expect_snapshot(parse_model(model), error = TRUE)
})

test_that("legacy get_ra_tree converts factor predictions to character", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  # Non-probability classification model returns factor predictions
  model <- ranger::ranger(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    num.trees = 1,
    max.depth = 2,
    seed = 123,
    num.threads = 1,
    probability = FALSE
  )

  tree <- tidypredict:::get_ra_tree(1, model)

  expect_type(tree, "list")
  # Predictions should be converted to character
  expect_type(tree[[1]]$prediction, "character")
})

test_that("legacy get_ra_tree handles probability predictions", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    num.trees = 1,
    max.depth = 2,
    seed = 123,
    num.threads = 2,
    probability = TRUE
  )

  tree <- tidypredict:::get_ra_tree(1, model)

  expect_type(tree, "list")
  expect_true(length(tree) > 0)

  # With probability = TRUE, nodes should have probs field
  expect_true("probs" %in% names(tree[[1]]))
  expect_true("prob" %in% names(tree[[1]]))
})
