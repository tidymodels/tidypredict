# Helper to create test model
make_catboost_model <- function() {
  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$hp

  pool <- catboost::catboost.load_pool(
    X,
    label = y,
    feature_names = as.list(c("mpg", "cyl", "disp"))
  )

  catboost::catboost.train(
    pool,
    params = list(
      iterations = 10L,
      depth = 3L,
      learning_rate = 0.5,
      loss_function = "RMSE",
      logging_level = "Silent",
      allow_writing_files = FALSE,
      train_dir = tempdir()
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
  expect_equal(pm$general$version, 1)

  expect_gt(length(pm$trees), 0)
})

test_that("correct number of trees extracted", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  expect_equal(length(pm$trees), 10)
})

test_that("each tree has leaves with predictions and paths", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  tree1 <- pm$trees[[1]]
  expect_gt(length(tree1), 0)

  leaf_names <- lapply(tree1, names)
  expect_true(all(vapply(leaf_names, \(x) all(c("prediction", "path") %in% x), logical(1))))

  predictions <- vapply(tree1, \(x) x$prediction, double(1))
  expect_type(predictions, "double")

  paths <- lapply(tree1, \(x) x$path)
  expect_true(all(vapply(paths, is.list, logical(1))))
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

test_that("path contains both more and less-equal operators", {
  skip_if_not_installed("catboost")
  model <- make_catboost_model()
  pm <- parse_model(model)

  # Collect all operators from first tree
  all_ops <- unlist(lapply(pm$trees[[1]], function(leaf) {
    sapply(leaf$path, function(p) p$op)
  }))

  expect_contains(all_ops, "more")
  expect_contains(all_ops, "less-equal")
})

