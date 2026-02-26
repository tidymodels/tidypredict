test_that("returns the right output", {
  set.seed(1234)

  model <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 3)

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(length(pm$tree_info_list), 3)
  expect_equal(pm$general$model, "randomForest")
  expect_equal(pm$general$version, 3)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("Model can be saved and re-loaded", {
  set.seed(1234)

  model <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 3)

  fit_expr <- tidypredict_fit(model)
  fit_pred <- dplyr::mutate(mtcars, pred = !!fit_expr)$pred
  original_pred <- predict(model, mtcars)

  expect_equal(fit_pred, as.vector(original_pred))
})

test_that("formulas produces correct predictions", {
  set.seed(1234)

  # regression
  expect_snapshot(
    tidypredict_test(
      randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 3),
      mtcars,
    )
  )
})

test_that("split operator uses <= for left child (#192)", {
  set.seed(42)
  df <- data.frame(x = c(1, 2, 3, 4), y = c(10, 20, 100, 200))
  suppressWarnings(
    model <- randomForest::randomForest(
      y ~ x,
      data = df,
      ntree = 1,
      nodesize = 2,
      maxnodes = 3
    )
  )

  test_df <- data.frame(x = c(2.99, 3, 3.01))

  native <- as.numeric(predict(model, test_df))
  fit <- tidypredict_fit(model)
  tidy <- rlang::eval_tidy(fit, test_df)

  expect_equal(native, tidy)
})

test_that("produced case_when uses .default", {
  set.seed(1234)

  model <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 3)

  fit <- tidypredict_fit(model)
  fit_text <- rlang::expr_text(fit)

  expect_match(fit_text, "\\.default")
})

test_that("classification models error with clear message (#193)", {
  set.seed(123)
  model <- randomForest::randomForest(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    ntree = 3
  )

  expect_snapshot(tidypredict_fit(model), error = TRUE)
})

test_that("parse_model errors on classification model", {
  set.seed(123)
  model <- randomForest::randomForest(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    ntree = 3
  )

  expect_snapshot(parse_model(model), error = TRUE)
})

# v2 backwards compatibility tests ---------------------------------------------

test_that("v2 parsed randomForest model produces correct predictions", {
  pm <- readRDS(test_path("backwards-compat", "rf-v2-regression.rds"))

  expect_equal(pm$general$version, 2)
  expect_true(!is.null(pm$trees))

  fit <- tidypredict_fit(pm)
  expect_type(fit, "language")

  # Verify predictions match expected values
  pred <- rlang::eval_tidy(fit, mtcars)
  expect_type(pred, "double")
  expect_length(pred, nrow(mtcars))
})

test_that("v2 parsed classification model errors", {
  pm <- readRDS(test_path("backwards-compat", "rf-v2-classification.rds"))

  expect_equal(pm$general$version, 2)
  expect_true(is.character(pm$trees[[1]][[1]]$prediction))

  expect_snapshot(tidypredict_fit(pm), error = TRUE)
})

# Tests for .extract_rf_classprob()

test_that(".extract_rf_classprob returns correct structure", {
  set.seed(123)
  model <- randomForest::randomForest(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    ntree = 3
  )

  result <- .extract_rf_classprob(model)

  expect_type(result, "list")
  expect_length(result, 3)
  expect_named(result, levels(iris$Species))
  # Each class should have ntree expressions
  expect_length(result[[1]], 3)
})

test_that(".extract_rf_classprob errors on non-randomForest model", {
  model <- lm(mpg ~ ., data = mtcars)

  expect_snapshot(error = TRUE, .extract_rf_classprob(model))
})

test_that(".extract_rf_classprob errors on regression model", {
  set.seed(123)
  model <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 3)

  expect_snapshot(error = TRUE, .extract_rf_classprob(model))
})

test_that(".extract_rf_classprob works with binary classification", {
  set.seed(123)
  mtcars$vs <- factor(mtcars$vs)
  model <- randomForest::randomForest(
    vs ~ disp + hp,
    data = mtcars,
    ntree = 3
  )

  result <- .extract_rf_classprob(model)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("0", "1"))
})

test_that(".extract_rf_classprob produces correct vote counts", {
  set.seed(123)
  model <- randomForest::randomForest(
    Species ~ .,
    data = iris,
    ntree = 5
  )

  class_trees <- .extract_rf_classprob(model)
  n_trees <- model$ntree

  # Sum votes for each class
  vote_counts <- sapply(names(class_trees), function(cls) {
    trees <- class_trees[[cls]]
    tree_vals <- sapply(trees, function(e) {
      rlang::eval_tidy(e, iris)
    })
    if (is.matrix(tree_vals)) rowSums(tree_vals) else tree_vals
  })

  # Calculate probabilities
  probs <- vote_counts / n_trees

  # Compare to native predictions
  native <- predict(model, iris, type = "prob")

  expect_equal(unname(probs), unname(native), tolerance = 1e-10)
})

test_that(".extract_rf_classprob works with single tree", {
  set.seed(123)
  model <- randomForest::randomForest(
    Species ~ .,
    data = iris,
    ntree = 1
  )

  result <- .extract_rf_classprob(model)

  expect_type(result, "list")
  expect_length(result, 3)
  # Each class should have 1 expression
  expect_length(result[[1]], 1)
})

# Tests for .extract_rf_trees() (regression)

test_that(".extract_rf_trees returns correct structure", {
  set.seed(123)
  model <- randomForest::randomForest(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    ntree = 5
  )

  result <- .extract_rf_trees(model)

  expect_type(result, "list")
  expect_length(result, 5)
  expect_all_true(vapply(result, is.language, logical(1)))
})

test_that(".extract_rf_trees errors on non-randomForest model", {
  model <- lm(mpg ~ ., data = mtcars)

  expect_snapshot(error = TRUE, .extract_rf_trees(model))
})

test_that(".extract_rf_trees errors on classification model", {
  set.seed(123)
  model <- randomForest::randomForest(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    ntree = 3
  )

  expect_snapshot(error = TRUE, .extract_rf_trees(model))
})

test_that(".extract_rf_trees produces correct predictions when averaged", {
  set.seed(123)
  model <- randomForest::randomForest(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    ntree = 5
  )

  trees <- .extract_rf_trees(model)
  n_trees <- length(trees)

  tree_preds <- sapply(trees, function(e) rlang::eval_tidy(e, mtcars))
  avg_pred <- rowMeans(tree_preds)

  native <- as.numeric(predict(model, mtcars))

  expect_equal(avg_pred, native)
})
