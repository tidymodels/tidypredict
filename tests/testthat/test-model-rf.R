test_that("returns the right output", {
  set.seed(1234)

  model <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 3)

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(length(pm$trees), 3)
  expect_equal(pm$general$model, "randomForest")
  expect_equal(pm$general$version, 2)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("Model can be saved and re-loaded", {
  set.seed(1234)

  model <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 3)

  pm <- parse_model(model)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)

  expect_identical(
    round_print(tidypredict_fit(model)),
    round_print(tidypredict_fit(pm))
  )
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

test_that("classification models error with clear message (#193)", {
  set.seed(123)
  model <- randomForest::randomForest(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    ntree = 3
  )

  expect_snapshot(tidypredict_fit(model), error = TRUE)
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
