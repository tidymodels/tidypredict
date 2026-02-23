test_that("rpart_tree_info returns correct structure", {
  model <- rpart::rpart(mpg ~ cyl + wt, data = mtcars)
  tree_info <- rpart_tree_info(model)

  expect_s3_class(tree_info, "data.frame")
  expect_named(
    tree_info,
    c(
      "nodeID",
      "leftChild",
      "rightChild",
      "splitvarName",
      "splitval",
      "splitclass",
      "terminal",
      "prediction"
    )
  )
})

test_that("returns the right output", {
  model <- rpart::rpart(mpg ~ am + cyl, data = mtcars)
  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")
  expect_s3_class(pm, "list")
  expect_equal(pm$general$model, "rpart")
  expect_equal(pm$general$version, 3)

  expect_snapshot(rlang::expr_text(tf))
})

test_that("tidypredict_fit produces correct predictions", {
  model <- rpart::rpart(mpg ~ am + cyl, data = mtcars)

  fit_expr <- tidypredict_fit(model)
  fit_pred <- dplyr::mutate(mtcars, pred = !!fit_expr)$pred
  original_pred <- predict(model, mtcars)

  expect_equal(fit_pred, unname(original_pred))
})

test_that("formulas produce correct predictions - regression", {
  expect_snapshot(
    tidypredict_test(
      rpart::rpart(mpg ~ am + cyl + wt, data = mtcars),
      mtcars
    )
  )
})

test_that("formulas produce correct predictions - classification", {
  expect_snapshot(
    tidypredict_test(
      rpart::rpart(Species ~ ., data = iris),
      iris
    )
  )
})

test_that("categorical predictors work correctly", {
  mtcars2 <- mtcars
  mtcars2$cyl <- factor(mtcars2$cyl)

  expect_snapshot(
    tidypredict_test(
      rpart::rpart(mpg ~ cyl + wt, data = mtcars2),
      mtcars2
    )
  )
})

test_that("stump trees work correctly", {
  ctrl <- rpart::rpart.control(minsplit = 100, cp = 1)
  model <- rpart::rpart(mpg ~ cyl + disp, data = mtcars, control = ctrl)

  fit <- tidypredict_fit(model)

  expect_type(fit, "double")
  expect_equal(fit, mean(mtcars$mpg))
})

test_that("produced case_when uses .default", {
  model <- rpart::rpart(mpg ~ am + cyl, data = mtcars)

  fit <- tidypredict_fit(model)
  fit_text <- rlang::expr_text(fit)

  expect_match(fit_text, "\\.default")
})

# .extract_rpart_classprob tests ------------------------------------------

test_that(".extract_rpart_classprob returns list of expressions", {
  model <- rpart::rpart(Species ~ Sepal.Length + Sepal.Width, data = iris)

  exprs <- .extract_rpart_classprob(model)

  expect_type(exprs, "list")
  expect_length(exprs, 3)
  expect_true(all(vapply(exprs, typeof, character(1)) == "language"))
})

test_that(".extract_rpart_classprob results match predict probabilities", {
  model <- rpart::rpart(Species ~ Sepal.Length + Sepal.Width, data = iris)

  exprs <- .extract_rpart_classprob(model)
  eval_env <- rlang::new_environment(
    data = as.list(iris),
    parent = asNamespace("dplyr")
  )
  probs <- lapply(exprs, rlang::eval_tidy, env = eval_env)
  combined <- do.call(cbind, probs)

  native <- predict(model, type = "prob")

  expect_equal(unname(combined), unname(native))
})

test_that(".extract_rpart_classprob errors on non-rpart model", {
  expect_snapshot(.extract_rpart_classprob(list()), error = TRUE)
})

test_that(".extract_rpart_classprob errors on regression model", {
  model <- rpart::rpart(mpg ~ cyl + wt, data = mtcars)
  expect_snapshot(.extract_rpart_classprob(model), error = TRUE)
})

# Nested case_when tests --------------------------------------------------

test_that("tidypredict_fit matches original model predictions", {
  model <- rpart::rpart(mpg ~ cyl + wt, data = mtcars)

  fit_expr <- tidypredict_fit(model)
  fit_pred <- dplyr::mutate(mtcars, pred = !!fit_expr)$pred
  original_pred <- predict(model, mtcars)

  expect_equal(fit_pred, unname(original_pred))
})

test_that("tidypredict_fit works for classification", {
  model <- rpart::rpart(Species ~ ., data = iris)

  fit_expr <- tidypredict_fit(model)
  fit_pred <- dplyr::mutate(iris, pred = !!fit_expr)$pred
  original_pred <- as.character(predict(model, iris, type = "class"))

  expect_equal(fit_pred, original_pred)
})

test_that(".extract_rpart_classprob matches original model probabilities", {
  model <- rpart::rpart(Species ~ Sepal.Length + Sepal.Width, data = iris)

  exprs <- .extract_rpart_classprob(model)

  eval_env <- rlang::new_environment(
    data = as.list(iris),
    parent = asNamespace("dplyr")
  )

  probs <- lapply(exprs, rlang::eval_tidy, env = eval_env)
  combined <- do.call(cbind, probs)
  native <- predict(model, type = "prob")

  expect_equal(unname(combined), unname(native))
})

test_that(".rpart_tree_info_full is exported and works", {
  model <- rpart::rpart(mpg ~ cyl + wt, data = mtcars)

  tree_info <- .rpart_tree_info_full(model)

  expect_type(tree_info, "list")
  expect_named(
    tree_info,
    c(
      "nodeID",
      "leftChild",
      "rightChild",
      "splitvarName",
      "terminal",
      "prediction",
      "node_splits",
      "majority_left",
      "use_surrogates"
    )
  )
})
