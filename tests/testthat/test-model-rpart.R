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
  expect_equal(pm$general$version, 2)

  expect_snapshot(rlang::expr_text(tf))
})

test_that("Model can be saved and re-loaded", {
  model <- rpart::rpart(mpg ~ am + cyl, data = mtcars)
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

# .extract_rpart_classprob tests ------------------------------------------

test_that(".extract_rpart_classprob returns list of expressions", {
  model <- rpart::rpart(Species ~ Sepal.Length + Sepal.Width, data = iris)

  exprs <- .extract_rpart_classprob(model)

  expect_type(exprs, "list")
  expect_length(exprs, 3)
  for (expr in exprs) {
    expect_type(expr, "language")
  }
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
