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
