test_that("returns the right output", {
  model <- glmnet::glmnet(mtcars[, -1], mtcars$mpg, lambda = 1)

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "glmnet")
  expect_equal(pm$general$version, 1)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("Model can be saved and re-loaded", {
  model <- glmnet::glmnet(mtcars[, -1], mtcars$mpg, lambda = 1)

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
  # gaussian
  expect_snapshot(
    tidypredict_test(
      glmnet::glmnet(mtcars[, -1], mtcars$mpg, family = "gaussian", lambda = 1),
      mtcars[, -1]
    )
  )

  # binomial
  expect_snapshot(
    tidypredict_test(
      glmnet::glmnet(mtcars[, -8], mtcars$vs, family = "binomial", lambda = 1),
      mtcars[, -1]
    )
  )

  # poisson
  expect_snapshot(
    tidypredict_test(
      glmnet::glmnet(mtcars[, -8], mtcars$vs, family = "poisson", lambda = 1),
      mtcars[, -1]
    )
  )
})

test_that("errors if more than 1 penalty is selected", {
  model <- glmnet::glmnet(mtcars[, -1], mtcars$mpg)

  expect_snapshot(
    error = TRUE,
    tidypredict_fit(model)
  )

  model <- glmnet::glmnet(mtcars[, -1], mtcars$mpg, lambda = c(1, 5))

  expect_snapshot(
    error = TRUE,
    tidypredict_fit(model)
  )
})

test_that("glmnet are handeld neatly with parsnip", {
  spec <- parsnip::linear_reg(engine = "glmnet", penalty = 1)

  model <- parsnip::fit(spec, mpg ~ ., mtcars)

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "glmnet")
  expect_equal(pm$general$version, 1)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})
