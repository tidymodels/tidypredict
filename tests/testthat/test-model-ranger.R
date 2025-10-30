test_that("returns the right output", {
  model <- ranger::ranger(
    Species ~ .,
    data = iris,
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
  expect_equal(length(pm$trees), 3)
  expect_equal(pm$general$model, "ranger")
  expect_equal(pm$general$version, 2)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("Model can be saved and re-loaded", {
  model <- ranger::ranger(
    Species ~ .,
    data = iris,
    num.trees = 3,
    max.depth = 2,
    seed = 100,
    num.threads = 2
  )

  pm <- parse_model(model)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)
  # Not OS stable
  expect_silent(tidypredict_fit(pm))
})

test_that("formulas produces correct predictions", {
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
