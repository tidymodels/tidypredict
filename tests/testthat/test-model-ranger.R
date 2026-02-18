test_that("returns the right output", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

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
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

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

  expect_equal(
    tidypredict_fit(model),
    tidypredict_fit(pm)
  )
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
