test_that("returns the right output", {
  skip_if_not_installed("mboost")

  set.seed(1)
  model <- mboost::blackboost(
    mpg ~ wt + cyl,
    data = mtcars,
    control = mboost::boost_control(mstop = 10)
  )

  tf <- tidypredict_fit(model)
  expect_type(tf, "language")

  pm <- parse_model(model)
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "blackboost")
  expect_equal(pm$general$version, 3)
})

test_that("blackboost regression predictions match", {
  skip_if_not_installed("mboost")

  set.seed(1)
  model <- mboost::blackboost(
    mpg ~ wt + cyl,
    data = mtcars,
    control = mboost::boost_control(mstop = 10)
  )

  fit <- rlang::eval_tidy(tidypredict_fit(model), mtcars)
  expect_equal(fit, as.numeric(predict(model, newdata = mtcars)))
  expect_false(tidypredict_test(model, df = mtcars)$alert)
})

test_that("blackboost works with categorical predictors", {
  skip_if_not_installed("mboost")

  set.seed(1)
  df <- transform(mtcars, cyl = factor(cyl))
  model <- mboost::blackboost(
    mpg ~ wt + cyl,
    data = df,
    control = mboost::boost_control(mstop = 10)
  )

  expect_false(tidypredict_test(model, df = df)$alert)
})

test_that("blackboost respects mstop and nu", {
  skip_if_not_installed("mboost")

  set.seed(1)
  model <- mboost::blackboost(
    mpg ~ wt + cyl + disp,
    data = mtcars,
    control = mboost::boost_control(mstop = 50, nu = 0.3)
  )

  fit <- rlang::eval_tidy(tidypredict_fit(model), mtcars)
  expect_equal(fit, as.numeric(predict(model, newdata = mtcars)))
})

test_that("blackboost honours an mstop reduced after fitting (#306)", {
  skip_if_not_installed("mboost")

  # The `cvrisk()` workflow subsets a fitted model to the best iteration, which
  # sets `mstop` but leaves the stored ensemble at its full length.
  set.seed(1)
  model <- mboost::blackboost(
    mpg ~ wt + cyl + disp,
    data = mtcars,
    control = mboost::boost_control(mstop = 100)
  )
  model[30]
  expect_equal(mboost::mstop(model), 30)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), mtcars),
    as.numeric(predict(model, newdata = mtcars))
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(parse_model(model)), mtcars),
    as.numeric(predict(model, newdata = mtcars))
  )
})

test_that("blackboost supports SQL", {
  skip_if_not_installed("mboost")

  set.seed(1)
  model <- mboost::blackboost(
    mpg ~ wt + cyl,
    data = mtcars,
    control = mboost::boost_control(mstop = 5)
  )

  expect_s3_class(tidypredict_sql(model, dbplyr::simulate_dbi()), "sql")
})

test_that("parse_model roundtrips and produces correct predictions", {
  skip_if_not_installed("mboost")

  set.seed(1)
  model <- mboost::blackboost(
    mpg ~ wt + cyl,
    data = mtcars,
    control = mboost::boost_control(mstop = 10)
  )

  pm <- parse_model(model)
  expect_s3_class(pm, "pm_tree")
  expect_identical(tidypredict_fit(pm), tidypredict_fit(model))

  base <- as.numeric(predict(model, newdata = mtcars))
  parsed <- rlang::eval_tidy(tidypredict_fit(pm), mtcars)
  expect_equal(parsed, base)
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("mboost")
  skip_if_not_installed("yaml")

  set.seed(1)
  model <- mboost::blackboost(
    mpg ~ wt + cyl,
    data = mtcars,
    control = mboost::boost_control(mstop = 10)
  )

  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), tmp)
  reloaded <- as_parsed_model(yaml::read_yaml(tmp))

  base <- as.numeric(predict(model, newdata = mtcars))
  parsed <- rlang::eval_tidy(tidypredict_fit(reloaded), mtcars)
  expect_equal(parsed, base, tolerance = 1e-6)
})

test_that("non-Gaussian families error with clear message", {
  skip_if_not_installed("mboost")

  set.seed(1)
  model <- mboost::blackboost(
    factor(vs) ~ wt + cyl,
    data = mtcars,
    family = mboost::Binomial(),
    control = mboost::boost_control(mstop = 10)
  )

  expect_snapshot(error = TRUE, tidypredict_fit(model))
  expect_snapshot(error = TRUE, parse_model(model))
})

test_that("a missing predictor gives NA rather than a random draw (#294)", {
  skip_if_not_installed("mboost")

  set.seed(1)
  model <- mboost::blackboost(mpg ~ wt + disp + cyl, data = mtcars)

  df <- mtcars
  df$wt[1:4] <- NA_real_
  fit <- rlang::eval_tidy(tidypredict_fit(model), df)

  expect_length(fit, nrow(df))
  expect_true(all(is.na(fit[1:4])))
  expect_equal(
    fit[-(1:4)],
    as.numeric(predict(model, mtcars))[-(1:4)]
  )
})
