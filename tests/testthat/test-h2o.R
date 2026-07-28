test_that("works with H2O GBM regression models", {
  skip_if_no_h2o()

  model <- parsnip::fit(
    parsnip::set_engine(
      parsnip::boost_tree(mode = "regression", trees = 10),
      "h2o_gbm"
    ),
    mpg ~ wt + cyl + hp,
    data = mtcars
  )

  expect_type(tidypredict_fit(model$fit), "language")
  expect_s3_class(tidypredict_sql(model$fit, dbplyr::simulate_dbi()), "sql")
  expect_false(tidypredict_test(model$fit, df = mtcars)$alert)
})

test_that("works with H2O GBM binary classification models", {
  skip_if_no_h2o()

  df <- mtcars
  df$vs <- factor(df$vs)
  model <- parsnip::fit(
    parsnip::set_engine(
      parsnip::boost_tree(mode = "classification", trees = 10),
      "h2o_gbm"
    ),
    vs ~ wt + cyl + hp,
    data = df
  )

  expect_type(tidypredict_fit(model$fit), "language")
  expect_false(tidypredict_test(model$fit, df = df)$alert)
})

test_that("works with H2O GBM multiclass classification models", {
  skip_if_no_h2o()

  model <- parsnip::fit(
    parsnip::set_engine(
      parsnip::boost_tree(mode = "classification", trees = 10),
      "h2o_gbm"
    ),
    Species ~ .,
    data = iris
  )

  fit <- tidypredict_fit(model$fit)
  expect_type(fit, "list")
  expect_named(fit, levels(iris$Species))
  expect_false(tidypredict_test(model$fit, df = iris)$alert)
})

test_that("works with H2O GBM models with categorical predictors", {
  skip_if_no_h2o()

  df <- mtcars
  df$cyl <- factor(df$cyl)
  df$gear <- factor(df$gear)
  model <- parsnip::fit(
    parsnip::set_engine(
      parsnip::boost_tree(mode = "regression", trees = 10),
      "h2o_gbm"
    ),
    mpg ~ cyl + gear + wt,
    data = df
  )

  expect_type(tidypredict_fit(model$fit), "language")
  expect_false(tidypredict_test(model$fit, df = df)$alert)
})

test_that("works with H2O RuleFit regression models", {
  skip_if_no_h2o()

  model <- parsnip::fit(
    parsnip::set_engine(parsnip::rule_fit(mode = "regression"), "h2o"),
    mpg ~ wt + hp + disp,
    data = mtcars
  )

  expect_type(tidypredict_fit(model$fit), "language")
  expect_s3_class(tidypredict_sql(model$fit, dbplyr::simulate_dbi()), "sql")
  expect_false(tidypredict_test(model$fit, df = mtcars)$alert)
})

test_that("works with H2O RuleFit binary classification models", {
  skip_if_no_h2o()

  df <- mtcars
  df$vs <- factor(df$vs)
  model <- parsnip::fit(
    parsnip::set_engine(parsnip::rule_fit(mode = "classification"), "h2o"),
    vs ~ wt + hp + disp + mpg,
    data = df
  )

  expect_type(tidypredict_fit(model$fit), "language")
  expect_false(tidypredict_test(model$fit, df = df)$alert)
})

test_that("works with H2O RuleFit models with categorical predictors", {
  skip_if_no_h2o()

  df <- mtcars
  df$cyl <- factor(df$cyl)
  df$gear <- factor(df$gear)
  model <- parsnip::fit(
    parsnip::set_engine(parsnip::rule_fit(mode = "regression"), "h2o"),
    mpg ~ cyl + gear + wt + hp,
    data = df
  )

  expect_type(tidypredict_fit(model$fit), "language")
  expect_false(tidypredict_test(model$fit, df = df)$alert)
})

test_that("works with H2O RuleFit models made up of linear terms only", {
  skip_if_no_h2o()

  df <- mtcars
  df$cyl <- factor(df$cyl)
  model <- h2o::h2o.rulefit(
    x = c("wt", "hp", "cyl"),
    y = "mpg",
    training_frame = h2o::as.h2o(df),
    model_type = "linear",
    lambda = 0
  )

  expect_type(tidypredict_fit(model), "language")
  expect_false(tidypredict_test(model, df = df)$alert)
})

test_that("multiclass H2O RuleFit models are not supported", {
  skip_if_no_h2o()

  model <- parsnip::fit(
    parsnip::set_engine(parsnip::rule_fit(mode = "classification"), "h2o"),
    Species ~ .,
    data = iris
  )

  expect_snapshot(tidypredict_fit(model$fit), error = TRUE)
})
