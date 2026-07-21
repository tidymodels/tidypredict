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
