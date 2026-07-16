test_that("works with parsnip model specification", {
  etitanic_fac <- dplyr::mutate(earth::etitanic, survived = as.factor(survived))

  # Classification
  model <- parsnip::fit(
    parsnip::set_engine(parsnip::mars(mode = "classification"), "earth"),
    survived ~ age + sibsp,
    data = etitanic_fac
  )

  expect_snapshot(
    tidypredict_test(
      model,
      df = etitanic_fac
    )
  )

  # Regression
  model <- parsnip::fit(
    parsnip::set_engine(parsnip::mars(mode = "regression"), "earth"),
    survived ~ age + sibsp,
    data = etitanic
  )

  expect_snapshot(
    tidypredict_test(
      model,
      df = etitanic_fac
    )
  )
})

test_that("works with decision_tree() and the C5.0 engine", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- as.factor(df$vs)

  model <- parsnip::fit(
    parsnip::set_engine(
      parsnip::decision_tree(mode = "classification"),
      "C5.0"
    ),
    vs ~ wt + cyl + mpg,
    data = df
  )

  expect_type(tidypredict_fit(model), "language")
  expect_s3_class(tidypredict_sql(model, dbplyr::simulate_dbi()), "sql")
  expect_snapshot(tidypredict_test(model, df = df))
})

test_that("works with linear_reg() and the glm engine", {
  model <- parsnip::fit(
    parsnip::set_engine(parsnip::linear_reg(), "glm"),
    mpg ~ wt + cyl,
    data = mtcars
  )

  expect_type(tidypredict_fit(model), "language")

  expect_snapshot(
    tidypredict_test(model, df = mtcars)
  )
})

test_that("works with logistic_reg() and the LiblineaR engine", {
  skip_if_not_installed("LiblineaR")

  df <- mtcars
  df$am <- factor(df$am)

  ridge <- parsnip::fit(
    parsnip::set_engine(
      parsnip::logistic_reg(penalty = 0.1, mixture = 0),
      "LiblineaR"
    ),
    am ~ mpg + cyl + hp,
    data = df
  )
  lasso <- parsnip::fit(
    parsnip::set_engine(
      parsnip::logistic_reg(penalty = 0.1, mixture = 1),
      "LiblineaR"
    ),
    am ~ mpg + cyl + hp,
    data = df
  )

  for (model in list(ridge, lasso)) {
    expect_type(tidypredict_fit(model), "language")
    expect_false(tidypredict_test(model, df = df)$alert)
    expect_s3_class(
      tidypredict_sql(model, dbplyr::simulate_dbi()),
      "sql"
    )
  }
})

test_that("works with decision_tree() and the rpart engine", {
  skip_if_not_installed("rpart")

  # Regression
  reg <- parsnip::fit(
    parsnip::set_mode(
      parsnip::set_engine(parsnip::decision_tree(), "rpart"),
      "regression"
    ),
    mpg ~ wt + cyl,
    data = mtcars
  )

  expect_type(tidypredict_fit(reg), "language")
  expect_s3_class(tidypredict_sql(reg, dbplyr::simulate_dbi()), "sql")
  expect_snapshot(tidypredict_test(reg, df = mtcars))

  # Classification
  df <- mtcars
  df$am <- factor(df$am)
  cls <- parsnip::fit(
    parsnip::set_mode(
      parsnip::set_engine(parsnip::decision_tree(), "rpart"),
      "classification"
    ),
    am ~ mpg + cyl + hp,
    data = df
  )

  expect_type(tidypredict_fit(cls), "language")
  expect_s3_class(tidypredict_sql(cls, dbplyr::simulate_dbi()), "sql")
})

test_that("works with linear_reg() and the quantreg engine", {
  skip_if_not_installed("quantreg")

  model <- parsnip::fit(
    parsnip::set_mode(
      parsnip::set_engine(parsnip::linear_reg(), "quantreg"),
      "quantile regression",
      quantile_levels = 0.5
    ),
    mpg ~ wt + cyl,
    data = mtcars
  )

  expect_type(tidypredict_fit(model), "language")

  expect_snapshot(
    tidypredict_test(model, df = mtcars)
  )
})
