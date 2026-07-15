test_that("returns the right output", {
  skip_if_not_installed("LiblineaR")

  df <- mtcars
  df$am <- factor(df$am)
  x <- as.matrix(df[, c("mpg", "cyl")])
  model <- LiblineaR::LiblineaR(data = x, target = df$am, type = 0)
  model$W <- round(model$W, 7)

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "LiblineaR")
  expect_equal(pm$general$version, 2)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("predictions match predict() for logistic types", {
  skip_if_not_installed("LiblineaR")

  df <- mtcars
  df$am <- factor(df$am)
  x <- as.matrix(df[, c("mpg", "cyl", "hp")])

  for (type in c(0, 6, 7)) {
    model <- LiblineaR::LiblineaR(data = x, target = df$am, type = type)
    te <- rlang::eval_tidy(tidypredict_fit(model), df)
    base <- predict(model, x, proba = TRUE)$probabilities[, "1"]
    expect_equal(te, unname(base), tolerance = 1e-10)
  }
})

test_that("works without a bias term", {
  skip_if_not_installed("LiblineaR")

  df <- mtcars
  df$am <- factor(df$am)
  x <- as.matrix(df[, c("mpg", "cyl")])
  model <- LiblineaR::LiblineaR(data = x, target = df$am, type = 0, bias = -1)

  expect_type(tidypredict_fit(model), "language")
  expect_false(tidypredict_test(model, df)$alert)
})

test_that("tidypredict_test agrees with predict()", {
  skip_if_not_installed("LiblineaR")

  df <- mtcars
  df$am <- factor(df$am)
  x <- as.matrix(df[, c("mpg", "cyl")])
  model <- LiblineaR::LiblineaR(data = x, target = df$am, type = 0)

  expect_false(tidypredict_test(model, df)$alert)
})

test_that("Model can be saved and re-loaded", {
  skip_if_not_installed("LiblineaR")

  df <- mtcars
  df$am <- factor(df$am)
  x <- as.matrix(df[, c("mpg", "cyl")])
  model <- LiblineaR::LiblineaR(data = x, target = df$am, type = 0)
  model$W <- round(model$W, 7)

  pm <- parse_model(model)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  expect_identical(
    tidypredict_fit(model),
    tidypredict_fit(pm)
  )
})

test_that("errors on non-logistic and multiclass models", {
  skip_if_not_installed("LiblineaR")

  df <- mtcars
  df$am <- factor(df$am)
  x <- as.matrix(df[, c("mpg", "cyl")])
  svm <- LiblineaR::LiblineaR(data = x, target = df$am, type = 1)
  expect_snapshot(tidypredict_fit(svm), error = TRUE)

  multi <- LiblineaR::LiblineaR(
    data = as.matrix(iris[, 1:4]),
    target = iris$Species,
    type = 0
  )
  expect_snapshot(tidypredict_fit(multi), error = TRUE)
})

test_that("SQL translation works", {
  skip_if_not_installed("LiblineaR")

  df <- mtcars
  df$am <- factor(df$am)
  x <- as.matrix(df[, c("mpg", "cyl")])
  model <- LiblineaR::LiblineaR(data = x, target = df$am, type = 0)

  expect_s3_class(
    tidypredict_sql(model, dbplyr::simulate_dbi()),
    "sql"
  )
})
