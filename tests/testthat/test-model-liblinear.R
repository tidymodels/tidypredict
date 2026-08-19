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

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("yaml")
  skip_if_not_installed("LiblineaR")

  df <- mtcars
  df$am <- factor(df$am)
  x <- as.matrix(df[, c("mpg", "cyl")])
  model <- LiblineaR::LiblineaR(data = x, target = df$am, type = 0)
  model$W <- round(model$W, 7)

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  expect_identical(
    tidypredict_fit(model),
    tidypredict_fit(pm)
  )
})

test_that("predictions match predict() for SVM classification types", {
  skip_if_not_installed("LiblineaR")

  df <- mtcars
  df$am <- factor(df$am)
  x <- as.matrix(df[, c("mpg", "cyl", "hp")])

  for (type in 1:5) {
    model <- LiblineaR::LiblineaR(data = x, target = df$am, type = type)
    te <- rlang::eval_tidy(tidypredict_fit(model), df)
    target <- as.character(model$ClassNames)[[1]]
    base <- predict(model, x, decisionValues = TRUE)$decisionValues[, target]
    expect_equal(te, unname(base), tolerance = 1e-10)
    expect_false(tidypredict_test(model, df)$alert)
  }
})

test_that("predictions match predict() for regression types", {
  skip_if_not_installed("LiblineaR")

  x <- as.matrix(mtcars[, c("wt", "hp", "disp")])

  for (type in 11:13) {
    model <- suppressWarnings(
      LiblineaR::LiblineaR(data = x, target = mtcars$mpg, type = type)
    )
    te <- rlang::eval_tidy(tidypredict_fit(model), mtcars)
    base <- predict(model, x)$predictions
    expect_equal(te, unname(base), tolerance = 1e-10)
    expect_false(tidypredict_test(model, mtcars)$alert)
  }
})

test_that("a non-default bias, cost and class weights are handled", {
  skip_if_not_installed("LiblineaR")

  df <- mtcars
  df$am <- factor(df$am)
  x <- as.matrix(df[, c("mpg", "cyl")])

  # `bias` is folded into the intercept, so a value other than the default `1`
  # scales the weight the model stores for it.
  model <- LiblineaR::LiblineaR(data = x, target = df$am, type = 0, bias = 5)
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(predict(model, x, proba = TRUE)$probabilities[, "1"]),
    tolerance = 1e-10
  )

  model <- LiblineaR::LiblineaR(
    data = x,
    target = df$am,
    type = 0,
    cost = 10,
    wi = c("0" = 1, "1" = 4)
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(predict(model, x, proba = TRUE)$probabilities[, "1"]),
    tolerance = 1e-10
  )
})

test_that("a single-column model matrix is handled", {
  skip_if_not_installed("LiblineaR")

  df <- mtcars
  df$am <- factor(df$am)
  x <- as.matrix(df[, "mpg", drop = FALSE])
  model <- LiblineaR::LiblineaR(data = x, target = df$am, type = 0)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(predict(model, x, proba = TRUE)$probabilities[, "1"]),
    tolerance = 1e-10
  )
})

test_that("errors on unsupported and multiclass models", {
  skip_if_not_installed("LiblineaR")

  multi_lr <- LiblineaR::LiblineaR(
    data = as.matrix(iris[, 1:4]),
    target = iris$Species,
    type = 0
  )
  expect_snapshot(tidypredict_fit(multi_lr), error = TRUE)

  multi_svm <- LiblineaR::LiblineaR(
    data = as.matrix(iris[, 1:4]),
    target = iris$Species,
    type = 1
  )
  expect_snapshot(tidypredict_fit(multi_svm), error = TRUE)
})

test_that("SQL translation works", {
  skip_if_not_installed("dbplyr")
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
