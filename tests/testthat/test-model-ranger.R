test_that("returns the right output", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    mpg ~ cyl + disp + hp,
    data = mtcars,
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
  expect_equal(length(pm$tree_info_list), 3)
  expect_equal(pm$general$model, "ranger")
  expect_equal(pm$general$version, 3)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("tidypredict_fit produces correct predictions", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    num.trees = 3,
    max.depth = 2,
    seed = 100,
    num.threads = 2
  )

  fit_expr <- tidypredict_fit(model)
  fit_pred <- dplyr::mutate(mtcars, pred = !!fit_expr)$pred
  original_pred <- predict(model, mtcars)$predictions

  expect_equal(fit_pred, original_pred)
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

test_that("produced case_when uses .default", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    mpg ~ cyl + disp + hp,
    data = mtcars,
    num.trees = 3,
    max.depth = 2,
    seed = 100,
    num.threads = 2
  )

  fit <- tidypredict_fit(model)
  fit_text <- rlang::expr_text(fit)

  expect_match(fit_text, "\\.default")
})

test_that("classification models error with clear message (#191)", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    num.trees = 3,
    max.depth = 2,
    seed = 123,
    num.threads = 2
  )

  expect_snapshot(tidypredict_fit(model), error = TRUE)
})

# Tests for .extract_ranger_classprob()

test_that(".extract_ranger_classprob returns correct structure", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    num.trees = 3,
    max.depth = 2,
    seed = 123,
    num.threads = 2,
    probability = TRUE
  )

  result <- .extract_ranger_classprob(model)

  expect_type(result, "list")
  expect_length(result, 3)
  expect_named(result, levels(iris$Species))
  # Each class should have num.trees expressions
  expect_length(result[[1]], 3)
})

test_that(".extract_ranger_classprob errors on non-ranger model", {
  model <- lm(mpg ~ ., data = mtcars)

  expect_snapshot(error = TRUE, .extract_ranger_classprob(model))
})

test_that(".extract_ranger_classprob errors without probability = TRUE", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    Species ~ Sepal.Length + Sepal.Width,
    data = iris,
    num.trees = 3,
    max.depth = 2,
    seed = 123,
    num.threads = 2,
    probability = FALSE
  )

  expect_snapshot(error = TRUE, .extract_ranger_classprob(model))
})

test_that(".extract_ranger_classprob works with binary classification", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  mtcars$vs <- factor(mtcars$vs)
  model <- ranger::ranger(
    vs ~ disp + hp,
    data = mtcars,
    num.trees = 3,
    max.depth = 2,
    seed = 123,
    num.threads = 2,
    probability = TRUE
  )

  result <- .extract_ranger_classprob(model)

  expect_type(result, "list")
  expect_length(result, 2)
  expect_named(result, c("0", "1"))
})

test_that(".extract_ranger_classprob produces correct probabilities", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    Species ~ .,
    data = iris,
    num.trees = 5,
    max.depth = 3,
    seed = 123,
    num.threads = 2,
    probability = TRUE
  )

  class_trees <- .extract_ranger_classprob(model)
  n_trees <- model$num.trees

  # Sum probabilities for each class
  prob_sums <- sapply(names(class_trees), function(cls) {
    trees <- class_trees[[cls]]
    tree_vals <- sapply(trees, function(e) {
      rlang::eval_tidy(e, iris)
    })
    if (is.matrix(tree_vals)) rowSums(tree_vals) else tree_vals
  })

  # Calculate averaged probabilities
  probs <- prob_sums / n_trees

  # Compare to native predictions
  native <- predict(model, iris)$predictions

  expect_equal(unname(probs), unname(native), tolerance = 1e-10)
})

test_that(".extract_ranger_classprob works with single tree", {
  skip_on_cran()
  skip_on_os("windows")
  skip_on_os("linux")

  model <- ranger::ranger(
    Species ~ .,
    data = iris,
    num.trees = 1,
    max.depth = 3,
    seed = 123,
    num.threads = 2,
    probability = TRUE
  )

  result <- .extract_ranger_classprob(model)

  expect_type(result, "list")
  expect_length(result, 3)
  # Each class should have 1 expression
  expect_length(result[[1]], 1)
})
