test_that("returns the right output", {
  skip_if_not_installed("nnet")

  model <- nnet::multinom(Species ~ ., data = iris, trace = FALSE)

  tf <- tidypredict_fit(model)
  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))
  expect_true(all(vapply(tf, is.language, logical(1))))

  pm <- parse_model(model)
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 3)
  expect_equal(pm$general$model, "multinom")
  expect_equal(pm$general$version, 2)
  expect_equal(pm$classes, levels(iris$Species))

  lps <- lapply(pm$class_terms, build_linear_predictor)
  expect_snapshot(lapply(lps, round_print))
})

test_that("predictions match native predict", {
  skip_if_not_installed("nnet")

  model <- nnet::multinom(Species ~ ., data = iris, trace = FALSE)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))
  native <- predict(model, iris, type = "probs")

  expect_equal(unname(probs), unname(native))
  expect_equal(unname(rowSums(probs)), rep(1, nrow(iris)))
})

test_that("categorical predictors are handled", {
  skip_if_not_installed("nnet")

  df <- transform(mtcars, cyl = factor(cyl), gear = factor(gear))
  model <- nnet::multinom(cyl ~ mpg + gear + disp, data = df, trace = FALSE)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))

  expect_equal(unname(probs), unname(predict(model, df, type = "probs")))
})

test_that("binary outcomes are handled", {
  skip_if_not_installed("nnet")

  df <- transform(mtcars, am = factor(am))
  model <- nnet::multinom(am ~ mpg + wt, data = df, trace = FALSE)

  tf <- tidypredict_fit(model)
  expect_named(tf, c("0", "1"))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, df))

  expect_equal(unname(probs[, 2]), unname(predict(model, df, type = "probs")))
})

test_that("interactions and weights are handled", {
  skip_if_not_installed("nnet")

  df <- transform(mtcars, cyl = factor(cyl))
  model <- nnet::multinom(
    cyl ~ mpg * wt,
    data = df,
    weights = rep(c(1, 2), 16),
    trace = FALSE
  )

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))

  expect_equal(unname(probs), unname(predict(model, df, type = "probs")))
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("yaml")

  model <- nnet::multinom(Species ~ ., data = iris, trace = FALSE)

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  from_model <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))
  from_pm <- sapply(tidypredict_fit(pm), \(f) rlang::eval_tidy(f, iris))

  expect_equal(from_model, from_pm, tolerance = 1e-6)
})

test_that("tidypredict_test errors for multinom models", {
  skip_if_not_installed("nnet")

  model <- nnet::multinom(Species ~ ., data = iris, trace = FALSE)

  expect_snapshot(error = TRUE, tidypredict_test(model, iris))
})

test_that("inline functions in the formula are rejected", {
  skip_if_not_installed("nnet")

  model <- nnet::multinom(
    Species ~ log(Sepal.Width),
    data = iris,
    trace = FALSE
  )

  expect_snapshot(error = TRUE, tidypredict_fit(model))
})

test_that("SQL translation works", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("dbplyr")

  model <- nnet::multinom(Species ~ ., data = iris, trace = FALSE)

  sql <- tidypredict_sql(model, dbplyr::simulate_dbi())

  expect_named(sql, levels(iris$Species))
  expect_true(all(vapply(sql, \(x) inherits(x, "sql"), logical(1))))
})

test_that("multinom is handled with parsnip", {
  skip_if_not_installed("nnet")
  skip_if_not_installed("parsnip")

  spec <- parsnip::multinom_reg(engine = "nnet")
  model <- parsnip::fit(spec, Species ~ ., iris)

  tf <- tidypredict_fit(model)

  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, iris))
  native <- as.matrix(predict(model, iris, type = "prob"))

  expect_equal(unname(probs), unname(native))
})

test_that("a coefficient label colliding with a variable name works (#376)", {
  skip_if_not_installed("nnet")

  set.seed(1)
  df <- data.frame(
    g = factor(rep(c("x1", "y2", "z3"), length.out = 60)),
    gy2 = rnorm(60)
  )
  df$y <- factor(ifelse(rnorm(60) + df$gy2 + as.numeric(df$g) > 2, "a", "b"))

  model <- nnet::multinom(y ~ g + gy2, data = df, trace = FALSE)
  probs <- rlang::eval_tidy(tidypredict_fit(model)[["b"]], df)

  expect_equal(probs, unname(predict(model, df, type = "probs")))
})
