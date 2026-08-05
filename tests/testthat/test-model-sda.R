# `predict.sda()` runs its posterior probabilities through `zapsmall()`, so they
# only agree with the exact softmax to about 7 decimal places.
sda_tolerance <- 1e-6

test_that("returns the right output", {
  skip_if_not_installed("sda")

  model <- sda::sda(as.matrix(iris[1:4]), iris$Species, verbose = FALSE)

  tf <- tidypredict_fit(model)
  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))
  expect_true(all(vapply(tf, is.language, logical(1))))

  pm <- parse_model(model)
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 3)
  expect_equal(pm$general$model, "sda")
  expect_equal(pm$general$version, 2)
  expect_equal(pm$classes, levels(iris$Species))

  lps <- lapply(pm$class_terms, build_linear_predictor)
  expect_snapshot(lapply(lps, round_print))
})

test_that("predictions match native predict", {
  skip_if_not_installed("sda")

  x <- as.matrix(iris[1:4])
  model <- sda::sda(x, iris$Species, verbose = FALSE)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))
  native <- sda::predict.sda(model, x, verbose = FALSE)$posterior

  expect_equal(unname(probs), unname(native), tolerance = sda_tolerance)
  expect_equal(unname(rowSums(probs)), rep(1, nrow(iris)))
})

test_that("diagonal discriminant analysis is handled", {
  skip_if_not_installed("sda")

  x <- as.matrix(iris[1:4])
  model <- sda::sda(x, iris$Species, diagonal = TRUE, verbose = FALSE)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))

  expect_equal(
    unname(probs),
    unname(sda::predict.sda(model, x, verbose = FALSE)$posterior),
    tolerance = sda_tolerance
  )
})

test_that("explicit shrinkage intensities are handled", {
  skip_if_not_installed("sda")

  x <- as.matrix(iris[1:4])
  model <- sda::sda(
    x,
    iris$Species,
    lambda = 0.2,
    lambda.var = 0.5,
    lambda.freqs = 0.1,
    verbose = FALSE
  )

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))

  expect_equal(
    unname(probs),
    unname(sda::predict.sda(model, x, verbose = FALSE)$posterior),
    tolerance = sda_tolerance
  )
})

test_that("binary outcomes are handled", {
  skip_if_not_installed("sda")

  df <- transform(mtcars, am = factor(am))
  x <- as.matrix(mtcars[c("mpg", "wt")])
  model <- sda::sda(x, df$am, verbose = FALSE)

  tf <- tidypredict_fit(model)
  expect_named(tf, c("0", "1"))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, df))

  expect_equal(
    unname(probs),
    unname(sda::predict.sda(model, x, verbose = FALSE)$posterior),
    tolerance = sda_tolerance
  )
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("sda")
  skip_if_not_installed("yaml")

  model <- sda::sda(as.matrix(iris[1:4]), iris$Species, verbose = FALSE)

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  from_model <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))
  from_pm <- sapply(tidypredict_fit(pm), \(f) rlang::eval_tidy(f, iris))

  expect_equal(from_model, from_pm, tolerance = sda_tolerance)
})

test_that("tidypredict_test errors for sda models", {
  skip_if_not_installed("sda")

  model <- sda::sda(as.matrix(iris[1:4]), iris$Species, verbose = FALSE)

  expect_snapshot(error = TRUE, tidypredict_test(model, iris))
})

test_that("SQL translation works", {
  skip_if_not_installed("sda")
  skip_if_not_installed("dbplyr")

  model <- sda::sda(as.matrix(iris[1:4]), iris$Species, verbose = FALSE)

  sql <- tidypredict_sql(model, dbplyr::simulate_dbi())

  expect_named(sql, levels(iris$Species))
  expect_true(all(vapply(sql, \(x) inherits(x, "sql"), logical(1))))
})

test_that("sda is handled with parsnip", {
  skip_if_not_installed("sda")
  skip_if_not_installed("discrim")

  spec <- parsnip::discrim_linear(engine = "sda")
  model <- parsnip::fit(spec, Species ~ ., iris)

  tf <- tidypredict_fit(model)

  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, iris))
  native <- as.matrix(predict(model, iris, type = "prob"))

  expect_equal(unname(probs), unname(native), tolerance = sda_tolerance)
})

test_that("categorical predictors are handled with parsnip", {
  skip_if_not_installed("sda")
  skip_if_not_installed("discrim")

  df <- transform(mtcars, cyl = factor(cyl), gear = factor(gear))
  spec <- parsnip::discrim_linear(engine = "sda")
  model <- parsnip::fit(spec, cyl ~ mpg + gear + disp, df)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))
  native <- as.matrix(predict(model, df, type = "prob"))

  expect_equal(unname(probs), unname(native), tolerance = sda_tolerance)
})
