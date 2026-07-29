test_that("returns the right output", {
  model <- parsnip::nullmodel(mtcars[-1], mtcars$mpg)

  tf <- tidypredict_fit(model)
  expect_equal(tf, mean(mtcars$mpg))

  pm <- parse_model(model)
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "nullmodel")
  expect_equal(pm$general$version, 2)
  expect_snapshot(rlang::expr_text(tf))
})

test_that("regression predictions match native predict", {
  model <- parsnip::nullmodel(mtcars[-1], mtcars$mpg)

  expect_snapshot(tidypredict_test(model, mtcars))
})

test_that("classification returns one probability per class", {
  model <- parsnip::nullmodel(iris[-5], iris$Species)

  tf <- tidypredict_fit(model)
  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))

  pm <- parse_model(model)
  expect_equal(length(pm), 3)
  expect_equal(pm$general$type, "nullmodel_classification")
  expect_equal(pm$classes, levels(iris$Species))

  probs <- vapply(tf, as.numeric, numeric(1))
  native <- parsnip:::predict.nullmodel(model, iris, type = "prob")

  expect_equal(unname(probs), unname(unlist(native[1, ])))
  expect_equal(sum(probs), 1)
})

test_that("unbalanced and binary outcomes are handled", {
  df <- transform(mtcars, am = factor(am))
  model <- parsnip::nullmodel(df[-9], df$am)

  tf <- tidypredict_fit(model)
  expect_named(tf, c("0", "1"))

  probs <- vapply(tf, as.numeric, numeric(1))
  native <- parsnip:::predict.nullmodel(model, df, type = "prob")

  expect_equal(unname(probs), unname(unlist(native[1, ])))
})

test_that("categorical predictors are ignored", {
  df <- transform(mtcars, cyl = factor(cyl))
  model <- parsnip::nullmodel(df[c("cyl", "wt")], df$mpg)

  expect_equal(tidypredict_fit(model), mean(df$mpg))
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("yaml")

  model <- parsnip::nullmodel(iris[-5], iris$Species)

  pm <- parse_model(model)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  # YAML only keeps a limited number of digits
  expect_equal(tidypredict_fit(pm), tidypredict_fit(model), tolerance = 1e-6)

  model <- parsnip::nullmodel(mtcars[-1], mtcars$mpg)

  pm <- parse_model(model)
  yaml::write_yaml(pm, mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  expect_equal(tidypredict_fit(pm), tidypredict_fit(model))
})

test_that("tidypredict_test errors for classification nullmodel", {
  model <- parsnip::nullmodel(iris[-5], iris$Species)

  expect_snapshot(error = TRUE, tidypredict_test(model, iris))
})

test_that("SQL translation works", {
  skip_if_not_installed("dbplyr")

  model <- parsnip::nullmodel(mtcars[-1], mtcars$mpg)
  sql <- tidypredict_sql(model, dbplyr::simulate_dbi())
  expect_s3_class(sql[[1]], "sql")

  model <- parsnip::nullmodel(iris[-5], iris$Species)
  sql <- tidypredict_sql(model, dbplyr::simulate_dbi())

  expect_named(sql, levels(iris$Species))
  expect_true(all(vapply(sql, \(x) inherits(x, "sql"), logical(1))))
})

test_that("null_model is handled with parsnip", {
  spec <- parsnip::null_model(mode = "regression")
  model <- parsnip::fit(spec, mpg ~ ., mtcars)

  expect_equal(tidypredict_fit(model), mean(mtcars$mpg))

  spec <- parsnip::null_model(mode = "classification")
  model <- parsnip::fit(spec, Species ~ ., iris)

  probs <- vapply(tidypredict_fit(model), as.numeric, numeric(1))
  native <- predict(model, iris, type = "prob")

  expect_equal(unname(probs), unname(unlist(native[1, ])))
})
