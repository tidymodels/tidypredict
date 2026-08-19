test_that("returns the right output", {
  skip_if_not_installed("parsnip")
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
  skip_if_not_installed("parsnip")
  model <- parsnip::nullmodel(mtcars[-1], mtcars$mpg)

  expect_false(tidypredict_test(model, mtcars)$alert)
})

test_that("classification returns one probability per class", {
  skip_if_not_installed("parsnip")
  model <- parsnip::nullmodel(iris[-5], iris$Species)

  tf <- tidypredict_fit(model)
  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))

  pm <- parse_model(model)
  expect_equal(length(pm), 3)
  expect_equal(pm$general$type, "nullmodel_classification")
  expect_equal(pm$classes, levels(iris$Species))

  probs <- vapply(tf, as.numeric, numeric(1))
  native <- predict(model, iris, type = "prob")

  expect_equal(unname(probs), unname(unlist(native[1, ])))
  expect_equal(sum(probs), 1)
})

test_that("unbalanced and binary outcomes are handled", {
  skip_if_not_installed("parsnip")
  df <- transform(mtcars, am = factor(am))
  model <- parsnip::nullmodel(df[-9], df$am)

  tf <- tidypredict_fit(model)
  expect_named(tf, c("0", "1"))

  probs <- vapply(tf, as.numeric, numeric(1))
  native <- predict(model, df, type = "prob")

  expect_equal(unname(probs), unname(unlist(native[1, ])))
})

test_that("categorical predictors are ignored", {
  skip_if_not_installed("parsnip")
  df <- transform(mtcars, cyl = factor(cyl))
  model <- parsnip::nullmodel(df[c("cyl", "wt")], df$mpg)

  expect_equal(tidypredict_fit(model), mean(df$mpg))
})

test_that("an unused outcome level gets a probability of zero", {
  skip_if_not_installed("parsnip")
  y <- factor(c(rep("a", 10), rep("b", 5)), levels = c("a", "b", "unused"))
  model <- parsnip::nullmodel(data.frame(x = seq_along(y)), y)

  tf <- tidypredict_fit(model)
  expect_named(tf, c("a", "b", "unused"))

  probs <- vapply(tf, as.numeric, numeric(1))
  native <- predict(model, data.frame(x = 1), type = "prob")

  expect_equal(unname(probs), unname(unlist(native[1, ])))
})

test_that("NA in the outcome and in the newdata are handled", {
  skip_if_not_installed("parsnip")
  model <- parsnip::nullmodel(data.frame(x = 1:5), c(1, 2, NA, 4, 5))

  expect_equal(
    tidypredict_fit(model),
    predict(model, data.frame(x = 1))
  )

  # The predictors are ignored, so a missing one cannot change the prediction.
  model <- parsnip::nullmodel(mtcars[-1], mtcars$mpg)
  nd <- mtcars
  nd$wt[1:2] <- NA

  expect_equal(
    rep(rlang::eval_tidy(tidypredict_fit(model), nd), nrow(nd)),
    predict(model, nd)
  )
})

test_that("single-row training data is handled", {
  skip_if_not_installed("parsnip")
  model <- parsnip::nullmodel(data.frame(x = 1), 5)

  expect_equal(
    tidypredict_fit(model),
    predict(model, data.frame(x = 1))
  )
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("parsnip")
  skip_if_not_installed("yaml")

  model <- parsnip::nullmodel(iris[-5], iris$Species)

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
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
  skip_if_not_installed("parsnip")
  model <- parsnip::nullmodel(iris[-5], iris$Species)

  expect_snapshot(error = TRUE, tidypredict_test(model, iris))
})

test_that("SQL translation works", {
  skip_if_not_installed("parsnip")
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
  skip_if_not_installed("parsnip")
  spec <- parsnip::null_model(mode = "regression")
  model <- parsnip::fit(spec, mpg ~ ., mtcars)

  expect_equal(tidypredict_fit(model), mean(mtcars$mpg))

  spec <- parsnip::null_model(mode = "classification")
  model <- parsnip::fit(spec, Species ~ ., iris)

  probs <- vapply(tidypredict_fit(model), as.numeric, numeric(1))
  native <- predict(model, iris, type = "prob")

  expect_equal(unname(probs), unname(unlist(native[1, ])))
})
