mixomics_x <- function() as.matrix(mtcars[c("cyl", "disp", "hp", "drat")])

# The full set of components is used, which is what {plsmod} predicts with.
mixomics_native <- function(model, x) {
  unname(predict(model, x)$predict[,, model$ncomp])
}

pm_model <- function(model) parse_model(model)$general$model

# {plsmod} turns the predicted dummy outcomes into class probabilities with a
# softmax.
softmax_rows <- function(x) {
  t(apply(x, 1, \(row) exp(row) / sum(exp(row))))
}

test_that("returns the right output", {
  skip_if_not_installed("mixOmics")

  model <- mixOmics::pls(mixomics_x(), mtcars$mpg, ncomp = 2)

  tf <- tidypredict_fit(model)
  expect_type(tf, "language")

  pm <- parse_model(model)
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "mixo_pls")
  expect_equal(pm$general$version, 2)
  expect_equal(pm$general$type, "regression")

  expect_snapshot(round_print(tf))
})

test_that("predictions match native predict", {
  skip_if_not_installed("mixOmics")

  x <- mixomics_x()
  model <- mixOmics::pls(x, mtcars$mpg, ncomp = 2)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), mtcars),
    mixomics_native(model, x)
  )
  expect_snapshot(tidypredict_test(model, mtcars))
})

test_that("all values of ncomp are handled", {
  skip_if_not_installed("mixOmics")

  x <- mixomics_x()

  for (ncomp in 1:4) {
    model <- mixOmics::pls(x, mtcars$mpg, ncomp = ncomp)
    expect_equal(
      rlang::eval_tidy(tidypredict_fit(model), mtcars),
      mixomics_native(model, x)
    )
  }
})

test_that("scale = FALSE is handled", {
  skip_if_not_installed("mixOmics")

  x <- mixomics_x()
  model <- mixOmics::pls(x, mtcars$mpg, ncomp = 2, scale = FALSE)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), mtcars),
    mixomics_native(model, x)
  )
})

test_that("all modes are handled", {
  skip_if_not_installed("mixOmics")

  x <- mixomics_x()
  y <- as.matrix(mtcars[c("mpg", "qsec")])

  for (mode in c("regression", "canonical", "invariant", "classic")) {
    model <- mixOmics::pls(x, y, ncomp = 2, mode = mode)
    preds <- vapply(
      tidypredict_fit(model),
      \(f) rlang::eval_tidy(f, mtcars),
      numeric(nrow(mtcars))
    )
    expect_equal(unname(preds), mixomics_native(model, x))
  }
})

test_that("multivariate outcomes return one formula per response", {
  skip_if_not_installed("mixOmics")

  x <- mixomics_x()
  model <- mixOmics::pls(x, as.matrix(mtcars[c("mpg", "qsec")]), ncomp = 2)

  tf <- tidypredict_fit(model)
  expect_type(tf, "list")
  expect_named(tf, c("mpg", "qsec"))
  expect_true(all(vapply(tf, is.language, logical(1))))

  pm <- parse_model(model)
  expect_named(pm, c("mpg", "qsec"))
  expect_equal(pm$mpg$general$model, "mixo_pls")
})

test_that("spls models are handled", {
  skip_if_not_installed("mixOmics")

  x <- mixomics_x()
  model <- mixOmics::spls(x, mtcars$mpg, ncomp = 2, keepX = c(2, 2))

  expect_equal(pm_model(model), "mixo_spls")
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), mtcars),
    mixomics_native(model, x)
  )
})

test_that("plsda models return one probability per class", {
  skip_if_not_installed("mixOmics")

  x <- as.matrix(iris[1:4])
  model <- mixOmics::plsda(x, iris$Species, ncomp = 2)

  tf <- tidypredict_fit(model)
  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))
  expect_true(all(vapply(tf, is.language, logical(1))))

  pm <- parse_model(model)
  expect_equal(length(pm), 3)
  expect_equal(pm$general$model, "mixo_plsda")
  expect_equal(pm$general$type, "multiclass_regression")
  expect_equal(pm$classes, levels(iris$Species))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, iris))
  expect_equal(unname(rowSums(probs)), rep(1, nrow(iris)))
  expect_equal(unname(probs), softmax_rows(mixomics_native(model, x)))
})

test_that("splsda models are handled", {
  skip_if_not_installed("mixOmics")

  x <- as.matrix(iris[1:4])
  model <- mixOmics::splsda(x, iris$Species, ncomp = 2, keepX = c(2, 2))

  expect_equal(pm_model(model), "mixo_splsda")

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))
  expect_equal(unname(probs), softmax_rows(mixomics_native(model, x)))
})

test_that("binary outcomes are handled", {
  skip_if_not_installed("mixOmics")

  df <- transform(mtcars, am = factor(am))
  x <- as.matrix(mtcars[c("mpg", "wt")])
  model <- mixOmics::plsda(x, df$am, ncomp = 2)

  tf <- tidypredict_fit(model)
  expect_named(tf, c("0", "1"))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, df))
  expect_equal(unname(probs), softmax_rows(mixomics_native(model, x)))
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("mixOmics")
  skip_if_not_installed("yaml")

  model <- mixOmics::pls(mixomics_x(), mtcars$mpg, ncomp = 2)

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), mtcars),
    rlang::eval_tidy(tidypredict_fit(pm), mtcars),
    tolerance = 1e-6
  )
})

test_that("tidypredict_test errors for discriminant and multivariate models", {
  skip_if_not_installed("mixOmics")

  da <- mixOmics::plsda(as.matrix(iris[1:4]), iris$Species, ncomp = 2)
  expect_snapshot(error = TRUE, tidypredict_test(da, iris))

  mv <- mixOmics::pls(mixomics_x(), as.matrix(mtcars[c("mpg", "qsec")]), 2)
  expect_snapshot(error = TRUE, tidypredict_test(mv, mtcars))
})

test_that("SQL translation works", {
  skip_if_not_installed("mixOmics")
  skip_if_not_installed("dbplyr")

  model <- mixOmics::pls(mixomics_x(), mtcars$mpg, ncomp = 2)
  expect_s3_class(tidypredict_sql(model, dbplyr::simulate_dbi()), "sql")

  da <- mixOmics::plsda(as.matrix(iris[1:4]), iris$Species, ncomp = 2)
  sql <- tidypredict_sql(da, dbplyr::simulate_dbi())
  expect_named(sql, levels(iris$Species))
  expect_true(all(vapply(sql, \(x) inherits(x, "sql"), logical(1))))
})

test_that("mixOmics is handled with parsnip", {
  skip_if_not_installed("mixOmics")
  skip_if_not_installed("plsmod")

  spec <- parsnip::pls(num_comp = 2) |> parsnip::set_engine("mixOmics")

  reg <- parsnip::fit(
    parsnip::set_mode(spec, "regression"),
    mpg ~ disp + hp + drat,
    mtcars
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(reg), mtcars),
    predict(reg, mtcars)$.pred
  )

  cls <- parsnip::fit(
    parsnip::set_mode(spec, "classification"),
    Species ~ .,
    iris
  )
  probs <- sapply(tidypredict_fit(cls), \(f) rlang::eval_tidy(f, iris))
  expect_equal(
    unname(probs),
    unname(as.matrix(predict(cls, iris, type = "prob")))
  )
})

test_that("categorical predictors are handled with parsnip", {
  skip_if_not_installed("mixOmics")
  skip_if_not_installed("plsmod")

  df <- transform(mtcars, gear = factor(gear))
  spec <- parsnip::pls(num_comp = 2) |>
    parsnip::set_engine("mixOmics") |>
    parsnip::set_mode("regression")
  model <- parsnip::fit(spec, mpg ~ disp + hp + gear, df)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    predict(model, df)$.pred
  )
})
