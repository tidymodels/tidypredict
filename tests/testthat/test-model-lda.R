test_that("returns the right output", {
  skip_if_not_installed("MASS")

  model <- MASS::lda(Species ~ ., data = iris)

  tf <- tidypredict_fit(model)
  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))
  expect_true(all(vapply(tf, is.language, logical(1))))

  pm <- parse_model(model)
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 3)
  expect_equal(pm$general$model, "lda")
  expect_equal(pm$general$version, 2)
  expect_equal(pm$classes, levels(iris$Species))

  lps <- lapply(pm$class_terms, build_linear_predictor)
  expect_snapshot(lapply(lps, round_print))
})

test_that("predictions match native predict", {
  skip_if_not_installed("MASS")

  model <- MASS::lda(Species ~ ., data = iris)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))
  native <- predict(model, iris)$posterior

  expect_equal(unname(probs), unname(native))
  expect_equal(unname(rowSums(probs)), rep(1, nrow(iris)))
})

test_that("categorical predictors are handled", {
  skip_if_not_installed("MASS")

  df <- transform(mtcars, cyl = factor(cyl), gear = factor(gear))
  model <- MASS::lda(cyl ~ mpg + gear + disp, data = df)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))

  expect_equal(unname(probs), unname(predict(model, df)$posterior))
})

test_that("binary outcomes are handled", {
  skip_if_not_installed("MASS")

  df <- transform(mtcars, am = factor(am))
  model <- MASS::lda(am ~ mpg + wt, data = df)

  tf <- tidypredict_fit(model)
  expect_named(tf, c("0", "1"))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, df))

  expect_equal(unname(probs), unname(predict(model, df)$posterior))
})

test_that("interactions are handled", {
  skip_if_not_installed("MASS")

  df <- transform(mtcars, cyl = factor(cyl))
  model <- MASS::lda(cyl ~ mpg * wt, data = df)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))

  expect_equal(unname(probs), unname(predict(model, df)$posterior))
})

test_that("non-default prior is handled", {
  skip_if_not_installed("MASS")

  model <- MASS::lda(Species ~ ., data = iris, prior = c(0.2, 0.3, 0.5))

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))

  expect_equal(unname(probs), unname(predict(model, iris)$posterior))
})

test_that("estimation methods are handled", {
  skip_if_not_installed("MASS")

  for (method in c("moment", "mle")) {
    model <- MASS::lda(Species ~ ., data = iris, method = method)

    probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))

    expect_equal(unname(probs), unname(predict(model, iris)$posterior))
  }
})

test_that("subset and nu are handled", {
  skip_if_not_installed("MASS")

  model <- MASS::lda(
    Species ~ .,
    data = iris,
    subset = seq(1, 150, by = 2),
    method = "t",
    nu = 10
  )

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))

  expect_equal(unname(probs), unname(predict(model, iris)$posterior))
})

test_that("the x/grouping interface is handled", {
  skip_if_not_installed("MASS")

  model <- MASS::lda(as.matrix(iris[1:4]), iris$Species)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))

  expect_equal(
    unname(probs),
    unname(predict(model, as.matrix(iris[1:4]))$posterior)
  )
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("yaml")

  model <- MASS::lda(Species ~ ., data = iris)

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  from_model <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))
  from_pm <- sapply(tidypredict_fit(pm), \(f) rlang::eval_tidy(f, iris))

  expect_equal(from_model, from_pm, tolerance = 1e-6)
})

test_that("tidypredict_test errors for lda models", {
  skip_if_not_installed("MASS")

  model <- MASS::lda(Species ~ ., data = iris)

  expect_snapshot(error = TRUE, tidypredict_test(model, iris))
})

test_that("a row far outside the training range matches predict() (#299)", {
  skip_if_not_installed("MASS")
  # The class scores of such a row are large enough that `exp()` overflows,
  # which used to make every probability `NaN`.
  model <- MASS::lda(Species ~ ., data = iris)

  far <- iris[rep(1, 3), ]
  far$Sepal.Length <- c(100, 1e3, -500)
  far$Sepal.Width <- c(100, -1e3, 500)
  far$Petal.Length <- c(100, 1e3, -500)
  far$Petal.Width <- c(100, -1e3, 500)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, far))

  expect_false(anyNA(probs))
  expect_equal(unname(probs), unname(predict(model, far)$posterior))
})

test_that("an unused outcome level is handled (#302)", {
  skip_if_not_installed("MASS")

  df <- iris
  df$Species <- factor(df$Species, levels = c(levels(df$Species), "unused"))
  model <- suppressWarnings(MASS::lda(Species ~ ., data = df))

  tf <- tidypredict_fit(model)
  expect_named(tf, levels(iris$Species))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, df))

  expect_equal(unname(probs), unname(predict(model, df)$posterior))
})

test_that("an ordered factor is rejected (#343)", {
  skip_if_not_installed("MASS")
  # R fits an ordered factor with `contr.poly`, whose columns are named `.L`,
  # `.Q` and `.C` rather than after the levels. `lda()` records no contrasts,
  # so the level recovered from those names used to reach the formula, where it
  # matched no row and the term was silently dropped.
  df <- transform(
    mtcars,
    cyl = factor(cyl),
    gear = factor(gear, ordered = TRUE)
  )

  expect_snapshot(
    tidypredict_fit(MASS::lda(cyl ~ mpg + gear + disp, data = df)),
    error = TRUE
  )
})

test_that("a global non-treatment contrast is rejected (#343)", {
  skip_if_not_installed("MASS")
  withr::local_options(contrasts = c("contr.sum", "contr.poly"))
  df <- transform(mtcars, cyl = factor(cyl), gear = factor(gear))

  expect_snapshot(
    tidypredict_fit(MASS::lda(cyl ~ mpg + gear + disp, data = df)),
    error = TRUE
  )
})

test_that("inline functions in the formula are rejected", {
  skip_if_not_installed("MASS")

  model <- MASS::lda(Species ~ log(Sepal.Width), data = iris)

  expect_snapshot(error = TRUE, tidypredict_fit(model))
})

test_that("SQL translation works", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("dbplyr")

  model <- MASS::lda(Species ~ ., data = iris)

  sql <- tidypredict_sql(model, dbplyr::simulate_dbi())

  expect_named(sql, levels(iris$Species))
  expect_true(all(vapply(sql, \(x) inherits(x, "sql"), logical(1))))
})

test_that("lda is handled with parsnip", {
  skip_if_not_installed("MASS")
  skip_if_not_installed("discrim")

  spec <- parsnip::discrim_linear(engine = "MASS")
  model <- parsnip::fit(spec, Species ~ ., iris)

  tf <- tidypredict_fit(model)

  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, iris))
  native <- as.matrix(predict(model, iris, type = "prob"))

  expect_equal(unname(probs), unname(native))
})
