test_that("returns the right output", {
  skip_if_not_installed("mda")

  model <- mda::fda(Species ~ ., data = iris)

  tf <- tidypredict_fit(model)
  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))
  expect_true(all(vapply(tf, is.language, logical(1))))

  pm <- parse_model(model)
  expect_s3_class(pm, "list")
  expect_equal(length(pm), 3)
  expect_equal(pm$general$model, "fda")
  expect_equal(pm$general$version, 2)
  expect_equal(pm$classes, levels(iris$Species))

  lps <- lapply(pm$class_terms, build_linear_predictor)
  expect_snapshot(lapply(lps, round_print))
})

test_that("predictions match native predict", {
  skip_if_not_installed("mda")

  model <- mda::fda(Species ~ ., data = iris)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))
  native <- predict(model, iris, type = "posterior")

  expect_equal(unname(probs), unname(native))
  expect_equal(unname(rowSums(probs)), rep(1, nrow(iris)))
})

test_that("gen.ridge fits are handled", {
  skip_if_not_installed("mda")

  for (lambda in c(1e-20, 0.1, 10)) {
    model <- mda::fda(
      Species ~ .,
      data = iris,
      method = mda::gen.ridge,
      lambda = lambda
    )

    probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))

    expect_equal(
      unname(probs),
      unname(predict(model, iris, type = "posterior"))
    )
  }
})

test_that("categorical predictors are handled", {
  skip_if_not_installed("mda")

  df <- transform(mtcars, cyl = factor(cyl), gear = factor(gear))
  model <- mda::fda(cyl ~ mpg + gear + disp, data = df)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))

  expect_equal(
    unname(probs),
    unname(predict(model, df, type = "posterior"))
  )
})

test_that("binary outcomes are handled", {
  skip_if_not_installed("mda")

  df <- transform(mtcars, am = factor(am))
  model <- mda::fda(am ~ mpg + wt, data = df)

  tf <- tidypredict_fit(model)
  expect_named(tf, c("0", "1"))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, df))

  expect_equal(
    unname(probs),
    unname(predict(model, df, type = "posterior"))
  )
})

test_that("interactions and weights are handled", {
  skip_if_not_installed("mda")

  df <- transform(mtcars, cyl = factor(cyl))
  model <- mda::fda(cyl ~ mpg * wt, data = df, weights = rep(c(1, 2), 16))

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))

  expect_equal(
    unname(probs),
    unname(predict(model, df, type = "posterior"))
  )
})

test_that("a reduced dimension is handled", {
  skip_if_not_installed("mda")

  model <- mda::fda(Species ~ ., data = iris, dimension = 1)

  probs <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))

  expect_equal(
    unname(probs),
    unname(predict(model, iris, type = "posterior"))
  )
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("mda")
  skip_if_not_installed("yaml")

  model <- mda::fda(Species ~ ., data = iris)

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  pm <- as_parsed_model(yaml::read_yaml(mp))

  from_model <- sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, iris))
  from_pm <- sapply(tidypredict_fit(pm), \(f) rlang::eval_tidy(f, iris))

  expect_equal(from_model, from_pm, tolerance = 1e-6)
})

test_that("unsupported fda fits are rejected", {
  skip_if_not_installed("mda")

  poly_model <- mda::fda(Species ~ ., data = iris, degree = 2)
  expect_snapshot(error = TRUE, tidypredict_fit(poly_model))

  mars_model <- mda::fda(Species ~ ., data = iris, method = mda::mars)
  expect_snapshot(error = TRUE, tidypredict_fit(mars_model))

  mda_model <- mda::mda(Species ~ ., data = iris)
  expect_snapshot(error = TRUE, tidypredict_fit(mda_model))
  expect_snapshot(error = TRUE, parse_model(mda_model))
})

test_that("tidypredict_test errors for fda models", {
  skip_if_not_installed("mda")

  model <- mda::fda(Species ~ ., data = iris)

  expect_snapshot(error = TRUE, tidypredict_test(model, iris))
})

test_that("an ordered factor is rejected (#343)", {
  skip_if_not_installed("mda")
  # `fda()` records neither the contrasts nor the levels its factors had, so
  # unlike `lda()` and `qda()` the check is on the predictor being ordered
  # rather than on the names of the columns it expanded into.
  df <- transform(
    mtcars,
    cyl = factor(cyl),
    gear = factor(gear, ordered = TRUE)
  )

  expect_snapshot(
    tidypredict_fit(mda::fda(cyl ~ mpg + gear, data = df)),
    error = TRUE
  )
})

test_that("an ordered outcome is not mistaken for an ordered predictor (#343)", {
  skip_if_not_installed("mda")
  df <- transform(mtcars, cyl = factor(cyl, ordered = TRUE))
  model <- mda::fda(cyl ~ mpg + disp, data = df)

  expect_equal(
    unname(sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, df))),
    unname(predict(model, df, type = "posterior"))
  )
})

test_that("inline functions in the formula are rejected", {
  skip_if_not_installed("mda")

  model <- mda::fda(Species ~ log(Sepal.Width) + Petal.Width, data = iris)

  expect_snapshot(error = TRUE, tidypredict_fit(model))
})

test_that("SQL translation works", {
  skip_if_not_installed("mda")
  skip_if_not_installed("dbplyr")

  model <- mda::fda(Species ~ ., data = iris)

  sql <- tidypredict_sql(model, dbplyr::simulate_dbi())

  expect_named(sql, levels(iris$Species))
  expect_true(all(vapply(sql, \(x) inherits(x, "sql"), logical(1))))
})

test_that("fda is handled with parsnip", {
  skip_if_not_installed("mda")
  skip_if_not_installed("discrim")

  spec <- parsnip::discrim_linear(engine = "mda", penalty = 1)
  model <- parsnip::fit(spec, Species ~ ., iris)

  tf <- tidypredict_fit(model)

  expect_type(tf, "list")
  expect_named(tf, levels(iris$Species))

  probs <- sapply(tf, \(f) rlang::eval_tidy(f, iris))
  native <- as.matrix(predict(model, iris, type = "prob"))

  expect_equal(unname(probs), unname(native))
})

test_that("a coefficient label colliding with a variable name works (#376)", {
  skip_if_not_installed("mda")

  set.seed(1)
  df <- data.frame(
    g = factor(rep(c("x1", "y2", "z3"), length.out = 60)),
    gy2 = rnorm(60)
  )
  df$cls <- factor(ifelse(df$gy2 + as.numeric(df$g) > 2, "a", "b"))

  model <- mda::fda(cls ~ g + gy2, data = df)
  probs <- rlang::eval_tidy(tidypredict_fit(model)[["b"]], df)

  expect_equal(probs, unname(predict(model, df, type = "posterior")[, "b"]))
})
