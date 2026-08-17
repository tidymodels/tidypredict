test_that("returns the right output", {
  skip_if_not_installed("quantreg")

  model <- quantreg::rq(mpg ~ wt + cyl, data = mtcars)

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "rq")
  expect_equal(pm$general$version, 2)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("formulas produce correct predictions", {
  skip_if_not_installed("quantreg")

  mtcars$cyl <- paste0("cyl", mtcars$cyl)
  # quantreg warns that the solution may be nonunique for this fit
  expect_false(
    suppressWarnings(
      tidypredict_test(
        quantreg::rq(mpg ~ wt + cyl + disp, data = mtcars),
        mtcars
      )
    )$alert
  )
})

test_that("works with non-default tau, method, and weights", {
  skip_if_not_installed("quantreg")

  mtcars$w <- rep(c(1, 2), length.out = nrow(mtcars))
  models <- list(
    quantreg::rq(mpg ~ wt + cyl, data = mtcars, tau = 0.9),
    quantreg::rq(mpg ~ wt + cyl, data = mtcars, method = "fn"),
    quantreg::rq(mpg ~ wt + cyl, data = mtcars, weights = w)
  )

  for (model in models) {
    expect_equal(
      rlang::eval_tidy(tidypredict_fit(model), mtcars),
      unname(predict(model, mtcars))
    )
  }
})

test_that("returns one expression per quantile for multiple quantiles", {
  skip_if_not_installed("quantreg")

  model <- quantreg::rq(mpg ~ wt + cyl, data = mtcars, tau = c(0.25, 0.5, 0.75))
  tf <- tidypredict_fit(model)

  expect_type(tf, "list")
  expect_named(tf, c("quantile_0.25", "quantile_0.50", "quantile_0.75"))
  expect_true(all(vapply(tf, is.language, logical(1))))

  expect_snapshot(
    lapply(tf, rlang::expr_text)
  )
})

test_that("predictions match predict()", {
  skip_if_not_installed("quantreg")
  model <- quantreg::rq(mpg ~ wt + cyl, data = mtcars)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), mtcars),
    unname(predict(model, mtcars))
  )
})

test_that("tidypredict_test() agrees with predict()", {
  skip_if_not_installed("quantreg")
  model <- quantreg::rq(mpg ~ wt + cyl, data = mtcars)

  expect_false(tidypredict_test(model, mtcars)$alert)
})

test_that("each quantile matches predict()", {
  skip_if_not_installed("quantreg")
  taus <- c(0.25, 0.5, 0.75)
  model <- quantreg::rq(mpg ~ wt + cyl, data = mtcars, tau = taus)

  fits <- tidypredict_fit(model)
  base <- predict(model, mtcars)

  for (i in seq_along(taus)) {
    expect_equal(
      rlang::eval_tidy(fits[[i]], mtcars),
      unname(base[, i]),
      tolerance = 1e-8
    )
  }
})

test_that("tidypredict_to_column() works", {
  skip_if_not_installed("quantreg")
  model <- quantreg::rq(mpg ~ wt + cyl, data = mtcars)

  res <- tidypredict_to_column(mtcars, model)
  expect_equal(res$fit, unname(predict(model, mtcars)))
})

test_that("SQL translation works", {
  skip_if_not_installed("quantreg")
  skip_if_not_installed("dbplyr")
  model <- quantreg::rq(mpg ~ wt + cyl, data = mtcars)

  expect_s3_class(tidypredict_sql(model, dbplyr::simulate_dbi()), "sql")
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("quantreg")
  skip_if_not_installed("yaml")
  model <- quantreg::rq(mpg ~ wt + cyl, data = mtcars)

  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(parse_model(model), tmp)
  pm <- as_parsed_model(yaml::read_yaml(tmp))

  # YAML stores fewer digits than a double carries, so the round-trip is lossy
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(pm), mtcars),
    unname(predict(model, mtcars)),
    tolerance = 1e-6
  )
})

test_that("factor predictors work", {
  skip_if_not_installed("quantreg")
  df <- mtcars
  df$cyl <- factor(df$cyl)
  model <- quantreg::rq(mpg ~ wt + cyl, data = df)

  # quantreg warns that the solution may be nonunique for this fit
  fit <- suppressWarnings(tidypredict_fit(model))

  expect_equal(
    rlang::eval_tidy(fit, df),
    unname(predict(model, df))
  )
})

rq_factor_df <- function(levels = letters[1:3], ordered = FALSE) {
  set.seed(1)
  df <- data.frame(x = rnorm(60), z = rnorm(60))
  g <- rep(c("a", "b", "c"), length.out = 60)
  df$g <- factor(g, levels = levels, ordered = ordered)
  df$y <- rnorm(60) + df$x + as.numeric(df$g)
  df
}

test_that("an unused factor level works", {
  skip_if_not_installed("quantreg")
  df <- rq_factor_df(levels = letters[1:4])
  model <- suppressWarnings(quantreg::rq(y ~ x + g, data = df))

  expect_equal(
    rlang::eval_tidy(suppressWarnings(tidypredict_fit(model)), df),
    unname(predict(model, df))
  )
})

test_that("an ordered factor is rejected", {
  skip_if_not_installed("quantreg")
  df <- rq_factor_df(ordered = TRUE)
  model <- suppressWarnings(quantreg::rq(y ~ x + g, data = df))

  expect_snapshot(error = TRUE, tidypredict_fit(model))
})

test_that("`NA` in newdata gives the same answer as predict()", {
  skip_if_not_installed("quantreg")
  df <- rq_factor_df()
  model <- suppressWarnings(quantreg::rq(y ~ x + g, data = df))

  na_df <- df
  na_df$x[c(2, 5)] <- NA
  na_df$g[c(1, 3)] <- NA

  expect_equal(
    rlang::eval_tidy(suppressWarnings(tidypredict_fit(model)), na_df),
    unname(predict(model, na_df))
  )
})

test_that("`NA` in the training data works", {
  skip_if_not_installed("quantreg")
  df <- rq_factor_df()
  train <- df
  train$x[1:5] <- NA
  model <- suppressWarnings(quantreg::rq(y ~ x + z, data = train))

  expect_equal(
    rlang::eval_tidy(suppressWarnings(tidypredict_fit(model)), df),
    unname(predict(model, df))
  )
})

test_that("degenerate fit shapes work", {
  skip_if_not_installed("quantreg")
  df <- rq_factor_df()

  # An intercept-only fit translates to a constant, so it evaluates to length 1
  intercept_only <- suppressWarnings(quantreg::rq(y ~ 1, data = df))
  expect_equal(
    rep(
      rlang::eval_tidy(suppressWarnings(tidypredict_fit(intercept_only)), df),
      nrow(df)
    ),
    unname(predict(intercept_only, df))
  )

  no_intercept <- suppressWarnings(quantreg::rq(y ~ x - 1, data = df))
  expect_equal(
    rlang::eval_tidy(suppressWarnings(tidypredict_fit(no_intercept)), df),
    unname(predict(no_intercept, df))
  )

  df$const <- 5
  constant <- suppressWarnings(quantreg::rq(const ~ x, data = df))
  expect_equal(
    rlang::eval_tidy(suppressWarnings(tidypredict_fit(constant)), df),
    unname(predict(constant, df))
  )
})
