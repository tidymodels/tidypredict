test_that("returns the right output", {
  model <- glm(am ~ wt + cyl, data = mtcars, family = "gaussian")

  #Don't have stable numbers at the tails across OS
  model$coefficients <- round(model$coefficients, 12)

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "glm")
  expect_equal(pm$general$version, 2)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("yaml")
  model <- glm(am ~ wt + cyl, data = mtcars, family = "gaussian")

  model$coefficients <- round(model$coefficients, 7)

  pm <- parse_model(model)
  mp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)

  expect_identical(
    tidypredict_fit(model),
    tidypredict_fit(pm)
  )
})

test_that("formulas produce correct predictions", {
  mtcars$cyl <- paste0("cyl", mtcars$cyl)
  # family = gaussian
  expect_false(
    tidypredict_test(
      glm(am ~ wt + cyl + disp, data = mtcars, family = "gaussian"),
      mtcars
    )$alert
  )
  # family = binomial
  expect_false(
    tidypredict_test(
      glm(am ~ wt + cyl + disp, data = mtcars, family = "binomial"),
      mtcars
    )$alert
  )
  # family = gaussian, with interactions
  expect_false(
    tidypredict_test(
      glm(am ~ wt * cyl + disp, data = mtcars, family = "gaussian"),
      mtcars
    )$alert
  )
  # family = binomial, with interactions. This fit separates the data, so glm
  # warns about fitted probabilities of 0 or 1; the agreement still holds.
  expect_false(
    suppressWarnings(
      tidypredict_test(
        glm(am ~ wt * cyl + disp, data = mtcars, family = "binomial"),
        mtcars
      )
    )$alert
  )
  # family = gaussian, with interactions
  expect_false(
    tidypredict_test(
      glm(am ~ wt:cyl + disp, data = mtcars, family = "gaussian"),
      mtcars
    )$alert
  )
  # family = binomial, with interactions
  expect_false(
    tidypredict_test(
      glm(am ~ wt:cyl + disp, data = mtcars, family = "binomial"),
      mtcars
    )$alert
  )
})

test_that("tidypredict works when variable names are subset of other variables", {
  mtcars$cyl <- paste0("cyl", mtcars$cyl)
  mtcars$wt_sq <- mtcars$wt^2
  mtcars$char_cyl <- as.character(mtcars$cyl)
  set.seed(22)
  mtcars$char_cyl_2 <- sample(letters[1:3], size = nrow(mtcars), replace = TRUE)

  model <- suppressWarnings(glm(
    am ~ wt + wt_sq + char_cyl + char_cyl_2,
    data = mtcars,
    family = "binomial"
  ))

  expect_false(
    tidypredict_test(
      model,
      mtcars
    )$alert
  )
})

test_that("an offset is applied", {
  set.seed(1)
  df <- data.frame(x = rnorm(60), off = runif(60))
  df$y <- rpois(60, exp(0.5 + df$x + df$off))

  model <- glm(y ~ x, data = df, family = poisson(), offset = off)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(predict(model, df, type = "response"))
  )
})

test_that("prior weights do not change the prediction formula", {
  set.seed(1)
  df <- data.frame(x = rnorm(60), z = rnorm(60))
  df$y <- as.integer(df$x + rnorm(60) > 0)
  df$w <- rep(c(1, 3), 30)

  model <- glm(y ~ x + z, data = df, family = binomial(), weights = w)

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), df),
    unname(predict(model, df, type = "response"))
  )
})

test_that("`NA` in newdata gives the same answer as predict()", {
  set.seed(1)
  df <- data.frame(x = rnorm(60), z = rnorm(60))
  df$y <- as.integer(df$x + rnorm(60) > 0)

  na_df <- df
  na_df$x[c(2, 5)] <- NA

  for (fam in list(gaussian(), binomial(), poisson())) {
    model <- suppressWarnings(glm(y ~ x + z, data = df, family = fam))
    expect_equal(
      rlang::eval_tidy(tidypredict_fit(model), na_df),
      unname(predict(model, na_df, type = "response"))
    )
  }
})

test_that("tidypredict_interval works for gaussian glm (#293)", {
  model <- glm(mpg ~ wt + cyl, data = mtcars, family = "gaussian")
  interval <- tidypredict_interval(model)
  expect_type(interval, "language")

  # a gaussian glm and the equivalent lm have the same prediction interval
  fit <- rlang::eval_tidy(tidypredict_fit(model), mtcars)
  half_width <- rlang::eval_tidy(interval, mtcars)
  reference <- predict(
    lm(mpg ~ wt + cyl, data = mtcars),
    mtcars,
    interval = "prediction"
  )

  expect_equal(fit - half_width, unname(reference[, "lwr"]))
  expect_equal(fit + half_width, unname(reference[, "upr"]))
})

test_that("tidypredict_to_column() adds intervals for a glm (#293)", {
  model <- glm(mpg ~ wt + cyl, data = mtcars, family = "gaussian")

  out <- tidypredict_to_column(mtcars, model, add_interval = TRUE)

  expect_equal(nrow(out), nrow(mtcars))
  expect_false(anyNA(out$lower))
  expect_false(anyNA(out$upper))
})

test_that("tidypredict_interval errors for non-gaussian glm", {
  model <- glm(am ~ wt + cyl, data = mtcars, family = "binomial")
  expect_snapshot(
    error = TRUE,
    tidypredict_interval(model)
  )
})
