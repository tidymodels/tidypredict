test_that("saving and loading round trips a parsed model", {
  model <- lm(mpg ~ wt + cyl, data = mtcars)
  path <- withr::local_tempfile(fileext = ".yml")

  expect_identical(tidypredict_save(model, path), parse_model(model))

  loaded <- tidypredict_load(path)
  expect_s3_class(loaded, "parsed_model")
  expect_identical(tidypredict_fit(loaded), tidypredict_fit(model))
})

test_that("an already parsed model can be saved", {
  parsed <- parse_model(lm(mpg ~ wt + cyl, data = mtcars))
  path <- withr::local_tempfile(fileext = ".yml")

  tidypredict_save(parsed, path)

  expect_identical(
    tidypredict_fit(tidypredict_load(path)),
    tidypredict_fit(parsed)
  )
})

test_that("thresholds survive the round trip exactly (#307)", {
  set.seed(100)
  n <- 200
  df <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  df$y <- rnorm(n) + 2 * df$x1 - df$x2

  models <- list(
    ctree = partykit::ctree(y ~ x1 + x2, data = df),
    rpart = rpart::rpart(y ~ x1 + x2, data = df),
    lm = lm(y ~ x1 + x2, data = df)
  )

  for (model in models) {
    path <- withr::local_tempfile(fileext = ".yml")
    tidypredict_save(model, path)

    expect_identical(
      rlang::eval_tidy(tidypredict_fit(tidypredict_load(path)), df),
      rlang::eval_tidy(tidypredict_fit(model), df)
    )
  }
})
