test_that("returns the right output", {
  model <- lm(am ~ wt + cyl, data = mtcars)

  #Don't have stable numbers at the tails across OS
  model$coefficients <- round(model$coefficients, 12)

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "lm")
  expect_equal(pm$general$version, 2)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("Model can be saved and re-loaded", {
  model <- lm(am ~ wt + cyl, data = mtcars)

  model$coefficients <- round(model$coefficients, 7)

  pm <- parse_model(model)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)

  expect_identical(
    tidypredict_fit(model),
    tidypredict_fit(pm)
  )
})

test_that("formulas produces correct predictions", {
  mtcars$cyl <- paste0("cyl", mtcars$cyl)

  # normal
  expect_snapshot(
    tidypredict_test(
      lm(mpg ~ wt + am + cyl, data = mtcars),
      mtcars
    )
  )

  # offset
  expect_snapshot(
    tidypredict_test(
      lm(mpg ~ wt, offset = am, data = mtcars),
      mtcars
    )
  )

  # interaction
  expect_snapshot(
    tidypredict_test(
      lm(mpg ~ wt + disp * cyl, data = mtcars),
      mtcars
    )
  )

  # interaction
  expect_snapshot(
    tidypredict_test(
      lm(mpg ~ wt + disp:cyl, data = mtcars),
      mtcars
    )
  )

  # interactions
  expect_snapshot(
    tidypredict_test(
      lm(mpg ~ (wt + disp) * cyl, data = mtcars),
      mtcars
    )
  )
})

test_that("tidypredict works when variable names are subset of other variables", {
  mtcars$cyl <- paste0("cyl", mtcars$cyl)
  mtcars$wt_sq <- mtcars$wt^2
  mtcars$char_cyl <- as.character(mtcars$cyl)
  set.seed(22)
  mtcars$char_cyl_2 <- sample(letters[1:3], size = nrow(mtcars), replace = TRUE)

  model <- lm(
    am ~ wt + wt_sq + char_cyl + char_cyl_2,
    data = mtcars
  )

  expect_snapshot(
    tidypredict_test(
      model,
      mtcars
    )
  )
})

test_that("tidy() works", {
  expect_s3_class(
    tidy(parse_model(lm(mpg ~ ., mtcars))),
    "tbl_df"
  )
})

test_that("we get better error from QR decomposition issues (#124)", {
  mtcars$vs2 <- mtcars$disp - mtcars$vs

  lm_fit <- lm(mpg ~ ., mtcars)

  expect_snapshot(
    error = TRUE,
    tidypredict::tidypredict_fit(lm_fit)
  )
})

test_that("don't add with 0 (#147)", {
  model <- lm(am ~ wt + cyl, data = mtcars)

  model$coefficients <- setNames(c(0, 1.5, 2.2), names(model$coefficients))

  expect_identical(
    tidypredict_fit(model),
    quote((wt * 1.5) + (cyl * 2.2))
  )
})
