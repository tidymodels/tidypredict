test_that("returns the right output", {
  model <- partykit::ctree(mpg ~ am + cyl, data = mtcars)
  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "party")
  expect_equal(pm$general$version, 2)

  expect_snapshot(
    rlang::expr_text(tf)
  )
})

test_that("Model can be saved and re-loaded", {
  model <- partykit::ctree(mpg ~ am + cyl, data = mtcars)

  pm <- parse_model(model)
  mp <- tempfile(fileext = ".yml")
  yaml::write_yaml(pm, mp)
  l <- yaml::read_yaml(mp)
  pm <- as_parsed_model(l)

  expect_identical(
    round_print(tidypredict_fit(model)),
    round_print(tidypredict_fit(pm))
  )
})

test_that("formulas produces correct predictions", {
  mtcars <- mtcars
  mtcars$am1 <- mtcars$am
  mtcars$am <- ifelse(mtcars$am == 1, "auto", "man")
  mtcars$am <- as.factor(mtcars$am)

  mtcars$cyl <- ifelse(mtcars$cyl == 4, "four", mtcars$cyl)
  mtcars$cyl <- ifelse(mtcars$cyl == 6, "six", mtcars$cyl)
  mtcars$cyl <- ifelse(mtcars$cyl == 8, "eight", mtcars$cyl)
  mtcars$cyl <- as.factor(mtcars$cyl)

  # normal
  expect_snapshot(
    tidypredict_test(
      partykit::ctree(mpg ~ am + cyl, data = mtcars),
      mtcars
    )
  )

  # offset
  expect_snapshot(
    tidypredict_test(
      partykit::ctree(mpg ~ wt, offset = am1, data = mtcars),
      mtcars
    )
  )

  # interaction
  expect_snapshot(
    tidypredict_test(
      partykit::ctree(mpg ~ wt + disp * cyl, data = mtcars),
      mtcars
    )
  )

  # interactions
  expect_snapshot(
    tidypredict_test(
      partykit::ctree(mpg ~ (wt + disp) * cyl, data = mtcars),
      mtcars
    )
  )
})
