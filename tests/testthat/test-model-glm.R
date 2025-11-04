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

test_that("Model can be saved and re-loaded", {
  model <- glm(am ~ wt + cyl, data = mtcars, family = "gaussian")

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
  # family = gaussian
  expect_snapshot(
    tidypredict_test(
      glm(am ~ wt + cyl + disp, data = mtcars, family = "gaussian"),
      mtcars
    )
  )
  # family = binomial
  expect_snapshot(
    tidypredict_test(
      glm(am ~ wt + cyl + disp, data = mtcars, family = "binomial"),
      mtcars
    )
  )
  # family = gaussian, with interactions
  expect_snapshot(
    tidypredict_test(
      glm(am ~ wt * cyl + disp, data = mtcars, family = "gaussian"),
      mtcars
    )
  )
  # family = binomial, with interactions
  expect_snapshot(
    tidypredict_test(
      glm(am ~ wt * cyl + disp, data = mtcars, family = "binomial"),
      mtcars
    )
  )
  # family = gaussian, with interactions
  expect_snapshot(
    tidypredict_test(
      glm(am ~ wt:cyl + disp, data = mtcars, family = "gaussian"),
      mtcars
    )
  )
  # family = binomial, with interactions
  expect_snapshot(
    tidypredict_test(
      glm(am ~ wt:cyl + disp, data = mtcars, family = "binomial"),
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

  model <- suppressWarnings(glm(
    am ~ wt + wt_sq + char_cyl + char_cyl_2,
    data = mtcars,
    family = "binomial"
  ))

  expect_snapshot(
    tidypredict_test(
      model,
      mtcars
    )
  )
})
