context("earth/MARS models")

test_that("Parsed model returns", {
  expect_silent(
    parse_model(earth::earth(wt ~ mpg, data = mtcars))
  )
})

test_that("Formula returns", {
  expect_silent(
    te_earth_fit(
      parse_model(earth::earth(wt ~ mpg, data = mtcars))
    )
  )
})
