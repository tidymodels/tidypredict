test_that("parse_model() errors for unsupported objects (#313)", {
  expect_snapshot(error = TRUE, parse_model(NULL))
  expect_snapshot(error = TRUE, parse_model(list()))
  expect_snapshot(error = TRUE, parse_model(1:10))
})

test_that("tidypredict_save() errors for unsupported objects (#313)", {
  skip_if_not_installed("yaml")

  expect_snapshot(error = TRUE, tidypredict_save(NULL, tempfile()))
})
