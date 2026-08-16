test_that("as_parsed_model() sets the dispatch class", {
  pm <- as_parsed_model(list(general = list(type = "regression")))

  expect_s3_class(pm, "parsed_model")
  expect_s3_class(pm, "pm_regression")
})

test_that("as_parsed_model() errors without a usable type (#313)", {
  expect_snapshot(error = TRUE, as_parsed_model(list()))
  expect_snapshot(error = TRUE, as_parsed_model(list(general = list())))
  expect_snapshot(
    error = TRUE,
    as_parsed_model(list(general = list(type = c("a", "b"))))
  )
})

test_that("as_parsed_model() errors for non-list input (#313)", {
  expect_snapshot(error = TRUE, as_parsed_model(NULL))
  expect_snapshot(error = TRUE, as_parsed_model("regression"))
})
