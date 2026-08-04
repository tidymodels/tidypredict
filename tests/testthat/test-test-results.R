test_that("test_results_numeric keeps the sign of the difference", {
  res <- test_results_numeric(c(1, 2), c(1.5, 1.5), threshold = 1)

  expect_equal(res$raw_results$fit_diff, c(-0.5, 0.5))
  expect_false(res$alert)
})

test_that("test_results_numeric flags differences in both directions", {
  # The fitted value being too high is as much a failure as it being too low.
  low <- test_results_numeric(c(0, 1), c(0, 1.5), threshold = 0.1)
  high <- test_results_numeric(c(0, 1.5), c(0, 1), threshold = 0.1)

  expect_true(low$alert)
  expect_true(high$alert)
  expect_equal(low$raw_results$fit_threshold, c(FALSE, TRUE))
  expect_equal(high$raw_results$fit_threshold, c(FALSE, TRUE))
})

test_that("test_results_numeric reports an absolute maximum", {
  res <- test_results_numeric(c(0, 0), c(0, 5), threshold = 0.1)

  expect_match(res$message, "Max difference: 5")
  expect_no_match(res$message, "Max difference: -")
})

test_that("test_results_numeric builds the standard columns", {
  res <- test_results_numeric(c(1, 2), c(1, 2), threshold = 0)

  expect_named(
    res$raw_results,
    c("rowid", "fit", "fit_te", "fit_diff", "fit_threshold")
  )
  expect_equal(res$raw_results$rowid, 1:2)
})

test_that("test_results_numeric handles intervals", {
  res <- test_results_numeric(
    c(1, 2),
    c(1, 2),
    threshold = 0.1,
    intervals = list(
      lwr = c(0, 1),
      upr = c(2, 3),
      lwr_te = c(0, 1),
      upr_te = c(2, 9)
    )
  )

  expect_true(res$alert)
  expect_equal(res$raw_results$upr_threshold, c(FALSE, TRUE))
  expect_match(res$message, "Upper interval records above the threshold: 1")
  expect_match(res$message, "Upper max difference: 6")
})

test_that("test_results_class compares labels exactly", {
  res <- test_results_class(c("a", "b"), c("a", "c"))

  expect_true(res$alert)
  expect_equal(res$raw_results$fit_diff, c(0, 1))
  expect_match(res$message, "Fitted records that do not match: 1")
})

test_that("test_results_class passes when every label matches", {
  res <- test_results_class(factor(c("a", "b")), factor(c("a", "b")))

  expect_false(res$alert)
  expect_match(res$message, "within the difference threshold")
})

test_that("test_results_multiclass reports failures", {
  fit <- matrix(c(0.5, 0.5, 0.5, 0.5), ncol = 2)
  fit_te <- matrix(c(0.5, 0.9, 0.5, 0.1), ncol = 2)

  res <- test_results_multiclass(fit, fit_te, 0.1, classes = c("a", "b"))

  expect_true(res$alert)
  expect_named(
    res$raw_results,
    c(
      "rowid",
      "base_class_a",
      "te_class_a",
      "diff_class_a",
      "base_class_b",
      "te_class_b",
      "diff_class_b",
      "max_diff",
      "fit_threshold"
    )
  )
  expect_match(res$message, "multiclass: 2 classes")
  expect_match(res$message, "Fitted records above the threshold: 1")
})

test_that("maybe_head only trims when asked", {
  expect_equal(nrow(maybe_head(mtcars, NULL)), 32)
  expect_equal(nrow(maybe_head(mtcars, 5)), 5)
})
