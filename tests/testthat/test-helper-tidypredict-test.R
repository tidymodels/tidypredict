make_test_result <- function(fit_diff, fit_threshold, alert) {
  structure(
    list(
      raw_results = data.frame(
        fit_diff = fit_diff,
        fit_threshold = fit_threshold
      ),
      alert = alert
    ),
    class = c("tidypredict_test", "list")
  )
}

test_that("expect_threshold_consistent passes on a consistent result", {
  res <- make_test_result(c(0, 1e-3), c(FALSE, TRUE), TRUE)

  expect_success(expect_threshold_consistent(res, 1e-7))
})

test_that("expect_threshold_consistent passes when nothing is flagged", {
  res <- make_test_result(c(0, 1e-12), c(FALSE, FALSE), FALSE)

  expect_success(expect_threshold_consistent(res, 1e-7))
})

test_that("expect_threshold_consistent catches a signed comparison", {
  res <- make_test_result(c(0, -1e-3), c(FALSE, FALSE), FALSE)

  expect_failure(expect_threshold_consistent(res, 1e-7))
})

test_that("expect_threshold_consistent catches a wrong alert", {
  res <- make_test_result(c(0, 1e-3), c(FALSE, TRUE), FALSE)

  expect_failure(expect_threshold_consistent(res, 1e-7))
})

test_that("expect_threshold_consistent reports informative failures", {
  expect_snapshot_failure(
    expect_threshold_consistent(
      make_test_result(c(0, -1e-3), c(FALSE, FALSE), FALSE),
      1e-7
    )
  )
  expect_snapshot_failure(
    expect_threshold_consistent(
      make_test_result(c(0, 1e-3), c(FALSE, TRUE), FALSE),
      1e-7
    )
  )
})
