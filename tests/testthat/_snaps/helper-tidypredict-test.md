# expect_threshold_consistent reports informative failures

    Code
      expect_threshold_consistent(make_test_result(c(0, -0.001), c(FALSE, FALSE),
      FALSE), 1e-07)
    Condition
      Error:
      ! `make_test_result(c(0, -0.001), c(FALSE, FALSE), FALSE)` flags the wrong rows.
      1 of 2 rows disagree with `abs(fit_diff) > 1e-07`.

---

    Code
      expect_threshold_consistent(make_test_result(c(0, 0.001), c(FALSE, TRUE), FALSE),
      1e-07)
    Condition
      Error:
      ! `make_test_result(c(0, 0.001), c(FALSE, TRUE), FALSE)` has `alert = FALSE`, not `TRUE`.

