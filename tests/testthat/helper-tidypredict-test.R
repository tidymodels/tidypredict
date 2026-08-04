# Invariant that every `tidypredict_test()` method must satisfy: a row is
# flagged exactly when its absolute difference exceeds the threshold, and
# `alert` is TRUE exactly when at least one row is flagged.
expect_threshold_consistent <- function(object, threshold) {
  act <- testthat::quasi_label(rlang::enquo(object))

  over <- abs(act$val$raw_results$fit_diff) > threshold
  flagged <- act$val$raw_results$fit_threshold

  if (!identical(flagged, over)) {
    testthat::fail(sprintf(
      "%s flags the wrong rows.\n%i of %i rows disagree with `abs(fit_diff) > %g`.",
      act$lab,
      sum(flagged != over),
      length(over),
      threshold
    ))
    return(invisible(act$val))
  }

  if (!identical(act$val$alert, any(over))) {
    testthat::fail(sprintf(
      "%s has `alert = %s`, not `%s`.",
      act$lab,
      act$val$alert,
      any(over)
    ))
    return(invisible(act$val))
  }

  testthat::pass()
  invisible(act$val)
}
