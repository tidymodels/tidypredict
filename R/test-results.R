# Shared assembly for `tidypredict_test()` results.
#
# Every `tidypredict_test()` method does the same three things: obtain the
# model's own predictions, obtain tidypredict's, and compare them. Only the
# first two genuinely differ between models, so the comparison, the message and
# the returned object are built here.
#
# `raw_results` always has the same shape: `rowid`, `fit` (the model's own
# prediction), `fit_te` (tidypredict's), `fit_diff` and `fit_threshold`. The
# difference keeps its sign so the direction of the error is visible, and the
# threshold is applied to its absolute value.

new_tidypredict_test <- function(
  raw_results,
  message,
  alert,
  model_call = NULL
) {
  results <- list()
  results$model_call <- model_call
  results$raw_results <- raw_results
  results$message <- message
  results$alert <- alert
  structure(results, class = c("tidypredict_test", "list"))
}

# A comparison over no rows cannot tell us anything, but every count below is
# then zero and the test would report success. That is more likely to be a
# mistake by the caller than a meaningful pass.
check_test_rows <- function(n, call = rlang::caller_env()) {
  if (n == 0) {
    cli::cli_abort(
      c(
        "There is nothing to compare.",
        i = "The data passed to {.fn tidypredict_test} has no rows."
      ),
      call = call
    )
  }
  invisible(n)
}

# Which rows disagree, when either side may be missing.
#
# `NA` is a prediction like any other here: if a model returns `NA` for a row,
# tidypredict should too. So two `NA`s agree, one `NA` against a number is a
# mismatch, and the numeric comparison only applies where both sides are
# present. Without this, `abs(NA) > threshold` makes `alert` itself `NA` and
# the test errors rather than reporting the difference.
over_threshold <- function(fit, fit_te, threshold) {
  one_missing <- xor(is.na(fit), is.na(fit_te))
  diff <- abs(fit - fit_te)
  ifelse(one_missing, TRUE, !is.na(diff) & diff > threshold)
}

# The count of rows where exactly one side is missing, reported on its own so a
# missing value mismatch is not mistaken for a large numeric difference.
count_missing <- function(fit, fit_te) {
  sum(xor(is.na(fit), is.na(fit_te)))
}

# `max()` of nothing but `NA`s warns and returns `-Inf`, which would be
# reported as a difference. There is no largest difference when no two values
# can be compared, so say so.
safe_max <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

# Trim `df` when the caller asked for a subset of rows.
maybe_head <- function(df, max_rows) {
  if (is.numeric(max_rows)) {
    return(head(df, max_rows))
  }
  df
}

test_message_header <- function(threshold, extra = NULL) {
  paste0(
    "tidypredict test results",
    extra,
    "\n",
    "Difference threshold: ",
    threshold,
    "\n"
  )
}

test_message_pass <- function(header) {
  paste0(header, "\n All results are within the difference threshold")
}

# Compare numeric predictions.
#
# `intervals` is an optional list with `lwr`, `upr`, `lwr_te` and `upr_te`, and
# is only used by the `lm()`-style default method.
test_results_numeric <- function(
  fit,
  fit_te,
  threshold,
  model_call = NULL,
  intervals = NULL
) {
  raw_results <- data.frame(
    rowid = seq_along(fit),
    fit = as.vector(fit),
    fit_te = as.vector(fit_te),
    row.names = NULL
  )
  check_test_rows(nrow(raw_results))

  raw_results$fit_diff <- raw_results$fit - raw_results$fit_te
  raw_results$fit_threshold <- over_threshold(
    raw_results$fit,
    raw_results$fit_te,
    threshold
  )

  counts <- c(fit = sum(raw_results$fit_threshold))
  missing <- c(fit = count_missing(raw_results$fit, raw_results$fit_te))
  maxima <- c(fit = safe_max(abs(raw_results$fit_diff)))

  if (!is.null(intervals)) {
    raw_results$lwr <- as.vector(intervals$lwr)
    raw_results$upr <- as.vector(intervals$upr)
    raw_results$lwr_te <- as.vector(intervals$lwr_te)
    raw_results$upr_te <- as.vector(intervals$upr_te)
    raw_results$lwr_diff <- raw_results$lwr - raw_results$lwr_te
    raw_results$upr_diff <- raw_results$upr - raw_results$upr_te
    raw_results$lwr_threshold <- over_threshold(
      raw_results$lwr,
      raw_results$lwr_te,
      threshold
    )
    raw_results$upr_threshold <- over_threshold(
      raw_results$upr,
      raw_results$upr_te,
      threshold
    )

    counts["lwr"] <- sum(raw_results$lwr_threshold)
    counts["upr"] <- sum(raw_results$upr_threshold)
    missing["lwr"] <- count_missing(raw_results$lwr, raw_results$lwr_te)
    missing["upr"] <- count_missing(raw_results$upr, raw_results$upr_te)
    maxima["lwr"] <- safe_max(abs(raw_results$lwr_diff))
    maxima["upr"] <- safe_max(abs(raw_results$upr_diff))
  }

  alert <- any(counts > 0)
  header <- test_message_header(threshold)

  if (!alert) {
    return(new_tidypredict_test(
      raw_results,
      test_message_pass(header),
      alert,
      model_call
    ))
  }

  message <- paste0(
    header,
    "\nFitted records above the threshold: ",
    counts[["fit"]]
  )
  if (!is.null(intervals)) {
    message <- paste0(
      message,
      "\nLower interval records above the threshold: ",
      counts[["lwr"]],
      "\nUpper interval records above the threshold: ",
      counts[["upr"]]
    )
  }
  if (any(missing > 0)) {
    message <- paste0(
      message,
      "\nRecords missing from only one side: ",
      sum(missing)
    )
  }
  message <- paste0(message, "\n\nMax difference: ", maxima[["fit"]])
  if (!is.null(intervals)) {
    message <- paste0(
      message,
      "\nLower max difference: ",
      maxima[["lwr"]],
      "\nUpper max difference: ",
      maxima[["upr"]]
    )
  }

  new_tidypredict_test(raw_results, message, alert, model_call)
}

# Compare predicted class labels. A label either matches or it does not, so
# `fit_diff` is a 0/1 indicator and the threshold is always zero.
test_results_class <- function(
  fit,
  fit_te,
  model_call = NULL,
  extra_header = NULL
) {
  raw_results <- data.frame(
    rowid = seq_along(fit),
    fit = as.character(fit),
    fit_te = as.character(fit_te),
    row.names = NULL
  )
  check_test_rows(nrow(raw_results))

  # Two missing labels agree; one missing against a label does not.
  both_missing <- is.na(raw_results$fit) & is.na(raw_results$fit_te)
  differs <- raw_results$fit != raw_results$fit_te
  mismatch <- ifelse(both_missing, FALSE, is.na(differs) | differs)
  raw_results$fit_diff <- as.numeric(mismatch)
  raw_results$fit_threshold <- mismatch

  n_off <- sum(raw_results$fit_threshold)
  alert <- n_off > 0
  header <- test_message_header(0, extra_header)

  message <- if (alert) {
    paste0(header, "\nFitted records that do not match: ", n_off)
  } else {
    test_message_pass(header)
  }

  new_tidypredict_test(raw_results, message, alert, model_call)
}

# Compare a matrix of class probabilities, one column per class.
test_results_multiclass <- function(
  fit,
  fit_te,
  threshold,
  classes,
  model_call = NULL
) {
  check_test_rows(nrow(fit))

  diffs <- abs(fit - fit_te)

  raw_results <- data.frame(rowid = seq_len(nrow(fit)))
  for (i in seq_along(classes)) {
    raw_results[[paste0("base_class_", classes[[i]])]] <- fit[, i]
    raw_results[[paste0("te_class_", classes[[i]])]] <- fit_te[, i]
    raw_results[[paste0("diff_class_", classes[[i]])]] <- diffs[, i]
  }
  raw_results$max_diff <- apply(diffs, 1, safe_max)
  one_missing <- apply(xor(is.na(fit), is.na(fit_te)), 1, any)
  raw_results$fit_threshold <- ifelse(
    one_missing,
    TRUE,
    !is.na(raw_results$max_diff) & raw_results$max_diff > threshold
  )

  n_off <- sum(raw_results$fit_threshold)
  alert <- n_off > 0
  header <- test_message_header(
    threshold,
    paste0(" (multiclass: ", length(classes), " classes)")
  )

  message <- if (alert) {
    paste0(
      header,
      "\nFitted records above the threshold: ",
      n_off,
      "\n\nMax difference: ",
      safe_max(diffs)
    )
  } else {
    test_message_pass(header)
  }

  new_tidypredict_test(raw_results, message, alert, model_call)
}
