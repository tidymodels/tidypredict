# Errors raised from more than one place.
#
# Each takes `call` so the message names the function the user actually called
# rather than the helper.

# Forests that vote cannot be written as one formula.
abort_classification_unsupported <- function(pkg, call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "Classification models are not supported for {pkg}.",
      i = "Only regression models can be converted to tidy formulas.",
      i = "Classification requires a voting mechanism that cannot be expressed as a single formula."
    ),
    call = call
  )
}

# `tidypredict_test()` compares a single column, so it has nothing to compare
# for models whose fit is a list of per-class expressions.
# `what` is interpolated rather than written inline, so its own cli markup has
# to be rendered first.
abort_test_unsupported <- function(
  what,
  detail = "multiclass predictions",
  call = rlang::caller_env()
) {
  cli::cli_abort(
    c(
      "{.fn tidypredict_test} does not support {cli::format_inline(what)}.",
      i = "Use {.fn tidypredict_fit} directly for {detail}."
    ),
    call = call
  )
}
