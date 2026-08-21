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

# Raised for a model class, or a parsed model type, that no method knows how to
# handle.
#
# Carries the `tidypredict_unsupported_model` class so callers can tell "no
# method exists for this model at all" apart from the many other "not
# supported" errors, which report a specific unsupported *configuration* of an
# otherwise supported model. orbital needs that distinction to decide whether
# to fall back or to report the model as unsupported.
abort_model_unsupported <- function(model, call = rlang::caller_env()) {
  if (inherits(model, "parsed_model")) {
    cli::cli_abort(
      "Parsed models of type {.val {model$general$type}} are not supported.",
      class = "tidypredict_unsupported_model",
      call = call
    )
  }
  cli::cli_abort(
    "Models of class {.cls {class(model)[[1]]}} are not supported.",
    class = "tidypredict_unsupported_model",
    call = call
  )
}

# Every parsed model is a list with a `general` element, and every method that
# takes one reads from it. A list without one is not a parsed model, and used to
# reach code that indexed `NULL` instead of saying so.
check_parsed_model <- function(
  x,
  arg = "model",
  call = rlang::caller_env()
) {
  if (!is.list(x) || !is.list(x$general)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a fitted model or a parsed model.",
        i = "A parsed model is a list with a {.field general} element,
             as returned by {.fn parse_model}."
      ),
      call = call
    )
  }

  invisible(x)
}

# `interval` is used as a probability, so anything outside (0, 1) either makes
# `qt()` return `NaN`, which silently poisons the whole formula, or collapses
# the interval to zero.
check_interval <- function(
  interval,
  arg = rlang::caller_arg(interval),
  call = rlang::caller_env()
) {
  if (!is.numeric(interval) || length(interval) != 1 || is.na(interval)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single number between 0 and 1,
       not {.obj_type_friendly {interval}}.",
      call = call
    )
  }

  if (interval <= 0 || interval >= 1) {
    cli::cli_abort(
      "{.arg {arg}} must be a single number between 0 and 1,
       not {.val {interval}}.",
      call = call
    )
  }

  invisible(interval)
}
