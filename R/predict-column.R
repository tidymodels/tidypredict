#' Adds the prediction columns to a piped command set.
#'
#' Adds a new column with the results from tidypredict_fit() to a piped command set.
#' If add_interval is set to TRUE, it will add two additional columns- one
#' for the lower and another for the upper prediction interval bounds.
#'
#' @param df A data.frame or tibble
#' @param model An R model or a parsed model inside a data frame
#' @param add_interval Switch that indicates if the prediction interval columns should be added. Defaults
#' to FALSE
#' @param interval The prediction interval, defaults to 0.95. Ignored if add_interval is set to
#' FALSE
#' @param vars The name of the variables that this function will produce. Defaults to "fit", "upper", and "lower".
#'
#' @export
tidypredict_to_column <- function(
  df,
  model,
  add_interval = FALSE,
  interval = 0.95,
  vars = c("fit", "upper", "lower")
) {
  if (!rlang::is_bool(add_interval)) {
    cli::cli_abort(
      "{.arg add_interval} must be {.code TRUE} or {.code FALSE}, not
       {.obj_type_friendly {add_interval}}."
    )
  }

  # `vars` is indexed positionally, so a shorter vector used to name a column
  # `NA` rather than error (#313).
  if (!is.character(vars) || anyNA(vars)) {
    cli::cli_abort(
      "{.arg vars} must be a character vector, not
       {.obj_type_friendly {vars}}."
    )
  }
  n_needed <- if (add_interval) 3 else 1
  if (length(vars) < n_needed) {
    cli::cli_abort(
      c(
        "{.arg vars} must name at least {n_needed} column{?s}, not
         {length(vars)}.",
        i = if (add_interval) {
          "The fit, upper and lower bound columns all need a name when
           {.arg add_interval} is {.code TRUE}."
        }
      )
    )
  }

  if (add_interval) {
    check_interval(interval)
  }

  fit_model <- tidypredict_fit(model)

  # Multiclass and multivariate models return one formula per outcome, and there
  # is no single column to put them in.
  if (inherits(fit_model, "list")) {
    cli::cli_abort(
      c(
        "{.fn tidypredict_to_column} does not support models that return more than one formula.",
        i = "Use {.fn tidypredict_fit} directly for these models."
      )
    )
  }

  fit <- vars[1]
  upper <- vars[2]
  lower <- vars[3]

  df <- mutate(df, !!fit := !!fit_model)

  if (add_interval) {
    formulas <- c(
      as.name(fit),
      tidypredict_interval(model, interval = interval)
    )
    upper_formula <- reduce_addition(formulas)
    lower_formula <- reduce_subtraction(formulas)

    df <- mutate(
      df,
      !!upper := !!upper_formula,
      !!lower := !!lower_formula
    )
  }

  df
}
