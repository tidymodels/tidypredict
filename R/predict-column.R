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
tidypredict_to_column <- function(df, model, add_interval = FALSE,
                                  interval = 0.95, vars = c("fit", "upper", "lower")) {
  fit_model <- tidypredict_fit(model)

  if (inherits(fit_model, "list")) stop("tidypredict_to_column does not support tree based models")

  fit <- vars[1]
  upper <- vars[2]
  lower <- vars[3]

  df <- mutate(df, !!fit := !!fit_model)

  if (add_interval) {
    formulas <- c(sym(fit), tidypredict_interval(model, interval = interval))
    upper_formula <- reduce(formulas, function(l, r) expr((!!l) + (!!r)))
    lower_formula <- reduce(formulas, function(l, r) expr((!!l) - (!!r)))

    df <- mutate(
      df,
      !!upper := !!upper_formula,
      !!lower := !!lower_formula
    )
  }

  df
}
