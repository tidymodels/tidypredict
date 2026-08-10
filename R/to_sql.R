#' Returns a SQL query with formula to calculate fitted values
#'
#' @param model An R model or a list with a parsed model
#' @param con Database connection object. It is used to select
#' the correct SQL translation syntax.
#'
#' @returns A SQL query, as returned by [dbplyr::translate_sql()]. Models that
#'   produce one formula per class or per outcome return a list of queries.
#'
#' @examplesIf rlang::is_installed("dbplyr")
#' model <- lm(mpg ~ wt + am + cyl, data = mtcars)
#' tidypredict_sql(model, dbplyr::simulate_dbi())
#' @export
tidypredict_sql <- function(model, con) {
  rlang::check_installed("dbplyr")
  translate_fit(tidypredict_fit(model), con)
}

#' Returns a SQL query with formula to calculate predicted interval
#'
#'
#' @param model An R model or a tibble with a parsed model
#' @param con  Database connection object. It is used to select
#' the correct SQL translation syntax.
#' @param interval The prediction interval, defaults to 0.95
#'
#' @returns A SQL query, as returned by [dbplyr::translate_sql()], giving the
#'   half width of the prediction interval.
#'
#' @examplesIf rlang::is_installed("dbplyr")
#' model <- lm(mpg ~ wt + am + cyl, data = mtcars)
#' tidypredict_sql_interval(model, dbplyr::simulate_dbi())
#' @export
tidypredict_sql_interval <- function(model, con, interval = 0.95) {
  rlang::check_installed("dbplyr")
  translate_fit(tidypredict_interval(model, interval), con)
}

# Multiclass and multivariate models return a list of expressions rather than a
# single one, so each element is translated separately.
translate_fit <- function(f, con) {
  if (inherits(f, "call")) {
    return(dbplyr::translate_sql(!!f, con = con))
  }
  map(f, ~ dbplyr::translate_sql(!!.x, con = con))
}
