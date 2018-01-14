#' Returns a SQL query with formula to calculate fitted values
#'
#' @param model An R model or a tibble with a parsed model
#' @param con Database connection object. It is used to select
#' the correct SQL translation syntax.
#'
#' @examples
#' library(dbplyr)
#'
#' model <- lm(mpg ~ wt + am + cyl, data = mtcars)
#' tidypredict_sql(model, simulate_dbi())
#'
#' @keywords internal
#' @export
tidypredict_sql <- function(model, con) {
  f <- tidypredict_fit(model)
  dbplyr::translate_sql(!! f, con = con)
}

#' Returns a SQL query with formula to calculate predicted interval
#'
#'
#' @param model An R model or a tibble with a parsed model
#' @param con  Database connection object. It is used to select
#' the correct SQL translation syntax.
#' @param interval The prediction interval, defaults to 0.95
#'
#' @examples
#' library(dbplyr)
#'
#' model <- lm(mpg ~ wt + am + cyl, data = mtcars)
#' tidypredict_sql_interval(model, simulate_dbi())
#'
#' @keywords internal
#' @export
tidypredict_sql_interval <- function(model, con, interval = 0.95) {
  f <- tidypredict_interval(model, interval)
  dbplyr::translate_sql(!! f, con = con)
}
