#' Returns a SQL query with formula to calculate fitted values
#' 
#' 
#' @param model An R model or a tibble with a parsed model
#' @param con Database connection object. It is used to select 
#' the correst SQL translation syntax.
#'
#' @examples
#' 
#' model <- lm(mpg ~ wt + am + cyl, data = mtcars)
#' tidypredict_sql(model, dbplyr::simulate_dbi())
#' 
#' @export
tidypredict_sql <- function(model, con){
  f <- tidypredict_fit(model)
  dbplyr::translate_sql(!!f, con = con)
}

#' Returns a SQL query with formula to calculate predicted interval
#' 
#' 
#' @param model An R model or a tibble with a parsed model
#' @param con  Database connection object. It is used to select 
#' the correst SQL translation syntax.
#' @param interval The prediction interval, defaults to 0.95
#'
#' @examples
#' 
#' model <- lm(mpg ~ wt + am + cyl, data = mtcars)
#' tidypredict_sql_interval(model, dbplyr::simulate_dbi())
#' 
#' @export
tidypredict_sql_interval <- function(model, con, interval = 0.95){
  f <- tidypredict_interval(model, interval)
  dbplyr::translate_sql(!!f, con = con)
}