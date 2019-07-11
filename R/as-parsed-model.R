#' Prepares parsed model object
#' 
#' @param x A parsed model object
#' 
#' @export
as_parsed_model <- function(x) {
  UseMethod("as_parsed_model")
}

#' @export
as_parsed_model.list <- function(x) {
  t <- paste0("pm_", x$general$type)
  class(x) <- c("parsed_model", t, class(x))
  x
}
