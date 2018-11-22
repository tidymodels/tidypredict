#' Checks that the formula can be parsed
#'
#' Uses an S3 method to check that a given formula can be parsed based on its class.
#' It currently scans for contrasts that are not supported and in-line functions.
#' (e.g: lm(wt ~ as.factor(am))). Sine this function is meant for function interaction,
#' as opposed to human interaction, a successful check is silent.
#'
#' @param model An R model object
#'
#' @examples
#'
#' model <- lm(mpg ~ wt, mtcars)
#' acceptable_formula(model)
#'
#' @export
acceptable_formula <- function(model) {
  UseMethod("acceptable_formula")
}


## As suggested by @topepo, brought in from the `pryr` package
## via the `recepies` package
fun_calls <- function(f) {
  if (is.function(f)) {
    fun_calls(body(f))
  } else if (is.call(f)) {
    fname <- as.character(f[[1]])
    # Calls inside .Internal are special and shouldn't be included
    if (identical(fname, ".Internal")) {
      return(fname)
    }
    unique(c(fname, unlist(lapply(f[-1], fun_calls), use.names = FALSE)))
  }
}
