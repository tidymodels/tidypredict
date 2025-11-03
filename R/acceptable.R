#' Checks that the formula can be parsed
#'
#' Uses an S3 method to check that a given formula can be parsed based on its class.
#' It currently scans for contrasts that are not supported and in-line functions.
#' (e.g: lm(wt ~ as.factor(am))). Since this function is meant for function interaction,
#' as opposed to human interaction, a successful check is silent.
#'
#' @param model An R model object
#'
#' @examples
#'
#' model <- lm(mpg ~ wt, mtcars)
#' acceptable_formula(model)
#' @export
acceptable_formula <- function(model) {
  UseMethod("acceptable_formula")
}

#' @export
acceptable_formula.lm <- function(model) {
  acceptable_lm(model)
}


#' @export
acceptable_formula.glm <- function(model) {
  acceptable_lm(model)
}

## As suggested by @topepo, brought in from the `pryr` package
## via the `recipes` package
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

acceptable_lm <- function(model) {
  # Check for invalid contrasts
  if (length(model$contrasts)) {
    contr <- model$contrasts
    contr <- contr[!("contr.treatment" %in% contr)]
    if (length(contr) > 0) {
      cli::cli_abort(
        "The treatment contrast is the only one supported at this time.
        Field(s) with an invalid contrast are: {.val {contr}}.",
        call. = FALSE
      )
    }
  }

  accepted_funs <- funs <- c(
    "~",
    "+",
    "-",
    "*",
    "(",
    ")",
    ":",
    "::",
    "lm",
    "glm",
    "factor",
    "stats"
  )

  # Check for in-line formulas
  funs <- fun_calls(model$call)
  funs <- funs[!(funs %in% accepted_funs)]
  if (length(funs) > 0) {
    contains_offset <- any(funs == "offset")
    contains_other <- funs[funs != "offset"]
    msg <- c(x = "Functions inside the formula are not supported.")
    if (contains_offset) {
      msg <- c(
        msg,
        i = "Offset detected, try using offset as an argument instead."
      )
    }
    if (length(contains_other) > 0) {
      msg <- c(
        msg,
        i = "Functions detected: {.val {contains_other}}. 
            Use `dplyr` transformations to prepare the data."
      )
    }
    cli::cli_abort(msg, call. = FALSE)
  }
}
