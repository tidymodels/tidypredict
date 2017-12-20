## As suggested by @topepo, brought in from the `pryr` package
## via the `recepies` package
fun_calls <- function(f) {
  if (is.function(f)) {
    fun_calls(body(f))
  } else if (is.call(f)) {
    fname <- as.character(f[[1]])
    # Calls inside .Internal are special and shouldn't be included
    if (identical(fname, ".Internal"))
      return(fname)
    unique(c(fname, unlist(lapply(f[-1], fun_calls), use.names = FALSE)))
  }
}

#' @export
acceptable_formula <- function(model){
  UseMethod("acceptable_formula")
}


#' @export
acceptable_formula.default <- function(model){
  # Check for invalid contrasts
  if(length(model$contrasts)){
    contr <- model$contrasts
    contr <- contr[!("contr.treatment"  %in% model$contrasts)]
    if(length(contr >0)){
      stop(
        "The treatment contrast is the only one supported at this time. Field(s) with an invalid contrast are: ",
        paste0("`", names(contr), "`", collapse = ","),
        call. = FALSE)     
    }
  }
  
  # Check for in-line formulas
  funs <- fun_calls(model$call)
  funs <- funs[!(funs %in% c("~", "+", "-", "lm", "glm"))]
  if(length(funs) > 0){
    stop(
      "Functions inside the formula are not supported. Functions detected: ",
      paste0("`", funs, "`", collapse = ","), ". Use `dplyr` transformations to prepare the data.",
      call. = FALSE)
  }
}