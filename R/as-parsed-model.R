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
  # `general$type` is what the `pm_*` dispatch class is built from, so without
  # it the object gets a class of `pm_` that no method matches, and the failure
  # surfaces much later (#313).
  type <- x$general$type
  if (!rlang::is_string(type) || type == "") {
    cli::cli_abort(
      c(
        "{.arg x} is not a valid parsed model.",
        i = "{.code x$general$type} must be a single string, not
             {.obj_type_friendly {type}}."
      )
    )
  }

  class(x) <- c("parsed_model", paste0("pm_", type), class(x))
  x
}

#' @export
as_parsed_model.default <- function(x) {
  cli::cli_abort(
    "{.arg x} must be a parsed model, not {.obj_type_friendly {x}}."
  )
}

# Models whose fit is one linear predictor per class, combined with a softmax.
new_multiclass_parsed_model <- function(
  model,
  classes,
  class_terms,
  version = 2
) {
  pm <- list()
  pm$general$model <- model
  pm$general$version <- version
  pm$general$type <- "multiclass_regression"
  pm$general$family <- "multinomial"
  pm$classes <- classes
  pm$class_terms <- class_terms

  as_parsed_model(pm)
}
