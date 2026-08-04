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
