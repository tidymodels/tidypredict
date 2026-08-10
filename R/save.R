# The `yaml` package formats numbers with a fixed number of significant
# digits, and its default (7) is far too few to round trip a double. A split
# threshold that changes in the 8th digit sends rows down the wrong branch, so
# the saved model quietly disagrees with the model it came from. 22 is the
# largest value `yaml` accepts, and is the smallest that was found to round
# trip every double in a 50,000 value sample exactly.
yaml_precision <- 22

#' Save and re-load a parsed model
#'
#' @description
#' `tidypredict_save()` writes a parsed model to a YAML file, and
#' `tidypredict_load()` reads one back. Together they persist a model's
#' prediction formula without needing the original model object, or the package
#' that fitted it, to be available later.
#'
#' Use these rather than calling [yaml::write_yaml()] directly. `yaml` defaults
#' to 7 significant digits, which is not enough to represent a split threshold
#' exactly: a re-loaded tree model can then send rows down a different branch
#' than the model it was saved from.
#'
#' @param x A fitted model, or a parsed model from [parse_model()]. Fitted
#'   models are parsed before being saved.
#' @param file Path to write the YAML file to, or read it from.
#'
#' @returns
#' `tidypredict_save()` returns `x`, invisibly, so it can be used in a pipe.
#' `tidypredict_load()` returns a parsed model object.
#'
#' @examples
#' model <- lm(mpg ~ wt + cyl, data = mtcars)
#'
#' path <- tempfile(fileext = ".yml")
#' tidypredict_save(model, path)
#'
#' loaded <- tidypredict_load(path)
#' tidypredict_fit(loaded)
#'
#' @export
tidypredict_save <- function(x, file) {
  rlang::check_installed("yaml")
  if (!inherits(x, "parsed_model")) {
    x <- parse_model(x)
  }
  yaml::write_yaml(x, file, precision = yaml_precision)
  invisible(x)
}

#' @rdname tidypredict_save
#' @export
tidypredict_load <- function(file) {
  rlang::check_installed("yaml")
  as_parsed_model(yaml::read_yaml(file))
}
