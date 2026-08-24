# Deprecated in favour of the generics in extractors.R.
#
# These were exported and documented with `\keyword{internal}` for orbital's
# use. They are kept as thin wrappers for one cycle rather than removed
# outright, since being exported means an unknown caller is possible.
#
# `deprecate_soft()` rather than `deprecate_warn()`: orbital still calls these
# from its own namespace, and warning there would surface a message to orbital
# users about a function they did not call and cannot avoid. Soft deprecation
# still warns a direct caller, which is who can act on it.
#
# Two of them change return type as well as name, and cannot be made
# type-compatible: `.extract_earth_multiclass()` and
# `.extract_glmnet_multiclass()` returned deparsed strings, while
# `tidypredict_class_exprs()` returns language objects like every other
# extractor. The wrappers deparse the result to keep the old behaviour intact.

#' Deprecated model extractors
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions have been replaced by generics with documented return
#' shapes. See [tidypredict_extractors].
#'
#' | Deprecated | Replacement |
#' | --- | --- |
#' | `.extract_xgb_trees()`, `.extract_lgb_trees()`, `.extract_catboost_trees()`, `.extract_rf_trees()`, `.extract_ranger_trees()` | [tidypredict_trees()] |
#' | `.extract_rf_classprob()`, `.extract_ranger_classprob()` | [tidypredict_class_trees()] |
#' | `.extract_rpart_classprob()`, `.extract_partykit_classprob()`, `.extract_earth_multiclass()`, `.extract_glmnet_multiclass()` | [tidypredict_class_exprs()] |
#'
#' @param model A fitted model object.
#' @param penalty The penalty value to use for coefficient extraction.
#'
#' @returns The same values these functions have always returned. Note that
#'   [tidypredict_class_exprs()] returns language objects where
#'   `.extract_earth_multiclass()` and `.extract_glmnet_multiclass()` return
#'   strings.
#'
#' @keywords internal
#' @name deprecated-extractors
NULL

#' @rdname deprecated-extractors
#' @export
.extract_xgb_trees <- function(model) {
  lifecycle::deprecate_soft(
    "1.1.1",
    ".extract_xgb_trees()",
    "tidypredict_trees()"
  )
  check_extractor_class(model, "xgb.Booster")
  tidypredict_trees(model)
}

#' @rdname deprecated-extractors
#' @export
.extract_lgb_trees <- function(model) {
  lifecycle::deprecate_soft(
    "1.1.1",
    ".extract_lgb_trees()",
    "tidypredict_trees()"
  )
  check_extractor_class(model, "lgb.Booster")
  tidypredict_trees(model)
}

#' @rdname deprecated-extractors
#' @export
.extract_catboost_trees <- function(model) {
  lifecycle::deprecate_soft(
    "1.1.1",
    ".extract_catboost_trees()",
    "tidypredict_trees()"
  )
  check_extractor_class(model, "catboost.Model")
  tidypredict_trees(model)
}

#' @rdname deprecated-extractors
#' @export
.extract_rf_trees <- function(model) {
  lifecycle::deprecate_soft(
    "1.1.1",
    ".extract_rf_trees()",
    "tidypredict_trees()"
  )
  check_extractor_class(model, "randomForest")
  tidypredict_trees(model)
}

#' @rdname deprecated-extractors
#' @export
.extract_ranger_trees <- function(model) {
  lifecycle::deprecate_soft(
    "1.1.1",
    ".extract_ranger_trees()",
    "tidypredict_trees()"
  )
  check_extractor_class(model, "ranger")
  tidypredict_trees(model)
}

#' @rdname deprecated-extractors
#' @export
.extract_rf_classprob <- function(model) {
  lifecycle::deprecate_soft(
    "1.1.1",
    ".extract_rf_classprob()",
    "tidypredict_class_trees()"
  )
  check_extractor_class(model, "randomForest")
  tidypredict_class_trees(model)
}

#' @rdname deprecated-extractors
#' @export
.extract_ranger_classprob <- function(model) {
  lifecycle::deprecate_soft(
    "1.1.1",
    ".extract_ranger_classprob()",
    "tidypredict_class_trees()"
  )
  check_extractor_class(model, "ranger")
  tidypredict_class_trees(model)
}

#' @rdname deprecated-extractors
#' @export
.extract_rpart_classprob <- function(model) {
  lifecycle::deprecate_soft(
    "1.1.1",
    ".extract_rpart_classprob()",
    "tidypredict_class_exprs()"
  )
  check_extractor_class(model, "rpart")
  # This one was already named by outcome level, so it is passed through as is.
  tidypredict_class_exprs(model)
}

#' @rdname deprecated-extractors
#' @export
.extract_partykit_classprob <- function(model) {
  lifecycle::deprecate_soft(
    "1.1.1",
    ".extract_partykit_classprob()",
    "tidypredict_class_exprs()"
  )
  check_extractor_class(model, "party")
  # The generic gained outcome-level names; this returned an unnamed list.
  unname(tidypredict_class_exprs(model))
}

#' @rdname deprecated-extractors
#' @export
.extract_earth_multiclass <- function(model) {
  lifecycle::deprecate_soft(
    "1.1.1",
    ".extract_earth_multiclass()",
    "tidypredict_class_exprs()"
  )
  check_extractor_class(model, "earth")
  deparse_class_exprs(tidypredict_class_exprs(model))
}

#' @rdname deprecated-extractors
#' @export
.extract_glmnet_multiclass <- function(model, penalty = NULL) {
  lifecycle::deprecate_soft(
    "1.1.1",
    ".extract_glmnet_multiclass()",
    "tidypredict_class_exprs()"
  )
  check_extractor_class(model, "multnet")
  deparse_class_exprs(tidypredict_class_exprs(model, penalty = penalty))
}

# The old functions checked the class themselves and reported it against
# `model`. Dispatch would report it against `x` and name a different function,
# so the check is kept here to leave the deprecated messages unchanged.
check_extractor_class <- function(
  model,
  cls,
  call = rlang::caller_env()
) {
  if (!inherits(model, cls)) {
    cli::cli_abort(
      "{.arg model} must be {.cls {cls}}, not {.obj_type_friendly {model}}.",
      call = call
    )
  }

  invisible(model)
}

deparse_class_exprs <- function(x) {
  lapply(x, function(e) deparse1(e, control = "digits17"))
}
