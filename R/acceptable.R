#' Checks that the formula can be parsed
#'
#' Uses an S3 method to check that a given formula can be parsed based on its class.
#' It currently scans for contrasts that are not supported and in-line functions.
#' (e.g: lm(wt ~ as.factor(am))). Since this function is meant for function interaction,
#' as opposed to human interaction, a successful check is silent.
#'
#' @param model An R model object
#'
#' @returns `NULL` (invisibly) when the model's formula can be parsed, or an
#'   error when it cannot.
#'
#' @examples
#'
#' model <- lm(mpg ~ wt, mtcars)
#' acceptable_formula(model)
#' @export
acceptable_formula <- function(model) {
  UseMethod("acceptable_formula")
}

# Mirrors `parse_model.default()`, so a class with no check reports the same
# thing as a class with no parser (#313).
#' @export
acceptable_formula.default <- function(model) {
  abort_model_unsupported(model)
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

# Abort unless every column a factor expanded into is named after one of its
# levels.
#
# That is what the treatment contrast does, and what the parsers rely on when
# they recover a level from a column name. `contr.poly`, which R gives an
# ordered factor by default, names the columns `.L`, `.Q` and `.C` instead, and
# `contr.sum` numbers them, so a level recovered from either matches no row.
#
# `acceptable_lm()` reads the contrasts off the model instead, which is more
# direct. This is for the models that do not record them.
acceptable_contrasts <- function(columns, vars, xlevels, terms = NULL) {
  invalid <- character(0)

  # `terms` says which variables each term multiplies and `xlevels` says what
  # levels they had, which is enough to decompose a column exactly. Splitting
  # the name on `:` instead cannot tell an interaction apart from a level whose
  # own name contains `:` (#391).
  decomposable <- column_decomposable(terms, xlevels)
  if (is.null(decomposable)) {
    columns <- unlist(strsplit(columns, ":", fixed = TRUE))
  }

  for (column in setdiff(columns, vars)) {
    if (!is.null(decomposable) && decomposable(column)) {
      next
    }
    # The longest matching variable wins, as it does in `parse_label_lm()`,
    # so that a column of `xy` is not read as a level of `x`.
    matches <- vars[startsWith(column, vars)]
    if (length(matches) == 0) {
      next
    }
    var <- matches[[which.max(nchar(matches))]]

    level <- substr(column, nchar(var) + 1, nchar(column))
    if (!level %in% xlevels[[var]]) {
      invalid <- c(invalid, var)
    }
  }

  if (length(invalid) > 0) {
    invalid <- unique(invalid)
    cli::cli_abort(
      "The treatment contrast is the only one supported at this time.
      Field(s) with an invalid contrast are: {.val {invalid}}."
    )
  }

  invisible()
}

# A predicate saying whether a model matrix column can be read as the expansion
# of one of the model's terms, or `NULL` when the model records too little to
# say.
#
# `match_label_fields()` is the same decomposition the parsers use, so a column
# it accepts is one they can take apart, whatever characters the level names
# happen to contain.
column_decomposable <- function(terms, xlevels) {
  if (is.null(terms)) {
    return(NULL)
  }
  term_labels <- attr(terms, "term.labels")
  classes <- attr(terms, "dataClasses")
  if (length(term_labels) == 0 || is.null(classes)) {
    return(NULL)
  }
  term_vars <- strsplit(term_labels, ":", fixed = TRUE)

  function(column) {
    any(map_lgl(term_vars, function(vars) {
      length(match_label_fields(column, vars, xlevels, classes)) > 0
    }))
  }
}

# Abort when a predictor is an ordered factor.
#
# A weaker check than `acceptable_contrasts()`, for a model that records
# neither its contrasts nor the levels its factors had, leaving nothing to
# compare the expanded column names against. R fits an ordered factor with
# `contr.poly` unless the global `contrasts` option says otherwise, so this
# catches the case that reaches the parser in practice, at the cost of missing
# a non-default contrast on an unordered factor.
acceptable_ordered <- function(model) {
  classes <- attr(model$terms, "dataClasses")
  response <- attr(model$terms, "response")
  if (length(response) == 1 && response > 0) {
    classes <- classes[-response]
  }

  ordered <- names(classes)[classes == "ordered"]
  if (length(ordered) > 0) {
    cli::cli_abort(
      "The treatment contrast is the only one supported at this time.
      Field(s) with an invalid contrast are: {.val {ordered}}."
    )
  }

  invisible()
}

acceptable_lm <- function(model) {
  # Check for invalid contrasts
  if (length(model$contrasts)) {
    # Every field is checked on its own. `"contr.treatment" %in% contrasts`
    # collapses to a single logical, so one treatment-coded field used to let
    # every other field through, whatever its contrast (#291).
    contr <- model$contrasts
    invalid <- !vapply(contr, identical, logical(1), "contr.treatment")
    if (any(invalid)) {
      invalid <- names(contr)[invalid]
      cli::cli_abort(
        "The treatment contrast is the only one supported at this time.
        Field(s) with an invalid contrast are: {.val {invalid}}."
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
  funs <- fun_calls(stats::formula(model))
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
    cli::cli_abort(msg)
  }
}
