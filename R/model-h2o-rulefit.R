# H2O RuleFit models (via agua's "h2o" engine for rule_fit()).
#
# A RuleFit model is a (lasso) linear model over two kinds of terms: rules
# extracted from a tree ensemble, and the original linear predictors. H2O
# exposes both through `h2o.rule_importance()`, so unlike the H2O GBM support
# nothing needs to be walked tree-by-tree. An active `h2o.init()` connection
# is still required to talk to the cluster.
#
# Regression and binary classification are supported. Multiclass models are
# not: see `tidypredict_fit_h2o_rulefit_multinomial()` below.

# Rule parsing -------------------------------------------------------

# H2O renders a rule as conditions joined by " & ", each parenthesized, e.g.
# "(hp < 118.0) & (gear in {4, 5} or gear is NA)". Missing values are only
# matched when the condition carries an explicit "or <var> is NA" clause.
parse_h2o_rule <- function(rule) {
  conditions <- strsplit(rule, " & ", fixed = TRUE)[[1]]
  conditions <- sub("^\\(", "", conditions)
  conditions <- sub("\\)$", "", conditions)
  reduce_and(map(conditions, parse_h2o_condition))
}

parse_h2o_condition <- function(condition) {
  na_match <- grepl(" or .+ is NA$", condition)
  if (na_match) {
    condition <- sub(" or .+ is NA$", "", condition)
  }

  parts <- regmatches(
    condition,
    regexec("^(.*) (<|<=|>|>=|==|in) (.*)$", condition)
  )[[1]]

  if (length(parts) != 4) {
    rlang::abort(paste0("Unable to parse H2O rule condition: ", condition))
  }

  col <- rlang::sym(parts[2])
  operator <- parts[3]
  value <- parts[4]

  if (operator == "in") {
    levels <- strsplit(gsub("^\\{|\\}$", "", value), ", ", fixed = TRUE)[[1]]
    out <- expr(!!col %in% !!levels)
  } else {
    out <- call(operator, col, as.numeric(value))
  }

  if (na_match) {
    # Explicit parentheses so the OR is not absorbed by the enclosing AND
    # when the expression is deparsed or translated to SQL.
    out <- call("(", expr_or(out, expr(is.na(!!col))))
  }

  out
}

# Term building ------------------------------------------------------

# Linear terms are named "linear.<column>" for numeric columns and
# "linear.<column>.<level>" for categorical ones. Column names may contain
# dots, so resolve them against the model's own column names.
h2o_linear_term <- function(variable, coefficient, model) {
  variable <- sub("^linear\\.", "", variable)
  columns <- model@model$names
  matches <- columns[
    variable == columns | startsWith(variable, paste0(columns, "."))
  ]

  if (length(matches) == 0) {
    rlang::abort(paste0(
      "Unable to match H2O linear term to a column: ",
      variable
    ))
  }
  column <- matches[which.max(nchar(matches))]
  col <- rlang::sym(column)

  if (identical(variable, column)) {
    return(expr_multiplication(coefficient, col))
  }

  level <- substr(variable, nchar(column) + 2, nchar(variable))
  expr(case_when(!!col == !!level ~ !!coefficient, .default = 0))
}

h2o_rule_term <- function(rule, coefficient) {
  expr(case_when(!!parse_h2o_rule(rule) ~ !!coefficient, .default = 0))
}

# Build the intercept plus one term per row of the rule importance table.
h2o_rulefit_linear_predictor <- function(model, importance, intercept) {
  terms <- map(
    seq_len(nrow(importance)),
    function(i) {
      variable <- importance$variable[i]
      coefficient <- importance$coefficient[i]
      if (startsWith(variable, "linear.")) {
        h2o_linear_term(variable, coefficient, model)
      } else {
        h2o_rule_term(importance$rule[i], coefficient)
      }
    }
  )

  if (length(terms) == 0) {
    return(intercept)
  }

  expr_addition(intercept, reduce_addition(terms))
}

h2o_rulefit_importance <- function(model) {
  as.data.frame(h2o::h2o.rule_importance(model))
}

# Fit model ----------------------------------------------------------

tidypredict_fit_h2o_rulefit_regression <- function(model) {
  h2o_rulefit_linear_predictor(
    model,
    h2o_rulefit_importance(model),
    model@model$intercept
  )
}

tidypredict_fit_h2o_rulefit_binomial <- function(model) {
  f <- h2o_rulefit_linear_predictor(
    model,
    h2o_rulefit_importance(model),
    model@model$intercept
  )
  # Probability of the second (positive) domain level: logistic link.
  expr_logistic(f)
}

# Multinomial RuleFit models cannot be reconstructed from the public H2O API:
# `h2o.rule_importance()` collapses rules that share a coefficient across
# classes and does not report every non-zero coefficient of the underlying
# GLM, so the per-class linear predictors are not recoverable.
tidypredict_fit_h2o_rulefit_multinomial <- function(model) {
  rlang::abort(c(
    "Multiclass H2O RuleFit models are not supported.",
    i = paste(
      "`h2o.rule_importance()` does not expose the per-class coefficients",
      "needed to reproduce the predictions."
    )
  ))
}
