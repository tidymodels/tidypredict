# Describe what a model's fitted expressions compute

[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
returns expressions, but not what those expressions mean. A single
expression could be a numeric prediction, a probability, or an
uncalibrated decision value, and the three call for different handling
downstream. These generics answer that question, so that a package
generating code from the result does not have to keep its own list of
which backend produces which shape.

The metadata is asked of the *model*, not of the fitted expressions,
deliberately. Attributes on the result do not survive the subsetting,
[`lapply()`](https://rdrr.io/r/base/lapply.html) and
[`unlist()`](https://rdrr.io/r/base/unlist.html) that callers apply to a
multiclass result, which is the case that most needs describing.

## Usage

``` r
tidypredict_output_type(x, ...)

tidypredict_outcome_levels(x, ...)

tidypredict_normalized(x, ...)
```

## Arguments

- x:

  A fitted model object.

- ...:

  Additional arguments passed to methods.

## Value

`tidypredict_output_type()` returns a single string, one of:

- `"numeric"`:

  A numeric prediction.
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  returns one expression, or a named list of them for a multivariate
  outcome or for a quantile regression with several `tau`.

- `"prob"`:

  A probability. Either one expression giving the probability of the
  second outcome level, for a binary model, or a list with one
  expression per level.

- `"decision"`:

  An uncalibrated decision value whose *sign* selects the class. Not a
  probability, and not comparable to one: the cut is at 0, not 0.5.

- `"class"`:

  A hard class prediction, with no probability available. Usually the
  class label as a string, but `xgboost`'s `binary:hinge` objective
  gives a 0/1 indicator instead. What makes it `"class"` rather than
  `"numeric"` is that only the class values can occur, so using it as a
  numeric prediction is a mistake even when its type is numeric.

`tidypredict_outcome_levels()` returns a character vector of outcome
levels in model order, or `NULL`.

`NULL` means two different things, and `tidypredict_output_type()`
distinguishes them. For a `"numeric"` model it means there are no
levels. For a `"prob"` or `"class"` model it means the fitted model
**did not retain the outcome levels**, so any names on the result are
positional placeholders and the caller has to supply the real levels
from elsewhere. LightGBM and CatBoost multiclass models are in this
position: they store integer labels and their expressions come back
named `class_0`, `class_1` and so on.

`tidypredict_normalized()` returns `TRUE` if the per-level values
already sum to one across levels, `FALSE` if the caller has to normalize
them, and `NA` when there are no per-level values to sum, which includes
every single-expression model.

At present no backend returns `FALSE`: every multiclass probability list
goes through one shared softmax, so the values are always normalized
already. The generic exists so that a caller can rely on that rather
than having to know it, and so a future backend that does not normalize
can say so instead of silently breaking the assumption.

## Details

None of this is recoverable from the shape of the result, which is the
whole reason for recording it. Two concrete cases:

A binary `"prob"` model and a `"decision"` model both return exactly one
expression. `LiblineaR` produces either, depending only on its `type`
argument. Treating a decision value as a probability and cutting it at
0.5 gives silently wrong classes for every row whose value falls between
0 and 0.5.

A multiclass `"prob"` model and a
[`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) fit with
several `tau` both return a named list of expressions of the same length
and structure. In the first the values sum to one across the list; in
the second they are unrelated numeric predictions.

## See also

[tidypredict_extractors](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md)
for the generics that expose a model's per-tree and per-level
expressions, and
[`tidypredict_combine_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_combine_trees.md)
for recombining them.

## Examples

``` r
model <- lm(mpg ~ wt, data = mtcars)
tidypredict_output_type(model)
#> [1] "numeric"
tidypredict_outcome_levels(model)
#> NULL
tidypredict_normalized(model)
#> [1] NA
```
