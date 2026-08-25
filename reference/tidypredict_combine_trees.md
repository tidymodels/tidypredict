# Combine per-tree expressions into a model's prediction

[`tidypredict_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md)
returns one expression per tree. Turning those back into the model's
prediction is not simply summing or averaging them, and the rule differs
by backend:
[`mboost::blackboost()`](https://rdrr.io/pkg/mboost/man/blackboost.html)
needs an offset and a shrinkage factor, CatBoost needs a scale and a
bias, `aorsf` needs a guard that returns `NA` for an incomplete row, and
boosters then apply their objective's inverse link on top.

This generic holds that rule, so a caller that has split the trees apart
can put them back together without knowing which backend it is holding.

## Usage

``` r
tidypredict_combine_trees(x, trees, ...)
```

## Arguments

- x:

  A fitted model object.

- trees:

  A list of expressions, one per tree, in the order
  [`tidypredict_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md)
  returns them. Typically either that return value itself, or symbols
  naming the columns the individual trees were written to.

- ...:

  Additional arguments passed to methods.

## Value

A single language object.

## Details

The point of separating `trees` from this function is that a caller can
compute each tree into its own column, for a database to evaluate in
parallel, and then pass symbols referring to those columns rather than
the expressions themselves. The combination is the same either way.

Every ensemble satisfies
`tidypredict_combine_trees(x, tidypredict_trees(x))` computing the same
values as `tidypredict_fit(x)`, and that identity is what the tests for
these methods assert.

Not every ensemble has a method.
[`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html)
boosting combines its trees by a confidence-weighted vote that yields a
class label, so there is no arithmetic to apply to per-tree numbers and
no method is provided.

## Examples

``` r
model <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 3)

trees <- tidypredict_trees(model)
tidypredict_combine_trees(model, trees)
#> ifelse(is.na(cyl) | is.na(disp) | is.na(hp) | is.na(drat) | is.na(wt) | 
#>     is.na(qsec) | is.na(vs) | is.na(am) | is.na(gear) | is.na(carb), 
#>     NA_real_, (case_when(drat <= 3.75 ~ case_when(carb <= 3.5 ~ 
#>         case_when(wt <= 3.3125 ~ 24.4, .default = case_when(drat <= 
#>             3.115 ~ case_when(qsec <= 16.96 ~ 15.5, .default = case_when(wt <= 
#>             3.7875 ~ 17.62, .default = 19.2)), .default = 15.2)), 
#>         .default = case_when(drat <= 3.105 ~ 10.4, .default = case_when(wt <= 
#>             4.5925 ~ 13.7, .default = 14.7))), .default = case_when(wt <= 
#>         1.9875 ~ 32.15, .default = case_when(wt <= 2.47 ~ 23.8666666666667, 
#>         .default = case_when(vs <= 0.5 ~ 21, .default = 19.8)))) + 
#>         case_when(cyl <= 5 ~ case_when(wt <= 1.9875 ~ 31.275, 
#>             .default = case_when(gear <= 4.5 ~ case_when(qsec <= 
#>                 18.605 ~ 21.4, .default = 23.76), .default = 26)), 
#>             .default = case_when(hp <= 190 ~ case_when(disp <= 
#>                 163.8 ~ case_when(qsec <= 16.26 ~ 19.7, .default = 21), 
#>                 .default = case_when(cyl <= 7 ~ 18.3666666666667, 
#>                   .default = 16.5333333333333)), .default = case_when(drat <= 
#>                 3.07 ~ 10.4, .default = 14.425))) + case_when(drat <= 
#>         4 ~ case_when(cyl <= 7 ~ case_when(cyl <= 5 ~ 22.4666666666667, 
#>         .default = 19.925), .default = case_when(carb <= 3.5 ~ 
#>         case_when(hp <= 162.5 ~ 15.425, .default = case_when(hp <= 
#>             177.5 ~ 19.0333333333333, .default = 16.775)), .default = case_when(qsec <= 
#>         15.005 ~ 15, .default = 14.15))), .default = case_when(drat <= 
#>         4.255 ~ 31.125, .default = 28.2)))/3)

# Or referring to columns the trees were written to first.
tidypredict_combine_trees(model, rlang::syms(c("t1", "t2", "t3")))
#> ifelse(is.na(cyl) | is.na(disp) | is.na(hp) | is.na(drat) | is.na(wt) | 
#>     is.na(qsec) | is.na(vs) | is.na(am) | is.na(gear) | is.na(carb), 
#>     NA_real_, (t1 + t2 + t3)/3)
```
