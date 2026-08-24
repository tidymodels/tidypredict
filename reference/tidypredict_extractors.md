# Extract model internals as expressions

These generics expose the pieces
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
is assembled from, rather than the finished formula. They exist so that
packages generating their own code from a fitted model, such as orbital,
can reuse tidypredict's parsing instead of reimplementing it per model
class.

Each generic has a single fixed return shape, described below. A model
class implements whichever generics make sense for it: a random forest
has trees and a tree count, a `glmnet` multinomial model has neither.

## Usage

``` r
tidypredict_trees(x, ...)

tidypredict_class_trees(x, ...)

tidypredict_class_exprs(x, ...)

tidypredict_n_trees(x, ...)
```

## Arguments

- x:

  A fitted model object.

- ...:

  Additional arguments passed to methods. `multnet` models accept
  `penalty`, which is required when the model was fitted with more than
  one value of lambda.

## Value

`tidypredict_trees()` returns an unnamed list with one element per tree,
each a language object.

`tidypredict_class_trees()` returns a list named by outcome level, in
model order. Each element is itself an unnamed list of per-tree language
objects for that level, so the result is `tidypredict_trees()` nested
one level deeper. What the leaves hold depends on the model:
`randomForest` gives 0/1 votes, `ranger` gives class probabilities.

`tidypredict_class_exprs()` returns a list named by outcome level, in
model order, with one language object per level. Unlike
`tidypredict_class_trees()` there is no per-tree structure and nothing
to combine: each expression computes that level's value on its own.

`tidypredict_n_trees()` returns a single integer, the number of trees in
the ensemble. For multiclass boosters this counts every tree, including
the per-class copies, so it is not the same as the number of boosting
rounds.

Wherever an expression is described above, a **bare numeric value** may
appear in its place when the model has nothing to branch on. Callers
must handle both. This happens for a single-leaf tree, a stump, and also
for a degenerate expression such as a `glmnet` class whose coefficients
are all zero. Note that the constant can appear alongside language
objects in the same result, so the element type is not uniform within
one list.

## Details

Two shapes that look similar are worth keeping apart.
`tidypredict_class_trees()` returns many trees per level that a caller
has to sum or average, and needs `tidypredict_n_trees()` to do it.
`tidypredict_class_exprs()` returns one finished expression per level.
Both are named by outcome level so that callers never have to assume the
order matches [`levels()`](https://rdrr.io/r/base/levels.html) of the
outcome.

## Examples

``` r
model <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 5)

tidypredict_n_trees(model)
#> [1] 5

trees <- tidypredict_trees(model)
length(trees)
#> [1] 5
trees[[1]]
#> case_when(drat <= 3.75 ~ case_when(carb <= 3.5 ~ case_when(wt <= 
#>     3.3125 ~ 24.4, .default = case_when(drat <= 3.115 ~ case_when(qsec <= 
#>     16.96 ~ 15.5, .default = case_when(wt <= 3.7875 ~ 17.62, 
#>     .default = 19.2)), .default = 15.2)), .default = case_when(drat <= 
#>     3.105 ~ 10.4, .default = case_when(wt <= 4.5925 ~ 13.7, .default = 14.7))), 
#>     .default = case_when(wt <= 1.9875 ~ 32.15, .default = case_when(wt <= 
#>         2.47 ~ 23.8666666666667, .default = case_when(vs <= 0.5 ~ 
#>         21, .default = 19.8))))
```
