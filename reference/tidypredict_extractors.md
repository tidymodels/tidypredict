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

## Which models implement which generic

`.` means the generic is not implemented for that class and will error.

                     trees  class_trees  class_exprs  n_trees  combine
    blackboost         x         .            .          x        x
    C5.0               .         .            .          .        x
    catboost.Model     x         .            .          x        x
    cforest            x         .            .          x        x
    earth              .         .            x          .        .
    lgb.Booster        x         .            .          x        x
    multnet            .         .            x          .        .
    ObliqueForest      x         .            .          x        x
    party              .         .            x          .        .
    randomForest       x         x            .          x        x
    ranger             x         x            .          x        x
    rpart              .         .            x          .        .
    xgb.Booster        x         .            .          x        x

[`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html) is
the one row with a
[`tidypredict_combine_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_combine_trees.md)
method and no `tidypredict_trees()`. That method exists only to refuse,
with an explanation, rather than to let the caller reach the `.default`
error and guess why.

## Implementing these for a new model class

The table above shows the grouping to follow. `tidypredict_trees()`,
`tidypredict_n_trees()` and
[`tidypredict_combine_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_combine_trees.md)
are a set: implement all three or none. Per-tree expressions are not
usable without a count to size them and a rule to recombine them, and
shipping the first without the third invites a caller to sum the trees,
which is wrong for every backend that carries an offset, a scale or a
link.

A useful check on a new method is that
`tidypredict_combine_trees(x, tidypredict_trees(x))` computes the same
values as `tidypredict_fit(x)`. That identity is what the tests for the
existing methods assert, and it catches a combination rule that was
assumed rather than read out of the model.

If a model's trees genuinely cannot be recombined arithmetically, give
it a
[`tidypredict_combine_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_combine_trees.md)
method that refuses and no `tidypredict_trees()` method, as
[`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html) does.
Splitting trees apart that cannot be put back together only enables a
wrong answer.

## See also

[`tidypredict_combine_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_combine_trees.md)
for turning per-tree expressions back into a prediction, and
[tidypredict_metadata](https://tidypredict.tidymodels.org/reference/tidypredict_metadata.md)
for what the resulting values mean.

## Examples

``` r
model <- randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 5)

tidypredict_n_trees(model)
#> [1] 5

trees <- tidypredict_trees(model)
length(trees)
#> [1] 5
trees[[1]]
#> case_when(wt <= 2.3325 ~ case_when(hp <= 65.5 ~ 33.9, .default = case_when(disp <= 
#>     78.85 ~ 32.4, .default = 29.78)), .default = case_when(cyl <= 
#>     7 ~ case_when(hp <= 116.5 ~ case_when(gear <= 3.5 ~ 19.8, 
#>     .default = case_when(drat <= 4.005 ~ 21, .default = 21.4)), 
#>     .default = 18.9), .default = case_when(carb <= 3.5 ~ 17.26, 
#>     .default = case_when(hp <= 217.5 ~ 10.4, .default = 14.96))))
```
