# Extract the outcome scaling of a bart model

For use in orbital package. The trees of
[`.extract_bart_trees()`](https://tidypredict.tidymodels.org/reference/dot-extract_bart_trees.md)
predict on the scale that
[`dbarts::bart()`](https://rdrr.io/pkg/dbarts/man/bart.html) centers and
scales the outcome to. Their sum, divided by `n_draws` and multiplied by
`y_scale`, plus `y_center`, gives the fitted value.

## Usage

``` r
.extract_bart_scaling(model)
```

## Arguments

- model:

  A [`dbarts::bart()`](https://rdrr.io/pkg/dbarts/man/bart.html) model
  object
