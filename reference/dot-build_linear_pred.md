# Build linear predictor expression from coefficient names and values

Shared helper for building linear predictor strings from coefficients.
Used by orbital package for glmnet models.

## Usage

``` r
.build_linear_pred(coef_names, coef_values)
```

## Arguments

- coef_names:

  Character vector of coefficient names (including "(Intercept)")

- coef_values:

  Numeric vector of coefficient values
