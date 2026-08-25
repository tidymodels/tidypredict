# Tests base predict function against tidypredict

Compares the results of predict() and tidypredict_to_column() functions.

## Usage

``` r
tidypredict_test(
  model,
  df = model$model,
  threshold = 1e-12,
  include_intervals = FALSE,
  max_rows = NULL,
  xg_df = NULL
)
```

## Arguments

- model:

  An R model or a list with a parsed model.

- df:

  A data frame that contains all of the needed fields to run the
  prediction. It defaults to the "model" data frame object inside the
  model object.

- threshold:

  The number that a given result difference, between predict() and
  tidypredict_to_column() should not exceed. For continuous predictions,
  the default value is 0.000000000001 (1e-12), and for categorical
  predictions, the default value is 0.

- include_intervals:

  Switch to indicate if the prediction intervals should be included in
  the test. It defaults to FALSE.

- max_rows:

  The number of rows in the object passed in the df argument. Highly
  recommended for large data sets.

- xg_df:

  The prediction matrix used to obtain the model's own predictions.
  Required for XGBoost, LightGBM and CatBoost models, which cannot
  predict from a data frame. Pass an `xgb.DMatrix` for XGBoost and a
  numeric matrix for LightGBM and CatBoost. It defaults to NULL.

## Value

A list of test results comparing
[`predict()`](https://rdrr.io/r/stats/predict.html) and
[`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md),
including the maximum difference and whether it stays within
`threshold`.

## Examples

``` r

model <- lm(mpg ~ wt + cyl * disp, offset = am, data = mtcars)
tidypredict_test(model)
#> tidypredict test results
#> Difference threshold: 1e-12
#> 
#>  All results are within the difference threshold
```
