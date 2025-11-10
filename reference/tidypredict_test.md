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

  An R model or a list with a parsed model. It currently supports lm(),
  glm() and randomForest() models.

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

  A xgb.DMatrix object, required only for XGBoost models. It defaults to
  NULL recommended for large data sets.

## Examples

``` r
model <- lm(mpg ~ wt + cyl * disp, offset = am, data = mtcars)
tidypredict_test(model)
#> tidypredict test results
#> Difference threshold: 1e-12
#> 
#>  All results are within the difference threshold
```
