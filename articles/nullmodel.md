# nullmodel models

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✔ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✔ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

[`parsnip::nullmodel()`](https://parsnip.tidymodels.org/reference/nullmodel.html)
fits a model that ignores the predictors entirely. For regression it
predicts the mean of the outcome, and for classification it predicts the
observed class frequencies. Predictions are therefore constants, and
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
returns a plain number for regression and a *named list* of one constant
per class for classification. Since the classification output is a list,
[`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)
and
[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
are only supported for regression.

## `tidypredict_` functions

``` r

model <- parsnip::nullmodel(mtcars[-1], mtcars$mpg)
```

- Create the R formula

  ``` r

  tidypredict_fit(model)
  #> [1] 20.09062
  ```

- Add the predictions to the original table

  ``` r

  mtcars %>%
    tidypredict_to_column(model) %>%
    glimpse()
  #> Rows: 32
  #> Columns: 12
  #> $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19…
  #> $ cyl  <dbl> 6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4,…
  #> $ disp <dbl> 160.0, 160.0, 108.0, 258.0, 360.0, 225.0, 360.0, 146.7, …
  #> $ hp   <dbl> 110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123, 180,…
  #> $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.…
  #> $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, …
  #> $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20.00, …
  #> $ vs   <dbl> 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1,…
  #> $ am   <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,…
  #> $ gear <dbl> 4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4,…
  #> $ carb <dbl> 4, 4, 1, 1, 2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1, 2,…
  #> $ fit  <dbl> 20.09062, 20.09062, 20.09062, 20.09062, 20.09062, 20.090…
  ```

- Confirm that the results match the model’s
  [`predict()`](https://rdrr.io/r/stats/predict.html) results

  ``` r

  tidypredict_test(model, mtcars)
  #> tidypredict test results
  #> Difference threshold: 1e-12
  #> 
  #>  All results are within the difference threshold
  ```

For classification, one expression per class is returned:

``` r

c_model <- parsnip::nullmodel(iris[-5], iris$Species)

tidypredict_fit(c_model)
#> $setosa
#> [1] 0.3333333
#> 
#> $versicolor
#> [1] 0.3333333
#> 
#> $virginica
#> [1] 0.3333333
```

## parsnip

`parsnip` fitted models are also supported by `tidypredict`:

``` r

library(parsnip)

p_model <- null_model(mode = "regression") %>%
  set_engine("parsnip") %>%
  fit(mpg ~ ., data = mtcars)
```

``` r

tidypredict_fit(p_model)
#> [1] 20.09062
```

## Parse model spec

Here is an example of the model spec:

``` r

pm <- parse_model(model)
str(pm, 2)
#> List of 2
#>  $ general:List of 4
#>   ..$ model  : chr "nullmodel"
#>   ..$ version: num 2
#>   ..$ type   : chr "regression"
#>   ..$ is_glm : num 0
#>  $ terms  :List of 1
#>   ..$ :List of 4
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_regression" "list"
```
