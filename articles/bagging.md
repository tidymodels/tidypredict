# bagger models

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✔ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✔ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

[`baguette::bagger()`](https://baguette.tidymodels.org/reference/bagger.html)
fits an ensemble of models on bootstrap samples of the training data.
Only the `"CART"` base model, which fits
[`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html) trees, is
supported.
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
returns one nested
[`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
per tree, so the size of the returned expression grows with `times`.

For regression models the fitted value is the mean of the individual
tree predictions. For classification models the class probabilities of
each tree are averaged, and the returned expression is the class with
the largest average probability.

## `tidypredict_` functions

``` r

set.seed(100)
model <- baguette::bagger(mpg ~ wt + cyl + disp, data = mtcars, times = 5)
#> Registered S3 method overwritten by 'butcher':
#>   method                 from    
#>   as.character.dev_topic generics
```

- Create the R formula

  ``` r

  tidypredict_fit(model)
  #> (case_when(wt < 2.975 ~ 23.0583333333333, .default = case_when(wt < 
  #>     3.545 ~ 16.9125, .default = 15.125)) + case_when(wt < 3.16 ~ 
  #>     24.3642857142857, .default = 15.7444444444444) + case_when(wt < 
  #>     2.26 ~ 30.0857142857143, .default = case_when(cyl < 7 ~ 20.4428571428571, 
  #>     .default = 14.6454545454545)) + case_when(disp < 163.8 ~ 
  #>     25.6733333333333, .default = 14.6764705882353) + case_when(wt < 
  #>     2.41 ~ 29.8, .default = case_when(disp < 266.9 ~ 21.325, 
  #>     .default = 14.6916666666667)))/5L
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
  #> $ fit  <dbl> 22.97276, 22.97276, 24.66776, 17.82025, 15.33411, 17.820…
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

- Get the SQL translation

  ``` r

  tidypredict_sql(model, dbplyr::simulate_mssql())
  #> <SQL> ((((CASE
  #> WHEN ([wt] < 2.975) THEN 23.0583333333333
  #> ELSE CASE WHEN ([wt] < 3.545) THEN 16.9125 ELSE 15.125 END
  #> END + CASE WHEN ([wt] < 3.16) THEN 24.3642857142857 ELSE 15.7444444444444 END) + CASE
  #> WHEN ([wt] < 2.26) THEN 30.0857142857143
  #> ELSE CASE WHEN ([cyl] < 7.0) THEN 20.4428571428571 ELSE 14.6454545454545 END
  #> END) + CASE WHEN ([disp] < 163.8) THEN 25.6733333333333 ELSE 14.6764705882353 END) + CASE
  #> WHEN ([wt] < 2.41) THEN 29.8
  #> ELSE CASE WHEN ([disp] < 266.9) THEN 21.325 ELSE 14.6916666666667 END
  #> END) / 5
  ```

## Classification

``` r

set.seed(100)
model <- baguette::bagger(Species ~ ., data = iris, times = 3)

tidypredict_test(model, iris)
#> tidypredict test results
#> Difference threshold: 0
#> 
#>  All results are within the difference threshold
```

## parsnip

Models fit with
[`parsnip::bag_tree()`](https://parsnip.tidymodels.org/reference/bag_tree.html)
and the `"rpart"` engine are supported as well.

``` r

library(parsnip)

set.seed(100)
model <- bag_tree(mode = "regression") %>%
  set_engine("rpart", times = 5) %>%
  fit(mpg ~ wt + cyl + disp, data = mtcars)

tidypredict_fit(model)
#> (case_when(wt < 2.0775 ~ case_when(wt < 1.674 ~ 30.4, .default = 33.9), 
#>     .default = case_when(wt < 2.975 ~ case_when(wt < 2.3925 ~ 
#>         22.8, .default = case_when(cyl < 5 ~ case_when(wt < 2.6225 ~ 
#>         21.5, .default = 21.4), .default = case_when(disp < 152.5 ~ 
#>         19.7, .default = 21))), .default = case_when(disp < 450 ~ 
#>         case_when(disp < 355.5 ~ case_when(disp < 288.4 ~ case_when(wt < 
#>             3.9 ~ case_when(wt < 3.595 ~ case_when(wt < 3.45 ~ 
#>             17.8, .default = 18.1), .default = 17.3), .default = 16.4), 
#>             .default = case_when(disp < 311 ~ case_when(disp < 
#>                 302.5 ~ 15, .default = 15.2), .default = case_when(wt < 
#>                 3.345 ~ 15.8, .default = 15.5))), .default = case_when(wt < 
#>             4.595 ~ case_when(wt < 3.7075 ~ case_when(wt < 3.505 ~ 
#>             18.7, .default = 14.3), .default = 19.2), .default = 14.7)), 
#>         .default = 10.4))) + case_when(wt < 2.26 ~ case_when(wt < 
#>     2.17 ~ case_when(wt < 1.724 ~ 30.4, .default = case_when(wt < 
#>     2.0375 ~ 27.3, .default = 26)), .default = 32.4), .default = case_when(cyl < 
#>     7 ~ case_when(wt < 3.3275 ~ case_when(cyl < 5 ~ case_when(wt < 
#>     2.3925 ~ 22.8, .default = case_when(wt < 2.965 ~ case_when(wt < 
#>     2.6225 ~ 21.5, .default = 21.4), .default = 22.8)), .default = case_when(disp < 
#>     152.5 ~ 19.7, .default = case_when(wt < 3.045 ~ 21, .default = 21.4))), 
#>     .default = case_when(wt < 3.45 ~ 19.2, .default = 18.1)), 
#>     .default = case_when(disp < 430 ~ case_when(disp < 380 ~ 
#>         case_when(wt < 3.955 ~ case_when(wt < 3.81 ~ case_when(disp < 
#>             355.5 ~ case_when(disp < 326 ~ case_when(wt < 3.675 ~ 
#>             15, .default = 15.2), .default = 15.8), .default = 14.3), 
#>             .default = 13.3), .default = 16.4), .default = 19.2), 
#>         .default = 10.4))) + case_when(wt < 2.26 ~ case_when(disp < 
#>     78.85 ~ case_when(wt < 1.9075 ~ 30.4, .default = 32.4), .default = case_when(wt < 
#>     1.724 ~ 30.4, .default = 27.3)), .default = case_when(cyl < 
#>     7 ~ case_when(wt < 3.315 ~ case_when(wt < 3.0325 ~ case_when(wt < 
#>     2.47 ~ 22.8, .default = case_when(disp < 152.5 ~ case_when(wt < 
#>     2.775 ~ 19.7, .default = 21.4), .default = 21)), .default = 24.4), 
#>     .default = case_when(wt < 3.45 ~ 18.5, .default = 18.1)), 
#>     .default = case_when(wt < 4.49 ~ case_when(disp < 339 ~ case_when(wt < 
#>         3.65 ~ case_when(disp < 311 ~ case_when(wt < 3.5025 ~ 
#>         15.2, .default = 15), .default = 15.5), .default = 17.3), 
#>         .default = case_when(wt < 3.505 ~ 18.7, .default = 14.3)), 
#>         .default = 10.4))) + case_when(disp < 163.8 ~ case_when(disp < 
#>     101.55 ~ case_when(wt < 2.0675 ~ case_when(wt < 1.775 ~ case_when(wt < 
#>     1.564 ~ 30.4, .default = 30.4), .default = 27.3), .default = 32.4), 
#>     .default = case_when(wt < 2.23 ~ 26, .default = case_when(wt < 
#>         3.0325 ~ case_when(wt < 2.3925 ~ 22.8, .default = case_when(wt < 
#>         2.8275 ~ case_when(wt < 2.6225 ~ 21.5, .default = 21.4), 
#>         .default = 21)), .default = 24.4))), .default = case_when(wt < 
#>     4.66 ~ case_when(disp < 288.4 ~ case_when(wt < 3.755 ~ case_when(wt < 
#>     3.585 ~ 17.8, .default = 17.3), .default = case_when(wt < 
#>     3.925 ~ 15.2, .default = 16.4)), .default = case_when(wt < 
#>     3.545 ~ case_when(disp < 311 ~ 15.2, .default = case_when(wt < 
#>     3.345 ~ 15.8, .default = 15.5)), .default = case_when(wt < 
#>     3.705 ~ case_when(disp < 330.5 ~ 15, .default = 14.3), .default = 13.3))), 
#>     .default = 10.4)) + case_when(wt < 2.41 ~ case_when(disp < 
#>     107.7 ~ case_when(wt < 1.9075 ~ 30.4, .default = 32.4), .default = 26), 
#>     .default = case_when(disp < 266.9 ~ case_when(cyl < 5 ~ case_when(wt < 
#>         3.17 ~ case_when(wt < 2.965 ~ 21.4, .default = 22.8), 
#>         .default = 24.4), .default = case_when(wt < 3.3275 ~ 
#>         case_when(disp < 152.5 ~ 19.7, .default = case_when(wt < 
#>             3.045 ~ 21, .default = 21.4)), .default = 18.5)), 
#>         .default = case_when(wt < 4.66 ~ case_when(wt < 3.505 ~ 
#>             case_when(wt < 3.4375 ~ case_when(disp < 327.5 ~ 
#>                 15.2, .default = 15.8), .default = 18.7), .default = case_when(wt < 
#>             3.955 ~ case_when(wt < 3.81 ~ case_when(disp < 330.5 ~ 
#>             case_when(wt < 3.675 ~ 15, .default = 15.2), .default = 14.3), 
#>             .default = 13.3), .default = 16.4)), .default = 10.4))))/5L
```
