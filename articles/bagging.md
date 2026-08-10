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
The `"CART"` base model, which fits
[`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html) trees, and
the `"C5.0"` base model, which fits
[`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html)
trees, are supported.
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
  #> (case_when(case_when(!is.na(wt) ~ wt < 2.975, !is.na(disp) ~ 
  #>     disp < 163.8, !is.na(cyl) ~ cyl < 7, .default = FALSE) ~ 
  #>     23.0583333333333, .default = case_when(case_when(!is.na(wt) ~ 
  #>     wt < 3.545, !is.na(disp) ~ disp < 380, !is.na(cyl) ~ cyl < 
  #>     7, .default = FALSE) ~ 16.9125, .default = 15.125)) + case_when(case_when(!is.na(wt) ~ 
  #>     wt < 3.16, !is.na(disp) ~ disp < 163.8, !is.na(cyl) ~ cyl < 
  #>     7, .default = FALSE) ~ 24.3642857142857, .default = 15.7444444444444) + 
  #>     case_when(case_when(!is.na(wt) ~ wt < 2.26, !is.na(disp) ~ 
  #>         disp < 101.55, !is.na(cyl) ~ cyl < 5, .default = FALSE) ~ 
  #>         30.0857142857143, .default = case_when(case_when(!is.na(cyl) ~ 
  #>         cyl < 7, !is.na(disp) ~ disp < 250.4, !is.na(wt) ~ wt < 
  #>         3.3125, .default = TRUE) ~ 20.4428571428571, .default = 14.6454545454545)) + 
  #>     case_when(case_when(!is.na(disp) ~ disp < 163.8, !is.na(wt) ~ 
  #>         wt < 3.3125, !is.na(cyl) ~ cyl < 5, .default = FALSE) ~ 
  #>         25.6733333333333, .default = 14.6764705882353) + case_when(case_when(!is.na(wt) ~ 
  #>     wt < 2.41, !is.na(disp) ~ disp < 120.65, !is.na(cyl) ~ cyl < 
  #>     5, .default = FALSE) ~ 29.8, .default = case_when(case_when(!is.na(disp) ~ 
  #>     disp < 266.9, !is.na(cyl) ~ cyl < 7, !is.na(wt) ~ wt < 3.325, 
  #>     .default = FALSE) ~ 21.325, !is.na(disp) | !is.na(cyl) | 
  #>     !is.na(wt) ~ 14.6916666666667, .default = 18.0083333333333)))/5L
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
  #> WHEN (CASE
  #> WHEN (NOT(([wt] IS NULL))) THEN ([wt] < 2.975)
  #> WHEN (NOT(([disp] IS NULL))) THEN ([disp] < 163.8)
  #> WHEN (NOT(([cyl] IS NULL))) THEN ([cyl] < 7.0)
  #> ELSE 0
  #> END) THEN 23.0583333333333
  #> ELSE CASE
  #> WHEN (CASE
  #> WHEN (NOT(([wt] IS NULL))) THEN ([wt] < 3.545)
  #> WHEN (NOT(([disp] IS NULL))) THEN ([disp] < 380.0)
  #> WHEN (NOT(([cyl] IS NULL))) THEN ([cyl] < 7.0)
  #> ELSE 0
  #> END) THEN 16.9125
  #> ELSE 15.125
  #> END
  #> END + CASE
  #> WHEN (CASE
  #> WHEN (NOT(([wt] IS NULL))) THEN ([wt] < 3.16)
  #> WHEN (NOT(([disp] IS NULL))) THEN ([disp] < 163.8)
  #> WHEN (NOT(([cyl] IS NULL))) THEN ([cyl] < 7.0)
  #> ELSE 0
  #> END) THEN 24.3642857142857
  #> ELSE 15.7444444444444
  #> END) + CASE
  #> WHEN (CASE
  #> WHEN (NOT(([wt] IS NULL))) THEN ([wt] < 2.26)
  #> WHEN (NOT(([disp] IS NULL))) THEN ([disp] < 101.55)
  #> WHEN (NOT(([cyl] IS NULL))) THEN ([cyl] < 5.0)
  #> ELSE 0
  #> END) THEN 30.0857142857143
  #> ELSE CASE
  #> WHEN (CASE
  #> WHEN (NOT(([cyl] IS NULL))) THEN ([cyl] < 7.0)
  #> WHEN (NOT(([disp] IS NULL))) THEN ([disp] < 250.4)
  #> WHEN (NOT(([wt] IS NULL))) THEN ([wt] < 3.3125)
  #> ELSE 1
  #> END) THEN 20.4428571428571
  #> ELSE 14.6454545454545
  #> END
  #> END) + CASE
  #> WHEN (CASE
  #> WHEN (NOT(([disp] IS NULL))) THEN ([disp] < 163.8)
  #> WHEN (NOT(([wt] IS NULL))) THEN ([wt] < 3.3125)
  #> WHEN (NOT(([cyl] IS NULL))) THEN ([cyl] < 5.0)
  #> ELSE 0
  #> END) THEN 25.6733333333333
  #> ELSE 14.6764705882353
  #> END) + CASE
  #> WHEN (CASE
  #> WHEN (NOT(([wt] IS NULL))) THEN ([wt] < 2.41)
  #> WHEN (NOT(([disp] IS NULL))) THEN ([disp] < 120.65)
  #> WHEN (NOT(([cyl] IS NULL))) THEN ([cyl] < 5.0)
  #> ELSE 0
  #> END) THEN 29.8
  #> ELSE CASE
  #> WHEN (CASE
  #> WHEN (NOT(([disp] IS NULL))) THEN ([disp] < 266.9)
  #> WHEN (NOT(([cyl] IS NULL))) THEN ([cyl] < 7.0)
  #> WHEN (NOT(([wt] IS NULL))) THEN ([wt] < 3.325)
  #> ELSE 0
  #> END) THEN 21.325
  #> WHEN (NOT(([disp] IS NULL)) OR NOT(([cyl] IS NULL)) OR NOT(([wt] IS NULL))) THEN 14.6916666666667
  #> ELSE 18.0083333333333
  #> END
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

C5.0 trees are only fit for classification, and are used by passing
`base_model = "C5.0"`.

``` r

set.seed(100)
model <- baguette::bagger(
  Species ~ .,
  data = iris,
  base_model = "C5.0",
  times = 3
)

tidypredict_test(model, iris)
#> tidypredict test results
#> Difference threshold: 0
#> 
#>  All results are within the difference threshold
```

## parsnip

Models fit with
[`parsnip::bag_tree()`](https://parsnip.tidymodels.org/reference/bag_tree.html)
and the `"rpart"` or `"C5.0"` engine are supported as well.

``` r

library(parsnip)

set.seed(100)
model <- bag_tree(mode = "regression") %>%
  set_engine("rpart", times = 5) %>%
  fit(mpg ~ wt + cyl + disp, data = mtcars)

tidypredict_fit(model)
#> (case_when(case_when(!is.na(wt) ~ wt < 2.0775, !is.na(disp) ~ 
#>     disp < 101.55, .default = FALSE) ~ case_when(case_when(!is.na(wt) ~ 
#>     wt < 1.674, .default = FALSE) ~ 30.4, !is.na(wt) ~ 33.9, 
#>     .default = 32.15), .default = case_when(case_when(!is.na(wt) ~ 
#>     wt < 2.975, !is.na(disp) ~ disp < 163.8, !is.na(cyl) ~ cyl < 
#>     7, .default = FALSE) ~ case_when(case_when(!is.na(wt) ~ wt < 
#>     2.3925, !is.na(disp) ~ disp < 114.05, .default = FALSE) ~ 
#>     22.8, .default = case_when(case_when(!is.na(cyl) ~ cyl < 
#>     5, !is.na(disp) ~ disp < 133, !is.na(wt) ~ wt < 2.5425, .default = FALSE) ~ 
#>     case_when(case_when(!is.na(wt) ~ wt < 2.6225, .default = TRUE) ~ 
#>         21.5, .default = 21.4), .default = case_when(case_when(!is.na(disp) ~ 
#>     disp < 152.5, !is.na(wt) ~ !wt < 2.695, .default = FALSE) ~ 
#>     19.7, .default = 21))), .default = case_when(case_when(!is.na(disp) ~ 
#>     disp < 450, !is.na(wt) ~ wt < 4.66, .default = TRUE) ~ case_when(case_when(!is.na(disp) ~ 
#>     disp < 355.5, !is.na(wt) ~ wt < 3.7875, .default = TRUE) ~ 
#>     case_when(case_when(!is.na(disp) ~ disp < 288.4, !is.na(wt) ~ 
#>         !wt < 3.65, !is.na(cyl) ~ cyl < 7, .default = FALSE) ~ 
#>         case_when(case_when(!is.na(wt) ~ wt < 3.9, .default = TRUE) ~ 
#>             case_when(case_when(!is.na(wt) ~ wt < 3.595, .default = TRUE) ~ 
#>                 case_when(case_when(!is.na(wt) ~ wt < 3.45, .default = FALSE) ~ 
#>                   17.8, !is.na(wt) ~ 18.1, .default = 17.95), 
#>                 .default = 17.3), .default = 16.4), .default = case_when(case_when(!is.na(disp) ~ 
#>         disp < 311, !is.na(wt) ~ !wt < 3.545, .default = FALSE) ~ 
#>         case_when(case_when(!is.na(disp) ~ disp < 302.5, .default = TRUE) ~ 
#>             15, .default = 15.2), !is.na(disp) | !is.na(wt) ~ 
#>         case_when(case_when(!is.na(wt) ~ wt < 3.345, .default = FALSE) ~ 
#>             15.8, .default = 15.5), .default = 15.3333333333333)), 
#>     .default = case_when(case_when(!is.na(wt) ~ wt < 4.595, .default = TRUE) ~ 
#>         case_when(case_when(!is.na(wt) ~ wt < 3.7075, !is.na(disp) ~ 
#>             disp < 380, .default = FALSE) ~ case_when(case_when(!is.na(wt) ~ 
#>             wt < 3.505, .default = TRUE) ~ 18.7, .default = 14.3), 
#>             !is.na(wt) | !is.na(disp) ~ 19.2, .default = 18.2166666666667), 
#>         .default = 14.7)), .default = 10.4))) + case_when(case_when(!is.na(wt) ~ 
#>     wt < 2.26, !is.na(disp) ~ disp < 101.55, .default = FALSE) ~ 
#>     case_when(case_when(!is.na(wt) ~ wt < 2.17, !is.na(disp) ~ 
#>         !disp < 78.85, .default = TRUE) ~ case_when(case_when(!is.na(wt) ~ 
#>         wt < 1.724, .default = FALSE) ~ 30.4, .default = case_when(case_when(!is.na(wt) ~ 
#>         wt < 2.0375, .default = FALSE) ~ 27.3, !is.na(wt) ~ 26, 
#>         .default = 26.65)), .default = 32.4), .default = case_when(case_when(!is.na(cyl) ~ 
#>     cyl < 7, !is.na(disp) ~ disp < 266.9, !is.na(wt) ~ wt < 3.515, 
#>     .default = FALSE) ~ case_when(case_when(!is.na(wt) ~ wt < 
#>     3.3275, !is.na(disp) ~ disp < 163.8, .default = TRUE) ~ case_when(case_when(!is.na(cyl) ~ 
#>     cyl < 5, !is.na(disp) ~ disp < 142.9, !is.na(wt) ~ wt < 2.5425, 
#>     .default = FALSE) ~ case_when(case_when(!is.na(wt) ~ wt < 
#>     2.3925, .default = FALSE) ~ 22.8, .default = case_when(case_when(!is.na(wt) ~ 
#>     wt < 2.965, .default = TRUE) ~ case_when(case_when(!is.na(wt) ~ 
#>     wt < 2.6225, .default = FALSE) ~ 21.5, .default = 21.4), 
#>     .default = 22.8)), !is.na(cyl) | !is.na(disp) | !is.na(wt) ~ 
#>     case_when(case_when(!is.na(disp) ~ disp < 152.5, .default = FALSE) ~ 
#>         19.7, .default = case_when(case_when(!is.na(wt) ~ wt < 
#>         3.045, .default = TRUE) ~ 21, .default = 21.4)), .default = 21.4), 
#>     .default = case_when(case_when(!is.na(wt) ~ wt < 3.45, .default = FALSE) ~ 
#>         19.2, !is.na(wt) ~ 18.1, .default = 18.65)), .default = case_when(case_when(!is.na(disp) ~ 
#>     disp < 430, !is.na(wt) ~ wt < 4.747, .default = TRUE) ~ case_when(case_when(!is.na(disp) ~ 
#>     disp < 380, !is.na(wt) ~ wt < 3.8425, .default = TRUE) ~ 
#>     case_when(case_when(!is.na(wt) ~ wt < 3.955, .default = TRUE) ~ 
#>         case_when(case_when(!is.na(wt) ~ wt < 3.81, .default = TRUE) ~ 
#>             case_when(case_when(!is.na(disp) ~ disp < 355.5, 
#>                 .default = TRUE) ~ case_when(case_when(!is.na(disp) ~ 
#>                 disp < 326, .default = TRUE) ~ case_when(case_when(!is.na(wt) ~ 
#>                 wt < 3.675, .default = FALSE) ~ 15, .default = 15.2), 
#>                 .default = 15.8), .default = 14.3), .default = 13.3), 
#>         .default = 16.4), .default = 19.2), .default = 10.4))) + 
#>     case_when(case_when(!is.na(wt) ~ wt < 2.26, !is.na(disp) ~ 
#>         disp < 101.55, !is.na(cyl) ~ cyl < 5, .default = FALSE) ~ 
#>         case_when(case_when(!is.na(disp) ~ disp < 78.85, !is.na(wt) ~ 
#>             !wt < 2.0675, .default = TRUE) ~ case_when(case_when(!is.na(wt) ~ 
#>             wt < 1.9075, !is.na(disp) ~ disp < 77.2, .default = FALSE) ~ 
#>             30.4, !is.na(wt) | !is.na(disp) ~ 32.4, .default = 31.4), 
#>             .default = case_when(case_when(!is.na(wt) ~ wt < 
#>                 1.724, .default = FALSE) ~ 30.4, .default = 27.3)), 
#>         .default = case_when(case_when(!is.na(cyl) ~ cyl < 7, 
#>             !is.na(disp) ~ disp < 250.4, !is.na(wt) ~ wt < 3.3125, 
#>             .default = TRUE) ~ case_when(case_when(!is.na(wt) ~ 
#>             wt < 3.315, !is.na(disp) ~ disp < 163.8, .default = TRUE) ~ 
#>             case_when(case_when(!is.na(wt) ~ wt < 3.0325, .default = TRUE) ~ 
#>                 case_when(case_when(!is.na(wt) ~ wt < 2.47, .default = FALSE) ~ 
#>                   22.8, .default = case_when(case_when(!is.na(disp) ~ 
#>                   disp < 152.5, !is.na(wt) ~ !wt < 2.695, .default = FALSE) ~ 
#>                   case_when(case_when(!is.na(wt) ~ wt < 2.775, 
#>                     .default = TRUE) ~ 19.7, .default = 21.4), 
#>                   .default = 21)), .default = 24.4), .default = case_when(case_when(!is.na(wt) ~ 
#>             wt < 3.45, !is.na(disp) ~ disp < 196.3, .default = FALSE) ~ 
#>             18.5, !is.na(wt) | !is.na(disp) ~ 18.1, .default = 18.3)), 
#>             .default = case_when(case_when(!is.na(wt) ~ wt < 
#>                 4.49, !is.na(disp) ~ disp < 410, .default = TRUE) ~ 
#>                 case_when(case_when(!is.na(disp) ~ disp < 339, 
#>                   .default = TRUE) ~ case_when(case_when(!is.na(wt) ~ 
#>                   wt < 3.65, .default = TRUE) ~ case_when(case_when(!is.na(disp) ~ 
#>                   disp < 311, .default = TRUE) ~ case_when(case_when(!is.na(wt) ~ 
#>                   wt < 3.5025, .default = TRUE) ~ 15.2, .default = 15), 
#>                   .default = 15.5), .default = 17.3), .default = case_when(case_when(!is.na(wt) ~ 
#>                   wt < 3.505, .default = TRUE) ~ 18.7, .default = 14.3)), 
#>                 .default = 10.4))) + case_when(case_when(!is.na(disp) ~ 
#>     disp < 163.8, !is.na(wt) ~ wt < 3.3125, !is.na(cyl) ~ cyl < 
#>     5, .default = FALSE) ~ case_when(case_when(!is.na(disp) ~ 
#>     disp < 101.55, !is.na(wt) ~ wt < 2.0375, .default = FALSE) ~ 
#>     case_when(case_when(!is.na(wt) ~ wt < 2.0675, .default = TRUE) ~ 
#>         case_when(case_when(!is.na(wt) ~ wt < 1.775, !is.na(disp) ~ 
#>             !disp < 87.05, .default = TRUE) ~ case_when(case_when(!is.na(wt) ~ 
#>             wt < 1.564, .default = TRUE) ~ 30.4, .default = 30.4), 
#>             .default = 27.3), .default = 32.4), .default = case_when(case_when(!is.na(wt) ~ 
#>     wt < 2.23, .default = FALSE) ~ 26, .default = case_when(case_when(!is.na(wt) ~ 
#>     wt < 3.0325, !is.na(disp) ~ disp < 133.85, .default = TRUE) ~ 
#>     case_when(case_when(!is.na(wt) ~ wt < 2.3925, .default = FALSE) ~ 
#>         22.8, .default = case_when(case_when(!is.na(wt) ~ wt < 
#>         2.8275, !is.na(cyl) ~ cyl < 5, !is.na(disp) ~ disp < 
#>         140.5, .default = FALSE) ~ case_when(case_when(!is.na(wt) ~ 
#>         wt < 2.6225, .default = FALSE) ~ 21.5, !is.na(wt) ~ 21.4, 
#>         .default = 21.45), !is.na(wt) | !is.na(cyl) | !is.na(disp) ~ 
#>         21, .default = 21.225)), .default = 24.4))), .default = case_when(case_when(!is.na(wt) ~ 
#>     wt < 4.66, !is.na(disp) ~ disp < 410, .default = TRUE) ~ 
#>     case_when(case_when(!is.na(disp) ~ disp < 288.4, !is.na(wt) ~ 
#>         !wt < 3.65, !is.na(cyl) ~ cyl < 7, .default = FALSE) ~ 
#>         case_when(case_when(!is.na(wt) ~ wt < 3.755, !is.na(cyl) ~ 
#>             cyl < 7, !is.na(disp) ~ disp < 221.7, .default = TRUE) ~ 
#>             case_when(case_when(!is.na(wt) ~ wt < 3.585, .default = TRUE) ~ 
#>                 17.8, .default = 17.3), .default = case_when(case_when(!is.na(wt) ~ 
#>             wt < 3.925, .default = FALSE) ~ 15.2, !is.na(wt) ~ 
#>             16.4, .default = 15.8)), .default = case_when(case_when(!is.na(wt) ~ 
#>         wt < 3.545, !is.na(disp) ~ disp < 334, .default = TRUE) ~ 
#>         case_when(case_when(!is.na(disp) ~ disp < 311, .default = TRUE) ~ 
#>             15.2, .default = case_when(case_when(!is.na(wt) ~ 
#>             wt < 3.345, .default = FALSE) ~ 15.8, !is.na(wt) ~ 
#>             15.5, .default = 15.65)), .default = case_when(case_when(!is.na(wt) ~ 
#>         wt < 3.705, .default = TRUE) ~ case_when(case_when(!is.na(disp) ~ 
#>         disp < 330.5, .default = FALSE) ~ 15, .default = 14.3), 
#>         .default = 13.3))), .default = 10.4)) + case_when(case_when(!is.na(wt) ~ 
#>     wt < 2.41, !is.na(disp) ~ disp < 120.65, !is.na(cyl) ~ cyl < 
#>     5, .default = FALSE) ~ case_when(case_when(!is.na(disp) ~ 
#>     disp < 107.7, .default = TRUE) ~ case_when(case_when(!is.na(wt) ~ 
#>     wt < 1.9075, !is.na(disp) ~ !disp < 86.9, .default = TRUE) ~ 
#>     30.4, .default = 32.4), .default = 26), .default = case_when(case_when(!is.na(disp) ~ 
#>     disp < 266.9, !is.na(cyl) ~ cyl < 7, !is.na(wt) ~ wt < 3.325, 
#>     .default = FALSE) ~ case_when(case_when(!is.na(cyl) ~ cyl < 
#>     5, !is.na(disp) ~ disp < 153.35, !is.na(wt) ~ wt < 3.2025, 
#>     .default = FALSE) ~ case_when(case_when(!is.na(wt) ~ wt < 
#>     3.17, !is.na(disp) ~ disp < 143.75, .default = TRUE) ~ case_when(case_when(!is.na(wt) ~ 
#>     wt < 2.965, .default = TRUE) ~ 21.4, .default = 22.8), .default = 24.4), 
#>     !is.na(cyl) | !is.na(disp) | !is.na(wt) ~ case_when(case_when(!is.na(wt) ~ 
#>         wt < 3.3275, !is.na(disp) ~ disp < 163.8, .default = TRUE) ~ 
#>         case_when(case_when(!is.na(disp) ~ disp < 152.5, .default = FALSE) ~ 
#>             19.7, .default = case_when(case_when(!is.na(wt) ~ 
#>             wt < 3.045, .default = TRUE) ~ 21, .default = 21.4)), 
#>         .default = 18.5), .default = 21.325), !is.na(disp) | 
#>     !is.na(cyl) | !is.na(wt) ~ case_when(case_when(!is.na(wt) ~ 
#>     wt < 4.66, !is.na(disp) ~ disp < 410, .default = TRUE) ~ 
#>     case_when(case_when(!is.na(wt) ~ wt < 3.505, !is.na(disp) ~ 
#>         !disp < 302.5, .default = FALSE) ~ case_when(case_when(!is.na(wt) ~ 
#>         wt < 3.4375, .default = TRUE) ~ case_when(case_when(!is.na(disp) ~ 
#>         disp < 327.5, .default = TRUE) ~ 15.2, .default = 15.8), 
#>         .default = 18.7), .default = case_when(case_when(!is.na(wt) ~ 
#>         wt < 3.955, !is.na(disp) ~ !disp < 288.4, .default = TRUE) ~ 
#>         case_when(case_when(!is.na(wt) ~ wt < 3.81, .default = TRUE) ~ 
#>             case_when(case_when(!is.na(disp) ~ disp < 330.5, 
#>                 .default = TRUE) ~ case_when(case_when(!is.na(wt) ~ 
#>                 wt < 3.675, .default = FALSE) ~ 15, !is.na(wt) ~ 
#>                 15.2, .default = 15.1), .default = 14.3), .default = 13.3), 
#>         .default = 16.4)), .default = 10.4), .default = 18.0083333333333)))/5L
```
