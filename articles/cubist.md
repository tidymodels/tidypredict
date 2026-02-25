# Cubist models

| Function                                                                                                                                                                                                                                                       | Works |
|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔     |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)                                                                                                                                                             | ✔     |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)                                                                                                                                                                       | ✗     |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md)                                                     | ✗     |
| `parsnip`                                                                                                                                                                                                                                                      | ✗     |

## `tidypredict_` functions

``` r
library(Cubist)
data("BostonHousing", package = "mlbench")

model <- Cubist::cubist(
  x = BostonHousing[, -14],
  y = BostonHousing$medv,
  committees = 3
)
```

- Create the R formula

  ``` r
  tidypredict_fit(model)
  #> ((ifelse(nox > 0.66799998, -1.11 + crim * -0.02 + nox * 21.4 + 
  #>     rm * 0.1 + age * -0.003 + dis * 2.93 + ptratio * -0.13 + 
  #>     b * 0.008 + lstat * -0.33, 0) + ifelse(lstat > 9.5900002 & 
  #>     nox <= 0.66799998, 23.57 + crim * 0.05 + nox * -5.2 + rm * 
  #>     3.1 + age * -0.048 + dis * -0.81 + rad * 0.02 + tax * -0.0041 + 
  #>     ptratio * -0.71 + b * 0.01 + lstat * -0.15, 0) + ifelse(lstat <= 
  #>     9.5900002 & rm <= 6.2259998, 1.18 + crim * 3.83 + rm * 4.3 + 
  #>     age * -0.06 + dis * -0.09 + tax * -0.003 + ptratio * -0.08 + 
  #>     lstat * -0.11, 0) + ifelse(lstat <= 9.5900002 & rm > 6.2259998, 
  #>     -4.71 + crim * 2.22 + zn * 0.008 + nox * -1.7 + rm * 9.2 + 
  #>         age * -0.04 + dis * -0.71 + rad * 0.03 + tax * -0.0182 + 
  #>         ptratio * -0.72 + lstat * -0.83, 0))/((nox > 0.66799998) + 
  #>     (lstat > 9.5900002 & nox <= 0.66799998) + (lstat <= 9.5900002 & 
  #>     rm <= 6.2259998) + (lstat <= 9.5900002 & rm > 6.2259998)) + 
  #>     (ifelse(dis <= 1.7553999 & lstat > 5.1199999, 122.32 + crim * 
  #>         -0.29 + nox * -21.6 + rm * -3 + dis * -30.88 + rad * 
  #>         0.02 + tax * -0.001 + b * -0.023 + lstat * -0.73, 0) + 
  #>         ifelse(rm <= 6.5450001 & lstat > 5.1199999, 27.8 + crim * 
  #>             -0.16 + zn * 0.007 + nox * -3.9 + rm * 2 + age * 
  #>             -0.035 + dis * -0.7 + rad * 0.28 + tax * -0.0135 + 
  #>             ptratio * -0.6 + b * 0.013 + lstat * -0.25, 0) + 
  #>         ifelse(rm > 6.5450001 & lstat > 5.1199999, 22.21 + crim * 
  #>             -0.04 + zn * 0.01 + indus * -0.02 + nox * -4 + rm * 
  #>             4.7 + dis * -0.34 + rad * 0.11 + tax * -0.0248 + 
  #>             ptratio * -0.9 + b * 0.002 + lstat * -0.1, 0) + ifelse(lstat <= 
  #>         5.1199999 & rm <= 8.0340004, -71.95 + rm * 17 + age * 
  #>         -0.06 + tax * -0.0112 + ptratio * -0.48 + lstat * -0.03, 
  #>         0) + ifelse(rm > 8.0340004 & dis > 3.1991999, -32.79 + 
  #>         crim * -0.01 + zn * 0.005 + nox * -1.8 + rm * 12.9 + 
  #>         age * -0.117 + dis * -0.15 + rad * 0.04 + tax * -0.0246 + 
  #>         ptratio * -1.05 + lstat * -0.04, 0) + ifelse(lstat <= 
  #>         5.1199999 & dis <= 3.1991999, 53.41 + rm * 1.6 + dis * 
  #>         -7.16 + tax * 0.0088 + lstat * -0.68, 0))/((dis <= 1.7553999 & 
  #>         lstat > 5.1199999) + (rm <= 6.5450001 & lstat > 5.1199999) + 
  #>         (rm > 6.5450001 & lstat > 5.1199999) + (lstat <= 5.1199999 & 
  #>         rm <= 8.0340004) + (rm > 8.0340004 & dis > 3.1991999) + 
  #>         (lstat <= 5.1199999 & dis <= 3.1991999)) + (ifelse(nox > 
  #>     0.66799998, -36.31 + crim * 0.08 + nox * 48.4 + dis * 7.52 + 
  #>     b * 0.01 + lstat * -0.24, 0) + ifelse(lstat > 9.5299997 & 
  #>     nox <= 0.66799998, 28.04 + nox * -4.8 + rm * 2.9 + age * 
  #>     -0.051 + dis * -0.86 + rad * 0.01 + tax * -0.0019 + ptratio * 
  #>     -0.72 + lstat * -0.12, 0) + ifelse(lstat <= 9.5299997, -26.05 + 
  #>     crim * 0.89 + nox * -2.3 + rm * 9.6 + dis * -0.17 + rad * 
  #>     0.02 + tax * -0.0055 + ptratio * -0.12 + b * 0.001 + lstat * 
  #>     -0.74, 0) + ifelse(lstat <= 9.5299997 & dis <= 2.6403, 136.67 + 
  #>     crim * 7.2 + nox * -96.6 + rm * 1.1 + tax * -0.0033 + ptratio * 
  #>     -3.31 + lstat * -0.1, 0))/((nox > 0.66799998) + (lstat > 
  #>     9.5299997 & nox <= 0.66799998) + (lstat <= 9.5299997) + (lstat <= 
  #>     9.5299997 & dis <= 2.6403)))/3
  ```

- SQL output example

  ``` r
  tidypredict_sql(model, dbplyr::simulate_odbc())
  #> <SQL> ((((((CASE WHEN (`nox` > 0.66799998) THEN ((((((((-1.11 + `crim` * -0.02) + `nox` * 21.4) + `rm` * 0.1) + `age` * -0.003) + `dis` * 2.93) + `ptratio` * -0.13) + `b` * 0.008) + `lstat` * -0.33) WHEN NOT (`nox` > 0.66799998) THEN 0.0 END + CASE WHEN (`lstat` > 9.5900002 AND `nox` <= 0.66799998) THEN ((((((((((23.57 + `crim` * 0.05) + `nox` * -5.2) + `rm` * 3.1) + `age` * -0.048) + `dis` * -0.81) + `rad` * 0.02) + `tax` * -0.0041) + `ptratio` * -0.71) + `b` * 0.01) + `lstat` * -0.15) WHEN NOT (`lstat` > 9.5900002 AND `nox` <= 0.66799998) THEN 0.0 END) + CASE WHEN (`lstat` <= 9.5900002 AND `rm` <= 6.2259998) THEN (((((((1.18 + `crim` * 3.83) + `rm` * 4.3) + `age` * -0.06) + `dis` * -0.09) + `tax` * -0.003) + `ptratio` * -0.08) + `lstat` * -0.11) WHEN NOT (`lstat` <= 9.5900002 AND `rm` <= 6.2259998) THEN 0.0 END) + CASE WHEN (`lstat` <= 9.5900002 AND `rm` > 6.2259998) THEN ((((((((((-4.71 + `crim` * 2.22) + `zn` * 0.008) + `nox` * -1.7) + `rm` * 9.2) + `age` * -0.04) + `dis` * -0.71) + `rad` * 0.03) + `tax` * -0.0182) + `ptratio` * -0.72) + `lstat` * -0.83) WHEN NOT (`lstat` <= 9.5900002 AND `rm` > 6.2259998) THEN 0.0 END) / (((`nox` > 0.66799998 + `lstat` > 9.5900002 AND `nox` <= 0.66799998) + `lstat` <= 9.5900002 AND `rm` <= 6.2259998) + `lstat` <= 9.5900002 AND `rm` > 6.2259998)) + (((((CASE WHEN (`dis` <= 1.7553999 AND `lstat` > 5.1199999) THEN ((((((((122.32 + `crim` * -0.29) + `nox` * -21.6) + `rm` * -3.0) + `dis` * -30.88) + `rad` * 0.02) + `tax` * -0.001) + `b` * -0.023) + `lstat` * -0.73) WHEN NOT (`dis` <= 1.7553999 AND `lstat` > 5.1199999) THEN 0.0 END + CASE WHEN (`rm` <= 6.5450001 AND `lstat` > 5.1199999) THEN (((((((((((27.8 + `crim` * -0.16) + `zn` * 0.007) + `nox` * -3.9) + `rm` * 2.0) + `age` * -0.035) + `dis` * -0.7) + `rad` * 0.28) + `tax` * -0.0135) + `ptratio` * -0.6) + `b` * 0.013) + `lstat` * -0.25) WHEN NOT (`rm` <= 6.5450001 AND `lstat` > 5.1199999) THEN 0.0 END) + CASE WHEN (`rm` > 6.5450001 AND `lstat` > 5.1199999) THEN (((((((((((22.21 + `crim` * -0.04) + `zn` * 0.01) + `indus` * -0.02) + `nox` * -4.0) + `rm` * 4.7) + `dis` * -0.34) + `rad` * 0.11) + `tax` * -0.0248) + `ptratio` * -0.9) + `b` * 0.002) + `lstat` * -0.1) WHEN NOT (`rm` > 6.5450001 AND `lstat` > 5.1199999) THEN 0.0 END) + CASE WHEN (`lstat` <= 5.1199999 AND `rm` <= 8.0340004) THEN (((((-71.95 + `rm` * 17.0) + `age` * -0.06) + `tax` * -0.0112) + `ptratio` * -0.48) + `lstat` * -0.03) WHEN NOT (`lstat` <= 5.1199999 AND `rm` <= 8.0340004) THEN 0.0 END) + CASE WHEN (`rm` > 8.0340004 AND `dis` > 3.1991999) THEN ((((((((((-32.79 + `crim` * -0.01) + `zn` * 0.005) + `nox` * -1.8) + `rm` * 12.9) + `age` * -0.117) + `dis` * -0.15) + `rad` * 0.04) + `tax` * -0.0246) + `ptratio` * -1.05) + `lstat` * -0.04) WHEN NOT (`rm` > 8.0340004 AND `dis` > 3.1991999) THEN 0.0 END) + CASE WHEN (`lstat` <= 5.1199999 AND `dis` <= 3.1991999) THEN ((((53.41 + `rm` * 1.6) + `dis` * -7.16) + `tax` * 0.0088) + `lstat` * -0.68) WHEN NOT (`lstat` <= 5.1199999 AND `dis` <= 3.1991999) THEN 0.0 END) / (((((`dis` <= 1.7553999 AND `lstat` > 5.1199999 + `rm` <= 6.5450001 AND `lstat` > 5.1199999) + `rm` > 6.5450001 AND `lstat` > 5.1199999) + `lstat` <= 5.1199999 AND `rm` <= 8.0340004) + `rm` > 8.0340004 AND `dis` > 3.1991999) + `lstat` <= 5.1199999 AND `dis` <= 3.1991999)) + (((CASE WHEN (`nox` > 0.66799998) THEN (((((-36.31 + `crim` * 0.08) + `nox` * 48.4) + `dis` * 7.52) + `b` * 0.01) + `lstat` * -0.24) WHEN NOT (`nox` > 0.66799998) THEN 0.0 END + CASE WHEN (`lstat` > 9.5299997 AND `nox` <= 0.66799998) THEN ((((((((28.04 + `nox` * -4.8) + `rm` * 2.9) + `age` * -0.051) + `dis` * -0.86) + `rad` * 0.01) + `tax` * -0.0019) + `ptratio` * -0.72) + `lstat` * -0.12) WHEN NOT (`lstat` > 9.5299997 AND `nox` <= 0.66799998) THEN 0.0 END) + CASE WHEN (`lstat` <= 9.5299997) THEN (((((((((-26.05 + `crim` * 0.89) + `nox` * -2.3) + `rm` * 9.6) + `dis` * -0.17) + `rad` * 0.02) + `tax` * -0.0055) + `ptratio` * -0.12) + `b` * 0.001) + `lstat` * -0.74) WHEN NOT (`lstat` <= 9.5299997) THEN 0.0 END) + CASE WHEN (`lstat` <= 9.5299997 AND `dis` <= 2.6403) THEN ((((((136.67 + `crim` * 7.2) + `nox` * -96.6) + `rm` * 1.1) + `tax` * -0.0033) + `ptratio` * -3.31) + `lstat` * -0.1) WHEN NOT (`lstat` <= 9.5299997 AND `dis` <= 2.6403) THEN 0.0 END) / (((`nox` > 0.66799998 + `lstat` > 9.5299997 AND `nox` <= 0.66799998) + `lstat` <= 9.5299997) + `lstat` <= 9.5299997 AND `dis` <= 2.6403)) / 3.0
  ```

- Add the prediction to the original table

  ``` r
  library(dplyr)

  BostonHousing %>%
    tidypredict_to_column(model) %>%
    glimpse()
  #> Rows: 506
  #> Columns: 15
  #> $ crim    <dbl> 0.00632, 0.02731, 0.02729, 0.03237, 0.06905, 0.02985,…
  #> $ zn      <dbl> 18.0, 0.0, 0.0, 0.0, 0.0, 0.0, 12.5, 12.5, 12.5, 12.5…
  #> $ indus   <dbl> 2.31, 7.07, 7.07, 2.18, 2.18, 2.18, 7.87, 7.87, 7.87,…
  #> $ chas    <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
  #> $ nox     <dbl> 0.538, 0.469, 0.469, 0.458, 0.458, 0.458, 0.524, 0.52…
  #> $ rm      <dbl> 6.575, 6.421, 7.185, 6.998, 7.147, 6.430, 6.012, 6.17…
  #> $ age     <dbl> 65.2, 78.9, 61.1, 45.8, 54.2, 58.7, 66.6, 96.1, 100.0…
  #> $ dis     <dbl> 4.0900, 4.9671, 4.9671, 6.0622, 6.0622, 6.0622, 5.560…
  #> $ rad     <dbl> 1, 2, 2, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4,…
  #> $ tax     <dbl> 296, 242, 242, 222, 222, 222, 311, 311, 311, 311, 311…
  #> $ ptratio <dbl> 15.3, 17.8, 17.8, 18.7, 18.7, 18.7, 15.2, 15.2, 15.2,…
  #> $ b       <dbl> 396.90, 396.90, 392.83, 394.63, 396.90, 394.12, 395.6…
  #> $ lstat   <dbl> 4.98, 9.14, 4.03, 2.94, 5.33, 5.21, 12.43, 19.15, 29.…
  #> $ medv    <dbl> 24.0, 21.6, 34.7, 33.4, 36.2, 28.7, 22.9, 27.1, 16.5,…
  #> $ fit     <dbl> 27.50665, 22.71805, 34.78128, 33.19372, 31.93653, 25.…
  ```

We are not able to give an exact match of the original predictions [due
to a minor bug](https://github.com/topepo/Cubist/issues/62) in Cubist.

## Parse model spec

Here is an example of the model spec:

``` r
pm <- parse_model(model)
str(pm, 2)
#> List of 2
#>  $ general:List of 6
#>   ..$ model       : chr "cubist"
#>   ..$ type        : chr "tree"
#>   ..$ version     : num 3
#>   ..$ mode        : chr "ifelse"
#>   ..$ n_committees: num 3
#>   ..$ ommittee_id : int [1:14] 1 1 1 1 2 2 2 2 2 2 ...
#>  $ trees  :List of 1
#>   ..$ :List of 14
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_tree" "list"
```

``` r
str(pm$terms[1:2])
#>  NULL
```

## Limitations

- [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  is not supported
- Prediction intervals are not supported
- Cubist uses 32-bit floats internally, which may cause prediction
  discrepancies at exact split boundaries. See the [float
  precision](https://tidypredict.tidymodels.org/articles/float-precision.md)
  article for details.
