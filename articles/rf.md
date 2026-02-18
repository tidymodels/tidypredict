# Random Forest

| Function                                                                                                                                                                                                                                                       | Works |
|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔     |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)                                                                                                                                                             | ✗     |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)                                                                                                                                                                       | ✗     |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md)                                                     | ✗     |
| `parsnip`                                                                                                                                                                                                                                                      | ✔     |

## How it works

Here is a simple
[`randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
model using the `mtcars` dataset:

``` r
library(dplyr)
library(tidypredict)
library(randomForest)

model <- randomForest(mpg ~ ., data = mtcars, ntree = 5, proximity = TRUE)
```

## Under the hood

The parser is based on the output from the
[`randomForest::getTree()`](https://rdrr.io/pkg/randomForest/man/getTree.html)
function. It will return as many decision paths as there are non-NA rows
in the `prediction` field.

``` r
getTree(model, labelVar = TRUE) %>%
  head()
#>   left daughter right daughter split var split point status prediction
#> 1             2              3      carb       1.500     -3   20.12813
#> 2             4              5      gear       3.500     -3   28.82222
#> 3             6              7      disp     221.700     -3   16.72609
#> 4             0              0      <NA>       0.000     -1   18.10000
#> 5             8              9        hp      65.500     -3   31.88571
#> 6            10             11        wt       3.295     -3   21.38333
```

The output from
[`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
is transformed into a `dplyr`, a.k.a Tidy Eval, formula. Each decision
tree becomes one
[`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
statement, which are then combined.

``` r
tidypredict_fit(model)
#> (case_when(gear <= 3.5 & carb <= 1.5 ~ 18.1, hp <= 65.5 & gear > 
#>     3.5 & carb <= 1.5 ~ 33.9, wt <= 3.295 & disp <= 221.7 & carb > 
#>     1.5 ~ 21.82, wt > 3.295 & disp <= 221.7 & carb > 1.5 ~ 19.2, 
#>     wt > 4.66 & disp > 221.7 & carb > 1.5 ~ 10.4, qsec <= 19.185 & 
#>         hp > 65.5 & gear > 3.5 & carb <= 1.5 ~ 27.3, qsec > 19.185 & 
#>         hp > 65.5 & gear > 3.5 & carb <= 1.5 ~ 32.4, qsec <= 
#>         16.96 & wt <= 4.66 & disp > 221.7 & carb > 1.5 ~ 14.94, 
#>     hp <= 162.5 & qsec > 16.96 & wt <= 4.66 & disp > 221.7 & 
#>         carb > 1.5 ~ 15.2, carb <= 2.5 & hp > 162.5 & qsec > 
#>         16.96 & wt <= 4.66 & disp > 221.7 & carb > 1.5 ~ 19.2, 
#>     .default = 16.34) + case_when(hp <= 65.5 & hp <= 95 ~ 31.5666666666667, 
#>     wt <= 2.23 & hp > 65.5 & hp <= 95 ~ 26.325, wt > 2.23 & hp > 
#>         65.5 & hp <= 95 ~ 22.8, qsec <= 17.62 & hp > 190 & hp > 
#>         95 ~ 14.325, qsec > 17.62 & hp > 190 & hp > 95 ~ 10.4, 
#>     hp <= 130 & drat <= 3.385 & hp <= 190 & hp > 95 ~ 19.2, hp > 
#>         130 & drat <= 3.385 & hp <= 190 & hp > 95 ~ 17.025, gear > 
#>         4.5 & drat > 3.385 & hp <= 190 & hp > 95 ~ 19.7, vs <= 
#>         0.5 & gear <= 4.5 & drat > 3.385 & hp <= 190 & hp > 95 ~ 
#>         21, .default = 21.5) + case_when(qsec <= 18.755 & disp <= 
#>     142.9 ~ 22.8, qsec > 18.755 & disp <= 142.9 ~ 28.74, disp <= 
#>     163.8 & wt <= 3.49 & disp > 142.9 ~ 20.1333333333333, qsec > 
#>     17.71 & wt > 3.49 & disp > 142.9 ~ 10.4, gear <= 3.5 & disp > 
#>     163.8 & wt <= 3.49 & disp > 142.9 ~ 19.2, gear > 3.5 & disp > 
#>     163.8 & wt <= 3.49 & disp > 142.9 ~ 17.4, drat <= 2.915 & 
#>     hp <= 212.5 & qsec <= 17.71 & wt > 3.49 & disp > 142.9 ~ 
#>     15.5, drat > 2.915 & hp <= 212.5 & qsec <= 17.71 & wt > 3.49 & 
#>     disp > 142.9 ~ 18.25, carb <= 6 & hp > 212.5 & qsec <= 17.71 & 
#>     wt > 3.49 & disp > 142.9 ~ 13.7, .default = 15) + case_when(cyl <= 
#>     5 & am <= 0.5 ~ 22.55, wt <= 2.2775 & am > 0.5 ~ 29.78, wt > 
#>     2.2775 & am > 0.5 ~ 18.42, hp > 192.5 & cyl > 5 & am <= 0.5 ~ 
#>     13.84, hp <= 136.5 & hp <= 192.5 & cyl > 5 & am <= 0.5 ~ 
#>     18.5, drat <= 3.11 & hp > 136.5 & hp <= 192.5 & cyl > 5 & 
#>     am <= 0.5 ~ 17.075, .default = 17.3) + case_when(cyl > 5 & 
#>     hp <= 116.5 ~ 21.1, qsec > 18.14 & hp > 116.5 ~ 18.2666666666667, 
#>     drat > 4.165 & cyl <= 5 & hp <= 116.5 ~ 32.15, hp <= 77.5 & 
#>         drat <= 4.165 & cyl <= 5 & hp <= 116.5 ~ 24.4, disp <= 
#>         337.9 & hp <= 192.5 & qsec <= 18.14 & hp > 116.5 ~ 16.76, 
#>     disp > 337.9 & hp <= 192.5 & qsec <= 18.14 & hp > 116.5 ~ 
#>         19.2, drat <= 3.105 & hp > 192.5 & qsec <= 18.14 & hp > 
#>         116.5 ~ 10.4, drat > 3.105 & hp > 192.5 & qsec <= 18.14 & 
#>         hp > 116.5 ~ 13.975, disp <= 114.05 & hp > 77.5 & drat <= 
#>         4.165 & cyl <= 5 & hp <= 116.5 ~ 22.8, .default = 21.7))/5L
```

From there, the Tidy Eval formula can be used anywhere where it can be
operated. `tidypredict` provides three paths:

- Use directly inside `dplyr`, `mutate(iris, !! tidypredict_fit(model))`
- Use `tidypredict_to_column(model)` to a piped command set
- Use `tidypredict_to_sql(model)` to retrieve the SQL statement

## parsnip

`tidypredict` also supports `randomForest` model objects fitted via the
`parsnip` package.

``` r
library(parsnip)

parsnip_model <- rand_forest(mode = "regression", trees = 5) %>%
  set_engine("randomForest") %>%
  fit(mpg ~ ., data = mtcars)

tidypredict_fit(parsnip_model)
#> (case_when(wt <= 2.0675 & disp <= 107.6 ~ 30.5, wt > 2.0675 & 
#>     disp <= 107.6 ~ 32.4, drat > 4.175 & wt <= 3.16 & disp > 
#>     107.6 ~ 26, hp <= 96 & drat <= 4.175 & wt <= 3.16 & disp > 
#>     107.6 ~ 22.8, hp > 96 & drat <= 4.175 & wt <= 3.16 & disp > 
#>     107.6 ~ 20.925, qsec <= 18.6 & cyl <= 7 & wt > 3.16 & disp > 
#>     107.6 ~ 19.2, qsec > 18.6 & cyl <= 7 & wt > 3.16 & disp > 
#>     107.6 ~ 17.9, wt <= 3.4375 & hp <= 177.5 & cyl > 7 & wt > 
#>     3.16 & disp > 107.6 ~ 15.2, drat <= 3.14 & hp > 177.5 & cyl > 
#>     7 & wt > 3.16 & disp > 107.6 ~ 16.55, drat > 3.14 & hp > 
#>     177.5 & cyl > 7 & wt > 3.16 & disp > 107.6 ~ 15.05, drat <= 
#>     2.92 & wt > 3.4375 & hp <= 177.5 & cyl > 7 & wt > 3.16 & 
#>     disp > 107.6 ~ 15.5, .default = 18.8) + case_when(hp <= 79.5 ~ 
#>     29.8, disp <= 142.9 & cyl <= 7 & hp > 79.5 ~ 22.125, disp > 
#>     456 & cyl > 7 & hp > 79.5 ~ 10.4, hp <= 116.5 & disp > 142.9 & 
#>     cyl <= 7 & hp > 79.5 ~ 21.1333333333333, drat > 3.115 & disp <= 
#>     456 & cyl > 7 & hp > 79.5 ~ 15.34, qsec <= 18.6 & hp > 116.5 & 
#>     disp > 142.9 & cyl <= 7 & hp > 79.5 ~ 19.5, qsec > 18.6 & 
#>     hp > 116.5 & disp > 142.9 & cyl <= 7 & hp > 79.5 ~ 17.8, 
#>     drat <= 3.075 & drat <= 3.115 & disp <= 456 & cyl > 7 & hp > 
#>         79.5 ~ 15.725, .default = 19.2) + case_when(wt <= 3.3125 & 
#>     drat <= 3.75 ~ 22.05, wt <= 2.23 & drat > 3.75 ~ 29.6, drat <= 
#>     3.035 & wt > 3.3125 & drat <= 3.75 ~ 10.4, gear > 4.5 & wt > 
#>     2.23 & drat > 3.75 ~ 15.8, carb <= 2.5 & drat > 3.035 & wt > 
#>     3.3125 & drat <= 3.75 ~ 16.3666666666667, vs <= 0.5 & gear <= 
#>     4.5 & wt > 2.23 & drat > 3.75 ~ 21, hp > 290 & carb > 2.5 & 
#>     drat > 3.035 & wt > 3.3125 & drat <= 3.75 ~ 15, qsec <= 0 & 
#>     vs > 0.5 & gear <= 4.5 & wt > 2.23 & drat > 3.75 ~ 24.4, 
#>     qsec > 0 & vs > 0.5 & gear <= 4.5 & wt > 2.23 & drat > 3.75 ~ 
#>         22.8, disp <= 395 & hp <= 290 & carb > 2.5 & drat > 3.035 & 
#>         wt > 3.3125 & drat <= 3.75 ~ 13.68, .default = 14.7) + 
#>     case_when(wt > 2.26 & cyl <= 5 ~ 22.56, qsec <= 16.8 & wt <= 
#>         2.26 & cyl <= 5 ~ 26, qsec > 16.8 & wt <= 2.26 & cyl <= 
#>         5 ~ 31.5, qsec > 19.83 & disp <= 266.9 & cyl > 5 ~ 18.1, 
#>         qsec > 17.71 & disp > 266.9 & cyl > 5 ~ 12.8, hp <= 116.5 & 
#>             qsec <= 19.83 & disp <= 266.9 & cyl > 5 ~ 21.08, 
#>         hp > 116.5 & qsec <= 19.83 & disp <= 266.9 & cyl > 5 ~ 
#>             19.2, drat > 3.19 & qsec <= 17.71 & disp > 266.9 & 
#>             cyl > 5 ~ 15.25, drat <= 3.075 & drat <= 3.19 & qsec <= 
#>             17.71 & disp > 266.9 & cyl > 5 ~ 16.4, .default = 18.95) + 
#>     case_when(hp <= 80.5 ~ 29.9, gear > 4.5 & carb <= 2.5 & hp > 
#>         80.5 ~ 30.4, hp <= 192.5 & carb > 2.5 & hp > 80.5 ~ 15.725, 
#>         disp <= 182.9 & gear <= 4.5 & carb <= 2.5 & hp > 80.5 ~ 
#>             21.7, hp <= 222.5 & hp > 192.5 & carb > 2.5 & hp > 
#>             80.5 ~ 10.4, wt <= 3.3275 & disp > 182.9 & gear <= 
#>             4.5 & carb <= 2.5 & hp > 80.5 ~ 21.4, drat <= 3.88 & 
#>             hp > 222.5 & hp > 192.5 & carb > 2.5 & hp > 80.5 ~ 
#>             14.925, drat > 3.88 & hp > 222.5 & hp > 192.5 & carb > 
#>             2.5 & hp > 80.5 ~ 15.8, cyl <= 7 & wt > 3.3275 & 
#>             disp > 182.9 & gear <= 4.5 & carb <= 2.5 & hp > 80.5 ~ 
#>             18.1, disp <= 380 & cyl > 7 & wt > 3.3275 & disp > 
#>             182.9 & gear <= 4.5 & carb <= 2.5 & hp > 80.5 ~ 18.7, 
#>         .default = 19.2))/5L
```
