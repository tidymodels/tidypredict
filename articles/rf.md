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
#> (case_when(carb <= 1.5 ~ case_when(gear <= 3.5 ~ 18.1, .default = case_when(hp <= 
#>     65.5 ~ 33.9, .default = case_when(qsec <= 19.185 ~ 27.3, 
#>     .default = 32.4))), .default = case_when(disp <= 221.7 ~ 
#>     case_when(wt <= 3.295 ~ 21.82, .default = 19.2), .default = case_when(wt <= 
#>     4.66 ~ case_when(qsec <= 16.96 ~ 14.94, .default = case_when(hp <= 
#>     162.5 ~ 15.2, .default = case_when(carb <= 2.5 ~ 19.2, .default = 16.34))), 
#>     .default = 10.4))) + case_when(hp <= 95 ~ case_when(hp <= 
#>     65.5 ~ 31.5666666666667, .default = case_when(wt <= 2.23 ~ 
#>     26.325, .default = 22.8)), .default = case_when(hp <= 190 ~ 
#>     case_when(drat <= 3.385 ~ case_when(hp <= 130 ~ 19.2, .default = 17.025), 
#>         .default = case_when(gear <= 4.5 ~ case_when(vs <= 0.5 ~ 
#>             21, .default = 21.5), .default = 19.7)), .default = case_when(qsec <= 
#>     17.62 ~ 14.325, .default = 10.4))) + case_when(disp <= 142.9 ~ 
#>     case_when(qsec <= 18.755 ~ 22.8, .default = 28.74), .default = case_when(wt <= 
#>     3.49 ~ case_when(disp <= 163.8 ~ 20.1333333333333, .default = case_when(gear <= 
#>     3.5 ~ 19.2, .default = 17.4)), .default = case_when(qsec <= 
#>     17.71 ~ case_when(hp <= 212.5 ~ case_when(drat <= 2.915 ~ 
#>     15.5, .default = 18.25), .default = case_when(carb <= 6 ~ 
#>     13.7, .default = 15)), .default = 10.4))) + case_when(am <= 
#>     0.5 ~ case_when(cyl <= 5 ~ 22.55, .default = case_when(hp <= 
#>     192.5 ~ case_when(hp <= 136.5 ~ 18.5, .default = case_when(drat <= 
#>     3.11 ~ 17.075, .default = 17.3)), .default = 13.84)), .default = case_when(wt <= 
#>     2.2775 ~ 29.78, .default = 18.42)) + case_when(hp <= 116.5 ~ 
#>     case_when(cyl <= 5 ~ case_when(drat <= 4.165 ~ case_when(hp <= 
#>         77.5 ~ 24.4, .default = case_when(disp <= 114.05 ~ 22.8, 
#>         .default = 21.7)), .default = 32.15), .default = 21.1), 
#>     .default = case_when(qsec <= 18.14 ~ case_when(hp <= 192.5 ~ 
#>         case_when(disp <= 337.9 ~ 16.76, .default = 19.2), .default = case_when(drat <= 
#>         3.105 ~ 10.4, .default = 13.975)), .default = 18.2666666666667)))/5
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
#> (case_when(disp <= 107.6 ~ case_when(wt <= 2.0675 ~ 30.5, .default = 32.4), 
#>     .default = case_when(wt <= 3.16 ~ case_when(drat <= 4.175 ~ 
#>         case_when(hp <= 96 ~ 22.8, .default = 20.925), .default = 26), 
#>         .default = case_when(cyl <= 7 ~ case_when(qsec <= 18.6 ~ 
#>             19.2, .default = 17.9), .default = case_when(hp <= 
#>             177.5 ~ case_when(wt <= 3.4375 ~ 15.2, .default = case_when(drat <= 
#>             2.92 ~ 15.5, .default = 18.8)), .default = case_when(drat <= 
#>             3.14 ~ 16.55, .default = 15.05))))) + case_when(hp <= 
#>     79.5 ~ 29.8, .default = case_when(cyl <= 7 ~ case_when(disp <= 
#>     142.9 ~ 22.125, .default = case_when(hp <= 116.5 ~ 21.1333333333333, 
#>     .default = case_when(qsec <= 18.6 ~ 19.5, .default = 17.8))), 
#>     .default = case_when(disp <= 456 ~ case_when(drat <= 3.115 ~ 
#>         case_when(drat <= 3.075 ~ 15.725, .default = 19.2), .default = 15.34), 
#>         .default = 10.4))) + case_when(drat <= 3.75 ~ case_when(wt <= 
#>     3.3125 ~ 22.05, .default = case_when(drat <= 3.035 ~ 10.4, 
#>     .default = case_when(carb <= 2.5 ~ 16.3666666666667, .default = case_when(hp <= 
#>         290 ~ case_when(disp <= 395 ~ 13.68, .default = 14.7), 
#>         .default = 15)))), .default = case_when(wt <= 2.23 ~ 
#>     29.6, .default = case_when(gear <= 4.5 ~ case_when(vs <= 
#>     0.5 ~ 21, .default = case_when(qsec <= 0 ~ 24.4, .default = 22.8)), 
#>     .default = 15.8))) + case_when(cyl <= 5 ~ case_when(wt <= 
#>     2.26 ~ case_when(qsec <= 16.8 ~ 26, .default = 31.5), .default = 22.56), 
#>     .default = case_when(disp <= 266.9 ~ case_when(qsec <= 19.83 ~ 
#>         case_when(hp <= 116.5 ~ 21.08, .default = 19.2), .default = 18.1), 
#>         .default = case_when(qsec <= 17.71 ~ case_when(drat <= 
#>             3.19 ~ case_when(drat <= 3.075 ~ 16.4, .default = 18.95), 
#>             .default = 15.25), .default = 12.8))) + case_when(hp <= 
#>     80.5 ~ 29.9, .default = case_when(carb <= 2.5 ~ case_when(gear <= 
#>     4.5 ~ case_when(disp <= 182.9 ~ 21.7, .default = case_when(wt <= 
#>     3.3275 ~ 21.4, .default = case_when(cyl <= 7 ~ 18.1, .default = case_when(disp <= 
#>     380 ~ 18.7, .default = 19.2)))), .default = 30.4), .default = case_when(hp <= 
#>     192.5 ~ 15.725, .default = case_when(hp <= 222.5 ~ 10.4, 
#>     .default = case_when(drat <= 3.88 ~ 14.925, .default = 15.8))))))/5
```
