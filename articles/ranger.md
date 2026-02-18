# Random Forest, using Ranger

| Function                                                                                                                                                                                                                                                       | Works |
|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔     |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)                                                                                                                                                             | ✗     |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)                                                                                                                                                                       | ✔     |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md)                                                     | ✗     |
| `parsnip`                                                                                                                                                                                                                                                      | ✔     |

## How it works

Here is a simple
[`ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md) model
using the `mtcars` dataset:

``` r
library(dplyr)
library(tidypredict)
library(ranger)

model <- ranger(mpg ~ ., data = mtcars, num.trees = 5, max.depth = 2)
```

## Under the hood

The parser is based on the output from the
[`ranger::treeInfo()`](http://imbs-hl.github.io/ranger/reference/treeInfo.md)
function. It will return as many decision paths as there are non-NA rows
in the `prediction` field.

``` r
treeInfo(model) %>%
  head()
#>   nodeID leftChild rightChild splitvarID splitvarName splitval
#> 1      0         1          2          8         gear     3.50
#> 2      1         3          4          2           hp   192.50
#> 3      2         5          6          4           wt     2.26
#> 4      3        NA         NA         NA         <NA>       NA
#> 5      4        NA         NA         NA         <NA>       NA
#> 6      5        NA         NA         NA         <NA>       NA
#>   terminal prediction
#> 1    FALSE         NA
#> 2    FALSE         NA
#> 3    FALSE         NA
#> 4     TRUE    17.4000
#> 5     TRUE    12.9000
#> 6     TRUE    29.4375
```

The output from
[`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
is transformed into a `dplyr`, a.k.a Tidy Eval, formula. Each decision
tree becomes one
[`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
statement, which are then combined.

``` r
tidypredict_fit(model)
#> case_when(hp <= 192.5 & gear <= 3.5 ~ 17.4, hp > 192.5 & gear <= 
#>     3.5 ~ 12.9, wt <= 2.26 & gear > 3.5 ~ 29.4375, .default = 21.3625) + 
#>     case_when(vs <= 0.5 & wt <= 2.455 ~ 26, vs > 0.5 & wt <= 
#>         2.455 ~ 31.8, gear <= 3.5 & wt > 2.455 ~ 14.25, .default = 19.0666666666667) + 
#>     case_when(hp <= 65.5 & disp <= 120.65 ~ 31.275, hp > 65.5 & 
#>         disp <= 120.65 ~ 26.6333333333333, wt <= 3.505 & disp > 
#>         120.65 ~ 19.9533333333333, .default = 14.6857142857143) + 
#>     case_when(disp <= 93.5 & cyl <= 5 ~ 30.625, disp > 93.5 & 
#>         cyl <= 5 ~ 22.32, cyl <= 7 & cyl > 5 ~ 18.78, .default = 15.7769230769231) + 
#>     case_when(disp <= 107.6 & cyl <= 5 ~ 31.1333333333333, disp > 
#>         107.6 & cyl <= 5 ~ 23.6625, hp <= 192.5 & cyl > 5 ~ 18.65, 
#>         .default = 12.8625)
```

From there, the Tidy Eval formula can be used anywhere where it can be
operated. `tidypredict` provides three paths:

- Use directly inside `dplyr`, `mutate(iris, !! tidypredict_fit(model))`
- Use `tidypredict_to_column(model)` to a piped command set
- Use `tidypredict_to_sql(model)` to retrieve the SQL statement

## parsnip

`tidypredict` also supports `ranger` model objects fitted via the
`parsnip` package.

``` r
library(parsnip)

parsnip_model <- rand_forest(mode = "regression", trees = 5) %>%
  set_engine("ranger", max.depth = 2) %>%
  fit(mpg ~ ., data = mtcars)

tidypredict_fit(parsnip_model)
#> case_when(disp <= 450 & vs <= 0.5 ~ 16.9684210526316, disp > 
#>     450 & vs <= 0.5 ~ 10.4, drat <= 4 & vs > 0.5 ~ 24.2, .default = 30.6857142857143) + 
#>     case_when(wt <= 2.3325 & hp <= 131.5 ~ 31.6666666666667, 
#>         wt > 2.3325 & hp <= 131.5 ~ 21.7571428571429, drat <= 
#>             3.035 & hp > 131.5 ~ 12.1, .default = 16.3153846153846) + 
#>     case_when(hp <= 78.5 & cyl <= 5 ~ 31.28, hp > 78.5 & cyl <= 
#>         5 ~ 24.4, wt <= 4.747 & cyl > 5 ~ 17.52, .default = 10.4) + 
#>     case_when(carb <= 2.5 & drat <= 3.615 ~ 17.5875, carb > 2.5 & 
#>         drat <= 3.615 ~ 13.4, wt <= 2.23 & drat > 3.615 ~ 28.8857142857143, 
#>         .default = 20.4285714285714) + case_when(wt <= 2.2775 & 
#>     disp <= 266.9 ~ 31.375, wt > 2.2775 & disp <= 266.9 ~ 22.4133333333333, 
#>     wt <= 4.747 & disp > 266.9 ~ 16.5818181818182, .default = 10.4)
```
