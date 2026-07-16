# Random Forest - partykit

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✔ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✔ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

Only regression models are supported. Classification
[`cforest()`](https://rdrr.io/pkg/partykit/man/cforest.html) models rely
on a voting mechanism that cannot be expressed as a single formula.

## How it works

Here is a simple
[`cforest()`](https://rdrr.io/pkg/partykit/man/cforest.html) model using
the `mtcars` dataset:

``` r

library(dplyr)
library(tidypredict)
library(partykit)

model <- cforest(mpg ~ wt + cyl, data = mtcars, ntree = 5)
```

## Under the hood

Each tree in the forest is a `partykit` party tree. `tidypredict` turns
every tree into a nested
[`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
statement using each terminal node’s in-bag weighted mean, then averages
the trees. This matches the default
[`predict()`](https://rdrr.io/r/stats/predict.html) behavior, which
scales the per-tree weights before aggregating.

``` r

tidypredict_fit(model)
#> (case_when(cyl <= 6 ~ 22.08, .default = 15.17) + case_when(wt <= 
#>     2.78 ~ 27.5428571428571, .default = 17.1384615384615) + case_when(wt <= 
#>     3.15 ~ 23.7625, .default = 16.5083333333333) + case_when(wt <= 
#>     2.78 ~ 26.6285714285714, .default = 16.9076923076923) + case_when(wt <= 
#>     2.465 ~ 28.3857142857143, .default = 16.7769230769231))/5L
```

From there, the Tidy Eval formula can be used anywhere where it can be
operated. `tidypredict` provides three paths:

- Use directly inside `dplyr`,
  `mutate(mtcars, !!tidypredict_fit(model))`
- Use `tidypredict_to_column(model)` to a piped command set
- Use `tidypredict_sql(model, con)` to retrieve the SQL statement

## parsnip

`tidypredict` also supports `cforest` model objects fitted via the
`parsnip` package with the `"partykit"` engine, which is provided by the
`bonsai` package.

``` r

library(bonsai)
library(parsnip)

parsnip_model <- rand_forest(mode = "regression", trees = 5) %>%
  set_engine("partykit") %>%
  fit(mpg ~ wt + cyl, data = mtcars)

tidypredict_fit(parsnip_model)
#> (case_when(cyl <= 4 ~ 28.275, .default = 16.1) + case_when(wt <= 
#>     3.215 ~ 25.2555555555556, .default = 16.0272727272727) + 
#>     case_when(cyl <= 4 ~ 25.775, .default = 16.675) + case_when(wt <= 
#>     3.15 ~ 25.1555555555556, .default = 16.1636363636364) + case_when(wt <= 
#>     2.875 ~ 25.9714285714286, .default = 17))/5L
```
