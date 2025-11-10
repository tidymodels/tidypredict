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
using the `iris` dataset:

``` r
library(dplyr)
library(tidypredict)
library(ranger)

model <- ranger(Species ~ ., data = iris, num.trees = 100)
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
#> 1      0         1          2          3  Petal.Width     1.75
#> 2      1         3          4          2 Petal.Length     2.45
#> 3      2         5          6          2 Petal.Length     4.85
#> 4      3        NA         NA         NA         <NA>       NA
#> 5      4         7          8          2 Petal.Length     5.40
#> 6      5        NA         NA         NA         <NA>       NA
#>   terminal prediction
#> 1    FALSE       <NA>
#> 2    FALSE       <NA>
#> 3    FALSE       <NA>
#> 4     TRUE     setosa
#> 5    FALSE       <NA>
#> 6     TRUE  virginica
```

The output from
[`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
is transformed into a `dplyr`, a.k.a Tidy Eval, formula. The entire
decision tree becomes one
[`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case_when.html)
statement

``` r
tidypredict_fit(model)[1]
#> `+`()
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

parsnip_model <- rand_forest(mode = "classification") %>%
  set_engine("ranger") %>%
  fit(Species ~ ., data = iris)

tidypredict_fit(parsnip_model)[[1]]
#> `+`
```
