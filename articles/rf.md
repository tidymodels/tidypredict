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
model using the `iris` dataset:

``` r
library(dplyr)
library(tidypredict)
library(randomForest)

model <- randomForest(Species ~ ., data = iris, ntree = 100, proximity = TRUE)
```

## Under the hood

The parser is based on the output from the
[`randomForest::getTree()`](https://rdrr.io/pkg/randomForest/man/getTree.html)
function. It will return as many decision paths as there are non-NA rows
in the `prediction` field.

``` r
getTree(model, labelVar = TRUE) %>%
  head()
#>   left daughter right daughter    split var split point status
#> 1             2              3 Petal.Length        2.50      1
#> 2             0              0         <NA>        0.00     -1
#> 3             4              5 Petal.Length        5.05      1
#> 4             6              7  Petal.Width        1.90      1
#> 5             0              0         <NA>        0.00     -1
#> 6             8              9 Sepal.Length        4.95      1
#>   prediction
#> 1       <NA>
#> 2     setosa
#> 3       <NA>
#> 4       <NA>
#> 5  virginica
#> 6       <NA>
```

The output from
[`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
is transformed into a `dplyr`, a.k.a Tidy Eval, formula. The entire
decision tree becomes one
[`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case_when.html)
statement

``` r
tidypredict_fit(model)[1]
#> `/`()
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

parsnip_model <- rand_forest(mode = "classification") %>%
  set_engine("randomForest") %>%
  fit(Species ~ ., data = iris)

tidypredict_fit(parsnip_model)[[1]]
#> `/`
```
