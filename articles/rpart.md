# Decision trees, using rpart

| Function                                                                                                                                                                                                                                                       | Works |
|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) |       |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)                                                                                                                                                             |       |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)                                                                                                                                                                       |       |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md)                                                     |       |
| `parsnip`                                                                                                                                                                                                                                                      |       |

## How it works

Here is a simple [`rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html)
model using the `mtcars` dataset:

``` r
library(dplyr)
library(tidypredict)
library(rpart)

model <- rpart(mpg ~ ., data = mtcars)
```

## Under the hood

The parser extracts the tree structure from the model’s `frame` and
`splits` components. It handles both numeric and categorical splits, as
well as surrogate splits for missing value handling.

``` r
model$frame |>
  head()
#>      var  n wt        dev     yval complexity ncompete nsurrogate
#> 1    cyl 32 32 1126.04719 20.09062 0.64312523        4          5
#> 2     hp 21 21  198.47238 16.64762 0.09748407        4          5
#> 4 <leaf>  7  7   28.82857 13.41429 0.01000000        0          0
#> 5 <leaf> 14 14   59.87214 18.26429 0.01000000        0          0
#> 3 <leaf> 11 11  203.38545 26.66364 0.01000000        0          0
```

The output from
[`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
is transformed into a `dplyr`, a.k.a Tidy Eval, formula. The decision
tree becomes a
[`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
statement.

``` r
tidypredict_fit(model)
#> case_when(cyl <= 5 ~ 26.6636363636364, .default = case_when(hp <= 
#>     192.5 ~ 18.2642857142857, .default = 13.4142857142857))
```

From there, the Tidy Eval formula can be used anywhere where it can be
operated. `tidypredict` provides three paths:

- Use directly inside `dplyr`,
  `mutate(mtcars, !! tidypredict_fit(model))`
- Use `tidypredict_to_column(model)` to a piped command set
- Use `tidypredict_to_sql(model)` to retrieve the SQL statement

## Classification

`rpart` classification models are also supported:

``` r
model_class <- rpart(Species ~ ., data = iris)
tidypredict_fit(model_class)
#> case_when(Petal.Length <= 2.45 ~ "setosa", .default = case_when(Petal.Width <= 
#>     1.75 ~ "versicolor", .default = "virginica"))
```

## parsnip

`tidypredict` also supports `rpart` model objects fitted via the
`parsnip` package.

``` r
library(parsnip)

parsnip_model <- decision_tree(mode = "regression") |>
  set_engine("rpart") |>
  fit(mpg ~ ., data = mtcars)

tidypredict_fit(parsnip_model)
#> case_when(cyl <= 5 ~ 26.6636363636364, .default = case_when(hp <= 
#>     192.5 ~ 18.2642857142857, .default = 13.4142857142857))
```

## Categorical predictors

`rpart` handles categorical predictors natively. The generated formula
uses `%in%` for categorical splits:

``` r
mtcars2 <- mtcars
mtcars2$cyl <- factor(mtcars2$cyl)

model_cat <- rpart(mpg ~ cyl + wt + hp, data = mtcars2)
tidypredict_fit(model_cat)
#> case_when(cyl %in% c("6", "8") ~ case_when(hp <= 192.5 ~ 18.2642857142857, 
#>     .default = 13.4142857142857), .default = 26.6636363636364)
```

## Surrogate splits

`rpart` uses surrogate splits to handle missing values during
prediction. When the primary split variable is missing, the model uses
surrogate variables (other variables that produce similar splits) to
route the observation. This behavior is controlled by the `usesurrogate`
parameter in
[`rpart.control()`](https://rdrr.io/pkg/rpart/man/rpart.control.html).
