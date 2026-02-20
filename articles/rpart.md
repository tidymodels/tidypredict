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
#> case_when((!is.na(cyl) & cyl > 5 | is.na(cyl) & !is.na(disp) & 
#>     disp > 142.9 | is.na(cyl) & is.na(disp) & !is.na(hp) & hp > 
#>     101 | is.na(cyl) & is.na(disp) & is.na(hp) & !is.na(wt) & 
#>     wt > 2.5425 | is.na(cyl) & is.na(disp) & is.na(hp) & is.na(wt) & 
#>     !is.na(qsec) & qsec <= 18.41 | is.na(cyl) & is.na(disp) & 
#>     is.na(hp) & is.na(wt) & is.na(qsec) & !is.na(vs) & vs <= 
#>     0.5 | is.na(cyl) & is.na(disp) & is.na(hp) & is.na(wt) & 
#>     is.na(qsec) & is.na(vs)) & (!is.na(hp) & hp > 192.5 | is.na(hp) & 
#>     !is.na(disp) & disp > 334 | is.na(hp) & is.na(disp) & !is.na(wt) & 
#>     wt > 4.66 | is.na(hp) & is.na(disp) & is.na(wt) & !is.na(qsec) & 
#>     qsec <= 15.455 | is.na(hp) & is.na(disp) & is.na(wt) & is.na(qsec) & 
#>     !is.na(carb) & carb > 3.5 | is.na(hp) & is.na(disp) & is.na(wt) & 
#>     is.na(qsec) & is.na(carb) & !is.na(gear) & gear > 4.5) ~ 
#>     13.4142857142857, (!is.na(cyl) & cyl > 5 | is.na(cyl) & !is.na(disp) & 
#>     disp > 142.9 | is.na(cyl) & is.na(disp) & !is.na(hp) & hp > 
#>     101 | is.na(cyl) & is.na(disp) & is.na(hp) & !is.na(wt) & 
#>     wt > 2.5425 | is.na(cyl) & is.na(disp) & is.na(hp) & is.na(wt) & 
#>     !is.na(qsec) & qsec <= 18.41 | is.na(cyl) & is.na(disp) & 
#>     is.na(hp) & is.na(wt) & is.na(qsec) & !is.na(vs) & vs <= 
#>     0.5 | is.na(cyl) & is.na(disp) & is.na(hp) & is.na(wt) & 
#>     is.na(qsec) & is.na(vs)) & (!is.na(hp) & hp <= 192.5 | is.na(hp) & 
#>     !is.na(disp) & disp <= 334 | is.na(hp) & is.na(disp) & !is.na(wt) & 
#>     wt <= 4.66 | is.na(hp) & is.na(disp) & is.na(wt) & !is.na(qsec) & 
#>     qsec > 15.455 | is.na(hp) & is.na(disp) & is.na(wt) & is.na(qsec) & 
#>     !is.na(carb) & carb <= 3.5 | is.na(hp) & is.na(disp) & is.na(wt) & 
#>     is.na(qsec) & is.na(carb) & !is.na(gear) & gear <= 4.5 | 
#>     is.na(hp) & is.na(disp) & is.na(wt) & is.na(qsec) & is.na(carb) & 
#>         is.na(gear)) ~ 18.2642857142857, .default = 26.6636363636364)
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
#> case_when(!is.na(Petal.Length) & Petal.Length <= 2.45 | is.na(Petal.Length) & 
#>     !is.na(Petal.Width) & Petal.Width <= 0.8 | is.na(Petal.Length) & 
#>     is.na(Petal.Width) & !is.na(Sepal.Length) & Sepal.Length <= 
#>     5.45 | is.na(Petal.Length) & is.na(Petal.Width) & is.na(Sepal.Length) & 
#>     !is.na(Sepal.Width) & Sepal.Width > 3.35 ~ "setosa", (!is.na(Petal.Length) & 
#>     Petal.Length > 2.45 | is.na(Petal.Length) & !is.na(Petal.Width) & 
#>     Petal.Width > 0.8 | is.na(Petal.Length) & is.na(Petal.Width) & 
#>     !is.na(Sepal.Length) & Sepal.Length > 5.45 | is.na(Petal.Length) & 
#>     is.na(Petal.Width) & is.na(Sepal.Length) & !is.na(Sepal.Width) & 
#>     Sepal.Width <= 3.35 | is.na(Petal.Length) & is.na(Petal.Width) & 
#>     is.na(Sepal.Length) & is.na(Sepal.Width)) & (!is.na(Petal.Width) & 
#>     Petal.Width <= 1.75 | is.na(Petal.Width) & !is.na(Petal.Length) & 
#>     Petal.Length <= 4.75 | is.na(Petal.Width) & is.na(Petal.Length) & 
#>     !is.na(Sepal.Length) & Sepal.Length <= 6.15 | is.na(Petal.Width) & 
#>     is.na(Petal.Length) & is.na(Sepal.Length) & !is.na(Sepal.Width) & 
#>     Sepal.Width <= 2.95 | is.na(Petal.Width) & is.na(Petal.Length) & 
#>     is.na(Sepal.Length) & is.na(Sepal.Width)) ~ "versicolor", 
#>     .default = "virginica")
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
#> case_when((!is.na(cyl) & cyl > 5 | is.na(cyl) & !is.na(disp) & 
#>     disp > 142.9 | is.na(cyl) & is.na(disp) & !is.na(hp) & hp > 
#>     101 | is.na(cyl) & is.na(disp) & is.na(hp) & !is.na(wt) & 
#>     wt > 2.5425 | is.na(cyl) & is.na(disp) & is.na(hp) & is.na(wt) & 
#>     !is.na(qsec) & qsec <= 18.41 | is.na(cyl) & is.na(disp) & 
#>     is.na(hp) & is.na(wt) & is.na(qsec) & !is.na(vs) & vs <= 
#>     0.5 | is.na(cyl) & is.na(disp) & is.na(hp) & is.na(wt) & 
#>     is.na(qsec) & is.na(vs)) & (!is.na(hp) & hp > 192.5 | is.na(hp) & 
#>     !is.na(disp) & disp > 334 | is.na(hp) & is.na(disp) & !is.na(wt) & 
#>     wt > 4.66 | is.na(hp) & is.na(disp) & is.na(wt) & !is.na(qsec) & 
#>     qsec <= 15.455 | is.na(hp) & is.na(disp) & is.na(wt) & is.na(qsec) & 
#>     !is.na(carb) & carb > 3.5 | is.na(hp) & is.na(disp) & is.na(wt) & 
#>     is.na(qsec) & is.na(carb) & !is.na(gear) & gear > 4.5) ~ 
#>     13.4142857142857, (!is.na(cyl) & cyl > 5 | is.na(cyl) & !is.na(disp) & 
#>     disp > 142.9 | is.na(cyl) & is.na(disp) & !is.na(hp) & hp > 
#>     101 | is.na(cyl) & is.na(disp) & is.na(hp) & !is.na(wt) & 
#>     wt > 2.5425 | is.na(cyl) & is.na(disp) & is.na(hp) & is.na(wt) & 
#>     !is.na(qsec) & qsec <= 18.41 | is.na(cyl) & is.na(disp) & 
#>     is.na(hp) & is.na(wt) & is.na(qsec) & !is.na(vs) & vs <= 
#>     0.5 | is.na(cyl) & is.na(disp) & is.na(hp) & is.na(wt) & 
#>     is.na(qsec) & is.na(vs)) & (!is.na(hp) & hp <= 192.5 | is.na(hp) & 
#>     !is.na(disp) & disp <= 334 | is.na(hp) & is.na(disp) & !is.na(wt) & 
#>     wt <= 4.66 | is.na(hp) & is.na(disp) & is.na(wt) & !is.na(qsec) & 
#>     qsec > 15.455 | is.na(hp) & is.na(disp) & is.na(wt) & is.na(qsec) & 
#>     !is.na(carb) & carb <= 3.5 | is.na(hp) & is.na(disp) & is.na(wt) & 
#>     is.na(qsec) & is.na(carb) & !is.na(gear) & gear <= 4.5 | 
#>     is.na(hp) & is.na(disp) & is.na(wt) & is.na(qsec) & is.na(carb) & 
#>         is.na(gear)) ~ 18.2642857142857, .default = 26.6636363636364)
```

## Categorical predictors

`rpart` handles categorical predictors natively. The generated formula
uses `%in%` for categorical splits:

``` r
mtcars2 <- mtcars
mtcars2$cyl <- factor(mtcars2$cyl)

model_cat <- rpart(mpg ~ cyl + wt + hp, data = mtcars2)
tidypredict_fit(model_cat)
#> case_when((!is.na(cyl) & cyl %in% c("6", "8") | is.na(cyl) & 
#>     !is.na(hp) & hp > 101 | is.na(cyl) & is.na(hp) & !is.na(wt) & 
#>     wt > 2.5425 | is.na(cyl) & is.na(hp) & is.na(wt)) & (!is.na(hp) & 
#>     hp > 192.5 | is.na(hp) & !is.na(wt) & wt > 4.66) ~ 13.4142857142857, 
#>     (!is.na(cyl) & cyl %in% c("6", "8") | is.na(cyl) & !is.na(hp) & 
#>         hp > 101 | is.na(cyl) & is.na(hp) & !is.na(wt) & wt > 
#>         2.5425 | is.na(cyl) & is.na(hp) & is.na(wt)) & (!is.na(hp) & 
#>         hp <= 192.5 | is.na(hp) & !is.na(wt) & wt <= 4.66 | is.na(hp) & 
#>         is.na(wt)) ~ 18.2642857142857, .default = 26.6636363636364)
```

## Surrogate splits

`rpart` uses surrogate splits to handle missing values during
prediction. When the primary split variable is missing, the model uses
surrogate variables (other variables that produce similar splits) to
route the observation. This behavior is controlled by the `usesurrogate`
parameter in
[`rpart.control()`](https://rdrr.io/pkg/rpart/man/rpart.control.html).
