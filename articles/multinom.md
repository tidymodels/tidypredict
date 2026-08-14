# multinom models

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✗ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✗ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

[`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) fits
multinomial log-linear models. Because these models predict one
probability per outcome class,
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
returns a *named list* of expressions, one for each class, rather than a
single expression. The expressions implement the softmax over the
per-class linear predictors, with the first level of the outcome acting
as the reference class.

Since the output is a list,
[`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)
and
[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
are not supported.

## `tidypredict_` functions

``` r

library(nnet)

model <- multinom(Species ~ ., data = iris, trace = FALSE)
```

- Create the R formulas, one per class

  ``` r

  fit <- tidypredict_fit(model)
  names(fit)
  #> [1] "setosa"     "versicolor" "virginica"
  fit[["setosa"]]
  #> 1/(1 + exp(18.6903742569359 + (Sepal.Length * -5.458424007004) + 
  #>     (Sepal.Width * -8.70740085056792) + (Petal.Length * 14.2447701274975) + 
  #>     (Petal.Width * -3.09768387034398) - 0) + exp(-23.8362760290472 + 
  #>     (Sepal.Length * -7.92363397245864) + (Sepal.Width * -15.3707689334118) + 
  #>     (Petal.Length * 23.6597792430121) + (Petal.Width * 15.1353005479763) - 
  #>     0))
  ```

- Add the predictions to the original table

  ``` r

  library(dplyr)

  iris %>%
    mutate(!!!tidypredict_fit(model)) %>%
    glimpse()
  #> Rows: 150
  #> Columns: 8
  #> $ Sepal.Length <dbl> 5.1, 4.9, 4.7, 4.6, 5.0, 5.4, 4.6, 5.0, 4.4, 4.9…
  #> $ Sepal.Width  <dbl> 3.5, 3.0, 3.2, 3.1, 3.6, 3.9, 3.4, 3.4, 2.9, 3.1…
  #> $ Petal.Length <dbl> 1.4, 1.4, 1.3, 1.5, 1.4, 1.7, 1.4, 1.5, 1.4, 1.5…
  #> $ Petal.Width  <dbl> 0.2, 0.2, 0.2, 0.2, 0.2, 0.4, 0.3, 0.2, 0.2, 0.1…
  #> $ Species      <fct> setosa, setosa, setosa, setosa, setosa, setosa, …
  #> $ setosa       <dbl> 1.0000000, 0.9999996, 1.0000000, 0.9999968, 1.00…
  #> $ versicolor   <dbl> 1.526406e-09, 3.536476e-07, 4.443506e-08, 3.1639…
  #> $ virginica    <dbl> 2.716417e-36, 2.883729e-32, 6.103424e-34, 7.1170…
  ```

- Confirm that the results match the model’s
  [`predict()`](https://rdrr.io/r/stats/predict.html) results

  ``` r

  probs <- sapply(fit, \(f) rlang::eval_tidy(f, iris))
  all.equal(unname(probs), unname(predict(model, iris, type = "probs")))
  #> [1] TRUE
  ```

## parsnip

`parsnip` fitted models are also supported by `tidypredict`:

``` r

library(parsnip)

p_model <- multinom_reg() %>%
  set_engine("nnet") %>%
  fit(Species ~ ., data = iris)
```

``` r

tidypredict_fit(p_model)[["virginica"]]
#> 1/(exp(0 - (-23.8362760290472 + (Sepal.Length * -7.92363397245864) + 
#>     (Sepal.Width * -15.3707689334118) + (Petal.Length * 23.6597792430121) + 
#>     (Petal.Width * 15.1353005479763))) + exp(18.6903742569359 + 
#>     (Sepal.Length * -5.458424007004) + (Sepal.Width * -8.70740085056792) + 
#>     (Petal.Length * 14.2447701274975) + (Petal.Width * -3.09768387034398) - 
#>     (-23.8362760290472 + (Sepal.Length * -7.92363397245864) + 
#>         (Sepal.Width * -15.3707689334118) + (Petal.Length * 23.6597792430121) + 
#>         (Petal.Width * 15.1353005479763))) + 1)
```

## Parse model spec

Here is an example of the model spec:

``` r

pm <- parse_model(model)
str(pm, 2)
#> List of 3
#>  $ general    :List of 4
#>   ..$ model  : chr "multinom"
#>   ..$ version: num 2
#>   ..$ type   : chr "multiclass_regression"
#>   ..$ family : chr "multinomial"
#>  $ classes    : chr [1:3] "setosa" "versicolor" "virginica"
#>  $ class_terms:List of 3
#>   ..$ :List of 1
#>   ..$ :List of 5
#>   ..$ :List of 5
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_multiclass_regression" "list"
```
