# sda models

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✗ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✗ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

[`sda::sda()`](https://rdrr.io/pkg/sda/man/sda.html) fits shrinkage
discriminant analysis models, including the diagonal variant
(`diagonal = TRUE`). Predicting with such a model is a softmax over one
linear predictor per outcome class, so
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
returns a *named list* of expressions, one for each class, rather than a
single expression. Since the output is a list,
[`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)
and
[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
are not supported.

Only the features that survive shrinkage appear in the fitted model, and
the generated expressions reference just those.

## `tidypredict_` functions

``` r

model <- sda::sda(as.matrix(iris[1:4]), iris$Species, verbose = FALSE)
```

- Create the R formulas, one per class

  ``` r

  fit <- tidypredict_fit(model)
  names(fit)
  #> [1] "setosa"     "versicolor" "virginica"
  fit[["setosa"]]
  #> 1/(1 + exp(-2.17403913501331 + (Sepal.Length * -1.38327577793292) + 
  #>     (Sepal.Width * -4.20634050168113) + (Petal.Length * 4.57106120700436) + 
  #>     (Petal.Width * 2.49705634843865) - (-13.1507094677662 + (Sepal.Length * 
  #>     5.73266109594555) + (Sepal.Width * 11.3604293693887) + (Petal.Length * 
  #>     -16.8084846238169) + (Petal.Width * -16.6240117338793))) + 
  #>     exp(-32.2372116751408 + (Sepal.Length * -4.34938531801259) + 
  #>         (Sepal.Width * -7.15408886770751) + (Petal.Length * 12.2374234168125) + 
  #>         (Petal.Width * 14.1269553854406) - (-13.1507094677662 + 
  #>         (Sepal.Length * 5.73266109594555) + (Sepal.Width * 11.3604293693887) + 
  #>         (Petal.Length * -16.8084846238169) + (Petal.Width * -16.6240117338793))))
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
  #> $ setosa       <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
  #> $ versicolor   <dbl> 1.009018e-20, 1.005223e-16, 2.186404e-18, 1.5199…
  #> $ virginica    <dbl> 3.704434e-39, 2.916177e-34, 2.957542e-36, 1.7209…
  ```

- Confirm that the results match the model’s
  [`predict()`](https://rdrr.io/r/stats/predict.html) results

  ``` r

  probs <- sapply(fit, \(f) rlang::eval_tidy(f, iris))
  posterior <- predict(model, as.matrix(iris[1:4]), verbose = FALSE)
  all.equal(unname(probs), unname(posterior$posterior))
  #> [1] "Mean relative difference: 4.24967e-08"
  ```

`sda()` rounds its posterior probabilities with
[`zapsmall()`](https://rdrr.io/r/base/zapsmall.html), so expect
agreement to about seven decimal places rather than exactly.

## parsnip

`parsnip` fitted models are also supported by `tidypredict`:

``` r

library(parsnip)
library(discrim)

p_model <- discrim_linear() %>%
  set_engine("sda") %>%
  fit(Species ~ ., data = iris)
```

``` r

tidypredict_fit(p_model)[["virginica"]]
#> 1/(exp(-13.1507094677662 + (Sepal.Length * 5.73266109594555) + 
#>     (Sepal.Width * 11.3604293693887) + (Petal.Length * -16.8084846238169) + 
#>     (Petal.Width * -16.6240117338793) - (-32.2372116751408 + 
#>     (Sepal.Length * -4.34938531801259) + (Sepal.Width * -7.15408886770751) + 
#>     (Petal.Length * 12.2374234168125) + (Petal.Width * 14.1269553854406))) + 
#>     exp(-2.17403913501331 + (Sepal.Length * -1.38327577793292) + 
#>         (Sepal.Width * -4.20634050168113) + (Petal.Length * 4.57106120700436) + 
#>         (Petal.Width * 2.49705634843865) - (-32.2372116751408 + 
#>         (Sepal.Length * -4.34938531801259) + (Sepal.Width * -7.15408886770751) + 
#>         (Petal.Length * 12.2374234168125) + (Petal.Width * 14.1269553854406))) + 
#>     1)
```

`sda()` is fit from a numeric matrix, so a model fit directly can only
refer to the matrix columns it was given. Categorical predictors
therefore work through the `parsnip` interface, which keeps the formula
around and lets the dummy columns be written in terms of the original
factors:

``` r

cars <- transform(mtcars, cyl = factor(cyl), gear = factor(gear))

c_model <- discrim_linear() %>%
  set_engine("sda") %>%
  fit(cyl ~ mpg + gear + disp, data = cars)

tidypredict_fit(c_model)[["8"]]
#> 1/(exp(-5.90478040863942 + (mpg * 0.450539182424721) + (ifelse(gear == 
#>     "4", 1, 0) * 0.12033481185876) + (ifelse(gear == "5", 1, 
#>     0) * -0.326397688436069) + (disp * -0.0361970033802602) - 
#>     (-9.69338951858337 + (mpg * -0.300898442298614) + (ifelse(gear == 
#>         "4", 1, 0) * -0.288744001063268) + (ifelse(gear == "5", 
#>         1, 0) * 0.346792280462245) + (disp * 0.0491641125277205))) + 
#>     exp(4.82341069251291 + (mpg * -0.145463097730621) + (ifelse(gear == 
#>         "4", 1, 0) * 0.189561232611208) + (ifelse(gear == "5", 
#>         1, 0) * -0.0358060646840255) + (disp * -0.0157055813201664) - 
#>         (-9.69338951858337 + (mpg * -0.300898442298614) + (ifelse(gear == 
#>             "4", 1, 0) * -0.288744001063268) + (ifelse(gear == 
#>             "5", 1, 0) * 0.346792280462245) + (disp * 0.0491641125277205))) + 
#>     1)
```

## Parse model spec

Here is an example of the model spec:

``` r

pm <- parse_model(model)
str(pm, 2)
#> List of 3
#>  $ general    :List of 4
#>   ..$ model  : chr "sda"
#>   ..$ version: num 2
#>   ..$ type   : chr "multiclass_regression"
#>   ..$ family : chr "multinomial"
#>  $ classes    : chr [1:3] "setosa" "versicolor" "virginica"
#>  $ class_terms:List of 3
#>   ..$ :List of 5
#>   ..$ :List of 5
#>   ..$ :List of 5
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_multiclass_regression" "list"
```
