# sparsediscrim models

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✗ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✗ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

`sparsediscrim` fits regularized linear discriminant analysis models.
The four supported functions are `lda_diag()`, `lda_shrink_mean()`,
`lda_shrink_cov()`, and `lda_emp_bayes_eigen()`, which are the models
behind the `"sparsediscrim"` engine of
[`discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.html)
(`regularization_method` values `"diagonal"`, `"shrink_mean"`,
`"shrink_cov"`, and `"min_distance"`).

All four share the same structure: one mean vector and prior per outcome
class, plus a single pooled covariance shared by every class, and they
only differ in how that covariance is regularized. The class posterior
probabilities are therefore a softmax over one linear predictor per
class, so
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
returns a *named list* of expressions, one for each class, rather than a
single expression. Since the output is a list,
[`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)
and
[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
are not supported.

## `tidypredict_` functions

``` r

model <- sparsediscrim::lda_diag(Species ~ ., data = iris)
```

- Create the R formulas, one per class

  ``` r

  fit <- tidypredict_fit(model)
  names(fit)
  #> [1] "setosa"     "versicolor" "virginica"
  fit[["setosa"]]
  #> exp(-107.930781085437 + (Sepal.Length * 19.2754940163568) + (Sepal.Width * 
  #>     30.314821365405) + (Petal.Length * 8.05580657248022) + (Petal.Width * 
  #>     5.99356787837443))/(exp(-107.930781085437 + (Sepal.Length * 
  #>     19.2754940163568) + (Sepal.Width * 30.314821365405) + (Petal.Length * 
  #>     8.05580657248022) + (Petal.Width * 5.99356787837443)) + exp(-174.280588008602 + 
  #>     (Sepal.Length * 22.8564387696952) + (Sepal.Width * 24.4959320834807) + 
  #>     (Petal.Length * 23.4731436380067) + (Petal.Width * 32.3067927102622)) + 
  #>     exp(-258.692783489496 + (Sepal.Length * 25.3669505752614) + 
  #>         (Sepal.Width * 26.2999646268129) + (Petal.Length * 30.5922285160124) + 
  #>         (Petal.Width * 49.3616606568561)))
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
  #> $ versicolor   <dbl> 8.524398e-19, 7.641570e-18, 2.495496e-19, 6.8152…
  #> $ virginica    <dbl> 2.418205e-41, 5.323668e-41, 7.407018e-43, 5.4569…
  ```

- Confirm that the results match the model’s
  [`predict()`](https://rdrr.io/r/stats/predict.html) results

  ``` r

  probs <- sapply(fit, \(f) rlang::eval_tidy(f, iris))
  posterior <- predict(model, iris, type = "prob")
  all.equal(unname(probs), unname(as.matrix(posterior)))
  #> [1] TRUE
  ```

Note that [`predict()`](https://rdrr.io/r/stats/predict.html) with
`type = "prob"` is not usable for `lda_emp_bayes_eigen()`
(`regularization_method = "min_distance"`) models: `sparsediscrim`
builds a rank-deficient covariance for them and returns `NaN` for every
row. The expressions that
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
generates use the model’s own pseudo-inverse of that covariance, so they
agree with the model’s `type = "class"` predictions.

## parsnip

`parsnip` fitted models are also supported by `tidypredict`:

``` r

library(parsnip)
library(discrim)

p_model <- discrim_linear(regularization_method = "shrink_mean") %>%
  set_engine("sparsediscrim") %>%
  fit(Species ~ ., data = iris)
```

``` r

tidypredict_fit(p_model)[["virginica"]]
#> exp(-258.712244691324 + (Sepal.Length * 25.3634171269089) + (Sepal.Width * 
#>     26.3045842550727) + (Petal.Length * 30.5894466999551) + (Petal.Width * 
#>     49.3835915687182))/(exp(-107.926997081959 + (Sepal.Length * 
#>     19.2748057606989) + (Sepal.Width * 30.314250317313) + (Petal.Length * 
#>     8.05623454310998) + (Petal.Width * 5.99760378610459)) + exp(-174.286289444853 + 
#>     (Sepal.Length * 22.8540964892416) + (Sepal.Width * 24.4977601426385) + 
#>     (Petal.Length * 23.4721691446234) + (Petal.Width * 32.3208860491096)) + 
#>     exp(-258.712244691324 + (Sepal.Length * 25.3634171269089) + 
#>         (Sepal.Width * 26.3045842550727) + (Petal.Length * 30.5894466999551) + 
#>         (Petal.Width * 49.3835915687182)))
```

These models are fit from a numeric matrix, so a model fit with the
`x`/`y` interface can only refer to the matrix columns it was given.
Categorical predictors work through the formula interface and through
`parsnip`, both of which keep the formula around and let the dummy
columns be written in terms of the original factors:

``` r

cars <- transform(mtcars, vs = factor(vs), gear = factor(gear))

c_model <- discrim_linear() %>%
  set_engine("sparsediscrim") %>%
  fit(vs ~ mpg + gear + disp, data = cars)

tidypredict_fit(c_model)[["1"]]
#> exp(-19.1256693330579 + (mpg * 1.24829943936813) + (ifelse(gear == 
#>     "4", 1, 0) * 4.93150684931507) + (ifelse(gear == "5", 1, 
#>     0) * 0.565815324165029) + (disp * 0.0179710473492101))/(exp(-14.23117559104 + 
#>     (mpg * 0.844665676493137) + (ifelse(gear == "4", 1, 0) * 
#>     0.767123287671233) + (ifelse(gear == "5", 1, 0) * 1.76031434184676) + 
#>     (disp * 0.0416724011574301)) + exp(-19.1256693330579 + (mpg * 
#>     1.24829943936813) + (ifelse(gear == "4", 1, 0) * 4.93150684931507) + 
#>     (ifelse(gear == "5", 1, 0) * 0.565815324165029) + (disp * 
#>     0.0179710473492101)))
```

## Parse model spec

Here is an example of the model spec:

``` r

pm <- parse_model(model)
str(pm, 2)
#> List of 3
#>  $ general    :List of 4
#>   ..$ model  : chr "lda_diag"
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
