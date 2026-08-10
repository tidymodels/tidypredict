# qda models

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✗ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✗ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

[`MASS::qda()`](https://rdrr.io/pkg/MASS/man/qda.html) fits quadratic
discriminant analysis models. Unlike
[`MASS::lda()`](https://rdrr.io/pkg/MASS/man/lda.html), each class gets
its own covariance estimate, so the class scores are quadratic rather
than linear in the predictors: one intercept, one coefficient per
predictor, and one coefficient per pair of predictors. The posterior
probabilities are the softmax of those class scores.

Because these models predict one probability per outcome class,
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
returns a *named list* of expressions, one for each class, rather than a
single expression. Since the output is a list,
[`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)
and
[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
are not supported.

## `tidypredict_` functions

Note that `MASS` is used with `::` below rather than attached, because
attaching it would mask
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html).

``` r

model <- MASS::qda(Species ~ ., data = iris)
```

- Create the R formulas, one per class

  ``` r

  fit <- tidypredict_fit(model)
  names(fit)
  #> [1] "setosa"     "versicolor" "virginica"
  fit[["setosa"]]
  #> exp(-113.714590795937 + (Sepal.Length * 44.5528812152965) + (Sepal.Width * 
  #>     -7.61595839316377) + (Petal.Length * 33.559537097271) + (Petal.Width * 
  #>     -31.2558694597319) + (Sepal.Length * Sepal.Length * -9.47171939291025) + 
  #>     (Sepal.Length * Sepal.Width * 12.4048261564855) + (Sepal.Length * 
  #>     Petal.Length * 4.50020653857199) + (Sepal.Length * Petal.Width * 
  #>     4.77612732803425) + (Sepal.Width * Sepal.Width * -7.78527008659406) + 
  #>     (Sepal.Width * Petal.Length * -1.1110791368753) + (Sepal.Width * 
  #>     Petal.Width * 2.1040978276337) + (Petal.Length * Petal.Length * 
  #>     -19.3881020632003) + (Petal.Length * Petal.Width * 17.9350353034304) + 
  #>     (Petal.Width * Petal.Width * -53.0229530714453))/(exp(-113.714590795937 + 
  #>     (Sepal.Length * 44.5528812152965) + (Sepal.Width * -7.61595839316377) + 
  #>     (Petal.Length * 33.559537097271) + (Petal.Width * -31.2558694597319) + 
  #>     (Sepal.Length * Sepal.Length * -9.47171939291025) + (Sepal.Length * 
  #>     Sepal.Width * 12.4048261564855) + (Sepal.Length * Petal.Length * 
  #>     4.50020653857199) + (Sepal.Length * Petal.Width * 4.77612732803425) + 
  #>     (Sepal.Width * Sepal.Width * -7.78527008659406) + (Sepal.Width * 
  #>     Petal.Length * -1.1110791368753) + (Sepal.Width * Petal.Width * 
  #>     2.1040978276337) + (Petal.Length * Petal.Length * -19.3881020632003) + 
  #>     (Petal.Length * Petal.Width * 17.9350353034304) + (Petal.Width * 
  #>     Petal.Width * -53.0229530714453)) + exp(-68.4372876863761 + 
  #>     (Sepal.Length * 18.0128645045466) + (Sepal.Width * 15.9607000492876) + 
  #>     (Petal.Length * 3.26878502392694) + (Petal.Width * -14.7125574674066) + 
  #>     (Sepal.Length * Sepal.Length * -4.7513818825332) + (Sepal.Length * 
  #>     Sepal.Width * 3.67621661105371) + (Sepal.Length * Petal.Length * 
  #>     8.63171191375034) + (Sepal.Length * Petal.Width * -6.45450343914608) + 
  #>     (Sepal.Width * Sepal.Width * -9.85548321295218) + (Sepal.Width * 
  #>     Petal.Length * -2.11602239677401) + (Sepal.Width * Petal.Width * 
  #>     19.4803247040045) + (Petal.Length * Petal.Length * -9.90187886398409) + 
  #>     (Petal.Length * Petal.Width * 26.9372270107537) + (Petal.Width * 
  #>     Petal.Width * -43.6223969131695)) + exp(-67.7090772023947 + 
  #>     (Sepal.Length * 7.37247478397155) + (Sepal.Width * 13.2452613006359) + 
  #>     (Petal.Length * 6.23406948373776) + (Petal.Width * 9.66197608445423) + 
  #>     (Sepal.Length * Sepal.Length * -5.26693339970326) + (Sepal.Length * 
  #>     Sepal.Width * 3.47972623582807) + (Sepal.Length * Petal.Length * 
  #>     9.96014594453995) + (Sepal.Length * Petal.Width * -1.78815223046388) + 
  #>     (Sepal.Width * Sepal.Width * -7.93772112409139) + (Sepal.Width * 
  #>     Petal.Length * -1.10268870214298) + (Sepal.Width * Petal.Width * 
  #>     8.47285053214324) + (Petal.Length * Petal.Length * -6.70291026519806) + 
  #>     (Petal.Length * Petal.Width * 2.8909184691834) + (Petal.Width * 
  #>     Petal.Width * -9.65702517612499)))
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
  #> $ versicolor   <dbl> 4.918517e-26, 7.655808e-19, 1.552279e-21, 8.3003…
  #> $ virginica    <dbl> 2.981541e-41, 1.311032e-34, 3.380440e-36, 8.5418…
  ```

- Confirm that the results match the model’s
  [`predict()`](https://rdrr.io/r/stats/predict.html) results

  ``` r

  probs <- sapply(fit, \(f) rlang::eval_tidy(f, iris))
  all.equal(unname(probs), unname(predict(model, iris)$posterior))
  #> [1] TRUE
  ```

## parsnip

`parsnip` fitted models are also supported by `tidypredict`:

``` r

library(parsnip)
library(discrim)

p_model <- discrim_quad() %>%
  set_engine("MASS") %>%
  fit(Species ~ ., data = iris)
```

``` r

tidypredict_fit(p_model)[["virginica"]]
#> exp(-67.7090772023947 + (Sepal.Length * 7.37247478397155) + (Sepal.Width * 
#>     13.2452613006359) + (Petal.Length * 6.23406948373776) + (Petal.Width * 
#>     9.66197608445423) + (Sepal.Length * Sepal.Length * -5.26693339970326) + 
#>     (Sepal.Length * Sepal.Width * 3.47972623582807) + (Sepal.Length * 
#>     Petal.Length * 9.96014594453995) + (Sepal.Length * Petal.Width * 
#>     -1.78815223046388) + (Sepal.Width * Sepal.Width * -7.93772112409139) + 
#>     (Sepal.Width * Petal.Length * -1.10268870214298) + (Sepal.Width * 
#>     Petal.Width * 8.47285053214324) + (Petal.Length * Petal.Length * 
#>     -6.70291026519806) + (Petal.Length * Petal.Width * 2.8909184691834) + 
#>     (Petal.Width * Petal.Width * -9.65702517612499))/(exp(-113.714590795937 + 
#>     (Sepal.Length * 44.5528812152965) + (Sepal.Width * -7.61595839316377) + 
#>     (Petal.Length * 33.559537097271) + (Petal.Width * -31.2558694597319) + 
#>     (Sepal.Length * Sepal.Length * -9.47171939291025) + (Sepal.Length * 
#>     Sepal.Width * 12.4048261564855) + (Sepal.Length * Petal.Length * 
#>     4.50020653857199) + (Sepal.Length * Petal.Width * 4.77612732803425) + 
#>     (Sepal.Width * Sepal.Width * -7.78527008659406) + (Sepal.Width * 
#>     Petal.Length * -1.1110791368753) + (Sepal.Width * Petal.Width * 
#>     2.1040978276337) + (Petal.Length * Petal.Length * -19.3881020632003) + 
#>     (Petal.Length * Petal.Width * 17.9350353034304) + (Petal.Width * 
#>     Petal.Width * -53.0229530714453)) + exp(-68.4372876863761 + 
#>     (Sepal.Length * 18.0128645045466) + (Sepal.Width * 15.9607000492876) + 
#>     (Petal.Length * 3.26878502392694) + (Petal.Width * -14.7125574674066) + 
#>     (Sepal.Length * Sepal.Length * -4.7513818825332) + (Sepal.Length * 
#>     Sepal.Width * 3.67621661105371) + (Sepal.Length * Petal.Length * 
#>     8.63171191375034) + (Sepal.Length * Petal.Width * -6.45450343914608) + 
#>     (Sepal.Width * Sepal.Width * -9.85548321295218) + (Sepal.Width * 
#>     Petal.Length * -2.11602239677401) + (Sepal.Width * Petal.Width * 
#>     19.4803247040045) + (Petal.Length * Petal.Length * -9.90187886398409) + 
#>     (Petal.Length * Petal.Width * 26.9372270107537) + (Petal.Width * 
#>     Petal.Width * -43.6223969131695)) + exp(-67.7090772023947 + 
#>     (Sepal.Length * 7.37247478397155) + (Sepal.Width * 13.2452613006359) + 
#>     (Petal.Length * 6.23406948373776) + (Petal.Width * 9.66197608445423) + 
#>     (Sepal.Length * Sepal.Length * -5.26693339970326) + (Sepal.Length * 
#>     Sepal.Width * 3.47972623582807) + (Sepal.Length * Petal.Length * 
#>     9.96014594453995) + (Sepal.Length * Petal.Width * -1.78815223046388) + 
#>     (Sepal.Width * Sepal.Width * -7.93772112409139) + (Sepal.Width * 
#>     Petal.Length * -1.10268870214298) + (Sepal.Width * Petal.Width * 
#>     8.47285053214324) + (Petal.Length * Petal.Length * -6.70291026519806) + 
#>     (Petal.Length * Petal.Width * 2.8909184691834) + (Petal.Width * 
#>     Petal.Width * -9.65702517612499)))
```

## Parse model spec

Here is an example of the model spec:

``` r

pm <- parse_model(model)
str(pm, 2)
#> List of 3
#>  $ general    :List of 4
#>   ..$ model  : chr "qda"
#>   ..$ version: num 2
#>   ..$ type   : chr "multiclass_regression"
#>   ..$ family : chr "multinomial"
#>  $ classes    : chr [1:3] "setosa" "versicolor" "virginica"
#>  $ class_terms:List of 3
#>   ..$ :List of 15
#>   ..$ :List of 15
#>   ..$ :List of 15
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_multiclass_regression" "list"
```
