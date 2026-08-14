# lda models

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✗ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✗ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

[`MASS::lda()`](https://rdrr.io/pkg/MASS/man/lda.html) fits linear
discriminant analysis models. Predicting with such a model projects the
predictors onto the discriminant space and compares the result against
each class centroid. That path is linear in the predictors, so it
collapses into one linear predictor per outcome class, and the posterior
probabilities are the softmax of those linear predictors.

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

model <- MASS::lda(Species ~ ., data = iris)
```

- Create the R formulas, one per class

  ``` r

  fit <- tidypredict_fit(model)
  names(fit)
  #> [1] "setosa"     "versicolor" "virginica"
  fit[["setosa"]]
  #> 1/(1 + exp(-2.02197415376308 + (Sepal.Length * -1.53119918820706) + 
  #>     (Sepal.Width * -4.37604347769129) + (Petal.Length * 4.69566530591248) + 
  #>     (Petal.Width * 3.06258538965303) - (-15.477836726795 + (Sepal.Length * 
  #>     6.31475845867536) + (Sepal.Width * 12.1393171806029) + (Petal.Length * 
  #>     -16.9464246511956) + (Petal.Width * -20.770054592318))) + 
  #>     exp(-33.5376867395709 + (Sepal.Length * -4.7835592704683) + 
  #>         (Sepal.Width * -7.76327370291161) + (Petal.Length * 12.2507593452831) + 
  #>         (Petal.Width * 17.707469202665) - (-15.477836726795 + 
  #>         (Sepal.Length * 6.31475845867536) + (Sepal.Width * 12.1393171806029) + 
  #>         (Petal.Length * -16.9464246511956) + (Petal.Width * -20.770054592318))))
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
  #> $ versicolor   <dbl> 3.896358e-22, 7.217970e-18, 1.463849e-19, 1.2685…
  #> $ virginica    <dbl> 2.611168e-42, 5.042143e-37, 4.675932e-39, 3.5666…
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

p_model <- discrim_linear() %>%
  set_engine("MASS") %>%
  fit(Species ~ ., data = iris)
```

``` r

tidypredict_fit(p_model)[["virginica"]]
#> 1/(exp(-15.477836726795 + (Sepal.Length * 6.31475845867536) + 
#>     (Sepal.Width * 12.1393171806029) + (Petal.Length * -16.9464246511956) + 
#>     (Petal.Width * -20.770054592318) - (-33.5376867395709 + (Sepal.Length * 
#>     -4.7835592704683) + (Sepal.Width * -7.76327370291161) + (Petal.Length * 
#>     12.2507593452831) + (Petal.Width * 17.707469202665))) + exp(-2.02197415376308 + 
#>     (Sepal.Length * -1.53119918820706) + (Sepal.Width * -4.37604347769129) + 
#>     (Petal.Length * 4.69566530591248) + (Petal.Width * 3.06258538965303) - 
#>     (-33.5376867395709 + (Sepal.Length * -4.7835592704683) + 
#>         (Sepal.Width * -7.76327370291161) + (Petal.Length * 12.2507593452831) + 
#>         (Petal.Width * 17.707469202665))) + 1)
```

## Parse model spec

Here is an example of the model spec:

``` r

pm <- parse_model(model)
str(pm, 2)
#> List of 3
#>  $ general    :List of 4
#>   ..$ model  : chr "lda"
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
