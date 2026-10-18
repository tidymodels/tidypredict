# fda models

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✗ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✗ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

[`mda::fda()`](https://rdrr.io/pkg/mda/man/fda.html) fits flexible
discriminant analysis models. Predicting with such a model runs the
predictors through a regression fit, projects the result onto the
discriminant variates, and turns the distance to each class centroid
into a posterior probability. When the regression fit is linear in the
predictors, the whole path collapses into one linear predictor per
outcome class and the posterior probabilities are the softmax of those
linear predictors.

That restricts support to the two linear regression methods:
[`mda::polyreg()`](https://rdrr.io/pkg/mda/man/polyreg.html) (the
default) with `degree = 1`, and
[`mda::gen.ridge()`](https://rdrr.io/pkg/mda/man/gen.ridge.html), which
is what
[`discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.html)
uses. Higher polynomial degrees and the
[`mda::mars()`](https://rdrr.io/pkg/mda/man/mars.html) and
[`mda::bruto()`](https://rdrr.io/pkg/mda/man/bruto.html) methods are not
supported, and neither are the mixture models from
[`mda::mda()`](https://rdrr.io/pkg/mda/man/mda.html).

Because these models predict one probability per outcome class,
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
returns a *named list* of expressions, one for each class, rather than a
single expression. Since the output is a list,
[`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)
and
[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
are not supported.

## `tidypredict_` functions

``` r

model <- mda::fda(Species ~ ., data = iris)
```

- Create the R formulas, one per class

  ``` r

  fit <- tidypredict_fit(model)
  names(fit)
  #> [1] "setosa"     "versicolor" "virginica"
  fit[["setosa"]]
  #> 1/(1 + exp(-2.04081827345901 + (Sepal.Length * -1.5624481512317) + 
  #>     (Sepal.Width * -4.4653504874401) + (Petal.Length * 4.7914952101148) + 
  #>     (Petal.Width * 3.12508713229903) - (-15.7712902867569 + (Sepal.Length * 
  #>     6.44363108028105) + (Sepal.Width * 12.3870583475541) + (Petal.Length * 
  #>     -17.2922700522406) + (Petal.Width * -21.1939332574676))) + 
  #>     exp(-34.1997086671406 + (Sepal.Length * -4.88118292904935) + 
  #>         (Sepal.Width * -7.92170786011395) + (Petal.Length * 12.5007748421258) + 
  #>         (Petal.Width * 18.0688461251685) - (-15.7712902867569 + 
  #>         (Sepal.Length * 6.44363108028105) + (Sepal.Width * 12.3870583475541) + 
  #>         (Petal.Length * -17.2922700522406) + (Petal.Width * -21.1939332574676))))
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
  #> $ versicolor   <dbl> 1.424733e-22, 3.225424e-18, 6.041136e-20, 6.0100…
  #> $ virginica    <dbl> 3.699975e-43, 9.159088e-38, 7.720058e-40, 7.0670…
  ```

- Confirm that the results match the model’s
  [`predict()`](https://rdrr.io/r/stats/predict.html) results

  ``` r

  probs <- sapply(fit, \(f) rlang::eval_tidy(f, iris))
  all.equal(unname(probs), unname(predict(model, iris, type = "posterior")))
  #> [1] TRUE
  ```

## parsnip

`parsnip` fitted models are also supported by `tidypredict`:

``` r

library(parsnip)
library(discrim)

p_model <- discrim_linear(penalty = 1) %>%
  set_engine("mda") %>%
  fit(Species ~ ., data = iris)
```

``` r

tidypredict_fit(p_model)[["virginica"]]
#> 1/(exp(-11.9575128198864 + (Sepal.Length * 5.80316342827787) + 
#>     (Sepal.Width * 10.8028995796442) + (Petal.Length * -16.5217691675752) + 
#>     (Petal.Width * -17.3381617005552) - (-33.2183837927999 + 
#>     (Sepal.Length * -4.41029984072516) + (Sepal.Width * -6.75173810807851) + 
#>     (Petal.Length * 12.0386389001351) + (Petal.Width * 14.7915580954521))) + 
#>     exp(-2.27975663240439 + (Sepal.Length * -1.39286358755271) + 
#>         (Sepal.Width * -4.05116147156568) + (Petal.Length * 4.48313026744015) + 
#>         (Petal.Width * 2.54660360510309) - (-33.2183837927999 + 
#>         (Sepal.Length * -4.41029984072516) + (Sepal.Width * -6.75173810807851) + 
#>         (Petal.Length * 12.0386389001351) + (Petal.Width * 14.7915580954521))) + 
#>     1)
```

## Parse model spec

Here is an example of the model spec:

``` r

pm <- parse_model(model)
str(pm, 2)
#> List of 3
#>  $ general    :List of 4
#>   ..$ model  : chr "fda"
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
