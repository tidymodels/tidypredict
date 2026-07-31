# NaiveBayes models

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✗ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✗ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

[`klaR::NaiveBayes()`](https://rdrr.io/pkg/klaR/man/NaiveBayes.html)
fits naive Bayes classifiers. Predicting with such a model multiplies
the prior probability of each class by one conditional density per
predictor, and then normalizes those products into posterior
probabilities. Working on the log scale turns the products into sums,
which makes the posterior probabilities the softmax of the summed log
densities.

Numeric predictors contribute a normal log density, and categorical
predictors contribute a
[`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
lookup of the conditional probability of the observed level. Only
Gaussian densities can be expressed this way, so models fit with
`usekernel = TRUE` are not supported.

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

model <- klaR::NaiveBayes(Species ~ ., data = iris)
```

- Create the R formulas, one per class

  ``` r

  fit <- tidypredict_fit(model)
  names(fit)
  #> [1] "setosa"     "versicolor" "virginica"
  fit[["setosa"]]
  #> exp(-1.09861228866811 + (1.04273391328984 - ((Sepal.Length - 
  #>     5.006)^2/0.248497959183673)) + (0.970049249016581 - ((Sepal.Width - 
  #>     3.428)^2/0.287379591836735)) + (1.75063290136911 - ((Petal.Length - 
  #>     1.462)^2/0.0603183673469388)) + (2.25012937537181 - ((Petal.Width - 
  #>     0.246)^2/0.0222122448979592)))/(exp(-1.09861228866811 + (1.04273391328984 - 
  #>     ((Sepal.Length - 5.006)^2/0.248497959183673)) + (0.970049249016581 - 
  #>     ((Sepal.Width - 3.428)^2/0.287379591836735)) + (1.75063290136911 - 
  #>     ((Petal.Length - 1.462)^2/0.0603183673469388)) + (2.25012937537181 - 
  #>     ((Petal.Width - 0.246)^2/0.0222122448979592))) + exp(-1.09861228866811 + 
  #>     (0.661316888138019 - ((Sepal.Length - 5.936)^2/0.532865306122449)) + 
  #>     (1.15900478165984 - ((Sepal.Width - 2.77)^2/0.196938775510204)) + 
  #>     (0.755212012346146 - ((Petal.Length - 4.26)^2/0.441632653061225)) + 
  #>     (1.620738119938 - ((Petal.Width - 1.326)^2/0.0782122448979592))) + 
  #>     exp(-1.09861228866811 + (0.452746052315965 - ((Sepal.Length - 
  #>         6.588)^2/0.808685714285714)) + (1.13166256707153 - ((Sepal.Width - 
  #>         2.974)^2/0.208008163265306)) + (0.594398019628377 - ((Petal.Length - 
  #>         5.552)^2/0.609175510204082)) + (1.29225751662055 - ((Petal.Width - 
  #>         2.026)^2/0.150865306122449))))
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
  #> $ versicolor   <dbl> 2.981309e-18, 3.169312e-17, 2.367113e-18, 3.0696…
  #> $ virginica    <dbl> 2.152373e-25, 6.938030e-25, 7.240956e-26, 8.6906…
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

p_model <- naive_Bayes() %>%
  set_engine("klaR", usekernel = FALSE) %>%
  fit(Species ~ ., data = iris)
```

``` r

tidypredict_fit(p_model)[["virginica"]]
#> exp(-1.09861228866811 + (0.452746052315965 - ((Sepal.Length - 
#>     6.588)^2/0.808685714285714)) + (1.13166256707153 - ((Sepal.Width - 
#>     2.974)^2/0.208008163265306)) + (0.594398019628377 - ((Petal.Length - 
#>     5.552)^2/0.609175510204082)) + (1.29225751662055 - ((Petal.Width - 
#>     2.026)^2/0.150865306122449)))/(exp(-1.09861228866811 + (1.04273391328984 - 
#>     ((Sepal.Length - 5.006)^2/0.248497959183673)) + (0.970049249016581 - 
#>     ((Sepal.Width - 3.428)^2/0.287379591836735)) + (1.75063290136911 - 
#>     ((Petal.Length - 1.462)^2/0.0603183673469388)) + (2.25012937537181 - 
#>     ((Petal.Width - 0.246)^2/0.0222122448979592))) + exp(-1.09861228866811 + 
#>     (0.661316888138019 - ((Sepal.Length - 5.936)^2/0.532865306122449)) + 
#>     (1.15900478165984 - ((Sepal.Width - 2.77)^2/0.196938775510204)) + 
#>     (0.755212012346146 - ((Petal.Length - 4.26)^2/0.441632653061225)) + 
#>     (1.620738119938 - ((Petal.Width - 1.326)^2/0.0782122448979592))) + 
#>     exp(-1.09861228866811 + (0.452746052315965 - ((Sepal.Length - 
#>         6.588)^2/0.808685714285714)) + (1.13166256707153 - ((Sepal.Width - 
#>         2.974)^2/0.208008163265306)) + (0.594398019628377 - ((Petal.Length - 
#>         5.552)^2/0.609175510204082)) + (1.29225751662055 - ((Petal.Width - 
#>         2.026)^2/0.150865306122449))))
```

## Parse model spec

Here is an example of the model spec:

``` r

pm <- parse_model(model)
str(pm, 2)
#> List of 3
#>  $ general    :List of 4
#>   ..$ model    : chr "NaiveBayes"
#>   ..$ version  : num 2
#>   ..$ type     : chr "naive_bayes"
#>   ..$ threshold: num 0.001
#>  $ classes    : chr [1:3] "setosa" "versicolor" "virginica"
#>  $ class_terms:List of 3
#>   ..$ :List of 2
#>   ..$ :List of 2
#>   ..$ :List of 2
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_naive_bayes" "list"
```
