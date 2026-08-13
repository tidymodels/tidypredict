# NaiveBayes models

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✗ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✗ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

[`klaR::NaiveBayes()`](https://rdrr.io/pkg/klaR/man/NaiveBayes.html) and
[`naivebayes::naive_bayes()`](https://majkamichal.github.io/naivebayes/reference/naive_bayes.html)
fit naive Bayes classifiers. Predicting with such a model multiplies the
prior probability of each class by one conditional density per
predictor, and then normalizes those products into posterior
probabilities. Working on the log scale turns the products into sums,
which makes the posterior probabilities the softmax of the summed log
densities.

Numeric predictors contribute a normal log density, and categorical
predictors contribute a
[`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
lookup of the conditional probability of the observed level.
[`naivebayes::naive_bayes()`](https://majkamichal.github.io/naivebayes/reference/naive_bayes.html)
additionally supports Poisson densities for integer predictors when fit
with `usepoisson = TRUE`. Kernel density estimates cannot be expressed
this way, so models fit with `usekernel = TRUE` are not supported.

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
  #> exp(-1.09861228866811 + ifelse(is.na(Sepal.Length), 0, 1.04273391328984 - 
  #>     ((Sepal.Length - 5.006)^2/0.248497959183673)) + ifelse(is.na(Sepal.Width), 
  #>     0, 0.970049249016581 - ((Sepal.Width - 3.428)^2/0.287379591836735)) + 
  #>     ifelse(is.na(Petal.Length), 0, 1.75063290136911 - ((Petal.Length - 
  #>         1.462)^2/0.0603183673469388)) + ifelse(is.na(Petal.Width), 
  #>     0, 2.25012937537181 - ((Petal.Width - 0.246)^2/0.0222122448979592)))/(exp(-1.09861228866811 + 
  #>     ifelse(is.na(Sepal.Length), 0, 1.04273391328984 - ((Sepal.Length - 
  #>         5.006)^2/0.248497959183673)) + ifelse(is.na(Sepal.Width), 
  #>     0, 0.970049249016581 - ((Sepal.Width - 3.428)^2/0.287379591836735)) + 
  #>     ifelse(is.na(Petal.Length), 0, 1.75063290136911 - ((Petal.Length - 
  #>         1.462)^2/0.0603183673469388)) + ifelse(is.na(Petal.Width), 
  #>     0, 2.25012937537181 - ((Petal.Width - 0.246)^2/0.0222122448979592))) + 
  #>     exp(-1.09861228866811 + ifelse(is.na(Sepal.Length), 0, 0.661316888138019 - 
  #>         ((Sepal.Length - 5.936)^2/0.532865306122449)) + ifelse(is.na(Sepal.Width), 
  #>         0, 1.15900478165984 - ((Sepal.Width - 2.77)^2/0.196938775510204)) + 
  #>         ifelse(is.na(Petal.Length), 0, 0.755212012346146 - ((Petal.Length - 
  #>             4.26)^2/0.441632653061225)) + ifelse(is.na(Petal.Width), 
  #>         0, 1.620738119938 - ((Petal.Width - 1.326)^2/0.0782122448979592))) + 
  #>     exp(-1.09861228866811 + ifelse(is.na(Sepal.Length), 0, 0.452746052315965 - 
  #>         ((Sepal.Length - 6.588)^2/0.808685714285714)) + ifelse(is.na(Sepal.Width), 
  #>         0, 1.13166256707153 - ((Sepal.Width - 2.974)^2/0.208008163265306)) + 
  #>         ifelse(is.na(Petal.Length), 0, 0.594398019628377 - ((Petal.Length - 
  #>             5.552)^2/0.609175510204082)) + ifelse(is.na(Petal.Width), 
  #>         0, 1.29225751662055 - ((Petal.Width - 2.026)^2/0.150865306122449))))
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

### Extreme values of a numeric predictor

There is one case where
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
does not reproduce [`predict()`](https://rdrr.io/r/stats/predict.html),
and it is deliberately left in place.

Both packages evaluate each normal density and then replace any density
that came out as exactly zero with their `threshold` argument, `0.001`
by default. A normal density only reaches zero by underflowing the
smallest number a double can hold, which takes a value roughly 38
standard deviations from that class’s mean. `tidypredict` works on the
log scale throughout, where such a value is an ordinary negative number
and nothing underflows, so the substitution never happens.

The substitution is drastic when it fires. Consider an iris with a
`Sepal.Length` of 20, well beyond the largest in the data:

``` r

outlier <- iris[1, ]
outlier$Sepal.Length <- 20

predict(model, outlier)$posterior
#>   setosa    versicolor     virginica
#> 1      1 7.500428e-176 9.059222e-118
```

`setosa` has the narrowest spread of the three classes, so its density
is the first to underflow. Replacing it with `0.001` makes it enormous
next to the other two, which are around `1e-162` and `1e-97` but were
computed honestly, so the class that fits the value worst is the one
that wins. This happens silently unless the value is extreme enough for
*every* class to underflow, which is the only case either package warns
about. `tidypredict` instead continues on the log scale and reports the
ordering the fitted model implies:

``` r

sapply(tidypredict_fit(model), \(f) rlang::eval_tidy(f, outlier))
#>      setosa  versicolor   virginica 
#> 0.00000e+00 8.27933e-59 1.00000e+00
```

Reproducing the substitution is not possible on the log scale in any
case. Testing `exp(log_density) == 0` instead of `density == 0` picks
out a different set of values: over 200,000 draws from the band where
the underflow happens the two tests disagree on about 1,000 of them,
because the two routes round differently in the denormal range. A SQL
backend, working in its own floating point, would disagree again in a
third way.

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
#> exp(-1.09861228866811 + ifelse(is.na(Sepal.Length), 0, 0.452746052315965 - 
#>     ((Sepal.Length - 6.588)^2/0.808685714285714)) + ifelse(is.na(Sepal.Width), 
#>     0, 1.13166256707153 - ((Sepal.Width - 2.974)^2/0.208008163265306)) + 
#>     ifelse(is.na(Petal.Length), 0, 0.594398019628377 - ((Petal.Length - 
#>         5.552)^2/0.609175510204082)) + ifelse(is.na(Petal.Width), 
#>     0, 1.29225751662055 - ((Petal.Width - 2.026)^2/0.150865306122449)))/(exp(-1.09861228866811 + 
#>     ifelse(is.na(Sepal.Length), 0, 1.04273391328984 - ((Sepal.Length - 
#>         5.006)^2/0.248497959183673)) + ifelse(is.na(Sepal.Width), 
#>     0, 0.970049249016581 - ((Sepal.Width - 3.428)^2/0.287379591836735)) + 
#>     ifelse(is.na(Petal.Length), 0, 1.75063290136911 - ((Petal.Length - 
#>         1.462)^2/0.0603183673469388)) + ifelse(is.na(Petal.Width), 
#>     0, 2.25012937537181 - ((Petal.Width - 0.246)^2/0.0222122448979592))) + 
#>     exp(-1.09861228866811 + ifelse(is.na(Sepal.Length), 0, 0.661316888138019 - 
#>         ((Sepal.Length - 5.936)^2/0.532865306122449)) + ifelse(is.na(Sepal.Width), 
#>         0, 1.15900478165984 - ((Sepal.Width - 2.77)^2/0.196938775510204)) + 
#>         ifelse(is.na(Petal.Length), 0, 0.755212012346146 - ((Petal.Length - 
#>             4.26)^2/0.441632653061225)) + ifelse(is.na(Petal.Width), 
#>         0, 1.620738119938 - ((Petal.Width - 1.326)^2/0.0782122448979592))) + 
#>     exp(-1.09861228866811 + ifelse(is.na(Sepal.Length), 0, 0.452746052315965 - 
#>         ((Sepal.Length - 6.588)^2/0.808685714285714)) + ifelse(is.na(Sepal.Width), 
#>         0, 1.13166256707153 - ((Sepal.Width - 2.974)^2/0.208008163265306)) + 
#>         ifelse(is.na(Petal.Length), 0, 0.594398019628377 - ((Petal.Length - 
#>             5.552)^2/0.609175510204082)) + ifelse(is.na(Petal.Width), 
#>         0, 1.29225751662055 - ((Petal.Width - 2.026)^2/0.150865306122449))))
```

## `naivebayes::naive_bayes()`

The `naivebayes` package is supported in the same way:

``` r

nb_model <- naivebayes::naive_bayes(Species ~ ., data = iris)

nb_fit <- tidypredict_fit(nb_model)
nb_fit[["setosa"]]
#> exp(-1.09861228866811 + ifelse(is.na(Sepal.Length), 0, 1.04273391328984 - 
#>     ((Sepal.Length - 5.006)^2/0.248497959183673)) + ifelse(is.na(Sepal.Width), 
#>     0, 0.970049249016581 - ((Sepal.Width - 3.428)^2/0.287379591836735)) + 
#>     ifelse(is.na(Petal.Length), 0, 1.75063290136911 - ((Petal.Length - 
#>         1.462)^2/0.0603183673469388)) + ifelse(is.na(Petal.Width), 
#>     0, 2.25012937537181 - ((Petal.Width - 0.246)^2/0.0222122448979592)))/(exp(-1.09861228866811 + 
#>     ifelse(is.na(Sepal.Length), 0, 1.04273391328984 - ((Sepal.Length - 
#>         5.006)^2/0.248497959183673)) + ifelse(is.na(Sepal.Width), 
#>     0, 0.970049249016581 - ((Sepal.Width - 3.428)^2/0.287379591836735)) + 
#>     ifelse(is.na(Petal.Length), 0, 1.75063290136911 - ((Petal.Length - 
#>         1.462)^2/0.0603183673469388)) + ifelse(is.na(Petal.Width), 
#>     0, 2.25012937537181 - ((Petal.Width - 0.246)^2/0.0222122448979592))) + 
#>     exp(-1.09861228866811 + ifelse(is.na(Sepal.Length), 0, 0.661316888138019 - 
#>         ((Sepal.Length - 5.936)^2/0.532865306122449)) + ifelse(is.na(Sepal.Width), 
#>         0, 1.15900478165984 - ((Sepal.Width - 2.77)^2/0.196938775510204)) + 
#>         ifelse(is.na(Petal.Length), 0, 0.755212012346146 - ((Petal.Length - 
#>             4.26)^2/0.441632653061225)) + ifelse(is.na(Petal.Width), 
#>         0, 1.620738119938 - ((Petal.Width - 1.326)^2/0.0782122448979592))) + 
#>     exp(-1.09861228866811 + ifelse(is.na(Sepal.Length), 0, 0.452746052315965 - 
#>         ((Sepal.Length - 6.588)^2/0.808685714285714)) + ifelse(is.na(Sepal.Width), 
#>         0, 1.13166256707153 - ((Sepal.Width - 2.974)^2/0.208008163265306)) + 
#>         ifelse(is.na(Petal.Length), 0, 0.594398019628377 - ((Petal.Length - 
#>             5.552)^2/0.609175510204082)) + ifelse(is.na(Petal.Width), 
#>         0, 1.29225751662055 - ((Petal.Width - 2.026)^2/0.150865306122449))))
```

``` r

nb_probs <- sapply(nb_fit, \(f) rlang::eval_tidy(f, iris))
all.equal(
  unname(nb_probs),
  unname(predict(nb_model, iris[names(nb_model$tables)], type = "prob"))
)
#> [1] TRUE
```

The `"naivebayes"` parsnip engine works too, as long as
`usekernel = FALSE`:

``` r

nb_p_model <- naive_Bayes() %>%
  set_engine("naivebayes", usekernel = FALSE) %>%
  fit(Species ~ ., data = iris)

tidypredict_fit(nb_p_model)[["virginica"]]
#> exp(-1.09861228866811 + ifelse(is.na(Sepal.Length), 0, 0.452746052315965 - 
#>     ((Sepal.Length - 6.588)^2/0.808685714285714)) + ifelse(is.na(Sepal.Width), 
#>     0, 1.13166256707153 - ((Sepal.Width - 2.974)^2/0.208008163265306)) + 
#>     ifelse(is.na(Petal.Length), 0, 0.594398019628377 - ((Petal.Length - 
#>         5.552)^2/0.609175510204082)) + ifelse(is.na(Petal.Width), 
#>     0, 1.29225751662055 - ((Petal.Width - 2.026)^2/0.150865306122449)))/(exp(-1.09861228866811 + 
#>     ifelse(is.na(Sepal.Length), 0, 1.04273391328984 - ((Sepal.Length - 
#>         5.006)^2/0.248497959183673)) + ifelse(is.na(Sepal.Width), 
#>     0, 0.970049249016581 - ((Sepal.Width - 3.428)^2/0.287379591836735)) + 
#>     ifelse(is.na(Petal.Length), 0, 1.75063290136911 - ((Petal.Length - 
#>         1.462)^2/0.0603183673469388)) + ifelse(is.na(Petal.Width), 
#>     0, 2.25012937537181 - ((Petal.Width - 0.246)^2/0.0222122448979592))) + 
#>     exp(-1.09861228866811 + ifelse(is.na(Sepal.Length), 0, 0.661316888138019 - 
#>         ((Sepal.Length - 5.936)^2/0.532865306122449)) + ifelse(is.na(Sepal.Width), 
#>         0, 1.15900478165984 - ((Sepal.Width - 2.77)^2/0.196938775510204)) + 
#>         ifelse(is.na(Petal.Length), 0, 0.755212012346146 - ((Petal.Length - 
#>             4.26)^2/0.441632653061225)) + ifelse(is.na(Petal.Width), 
#>         0, 1.620738119938 - ((Petal.Width - 1.326)^2/0.0782122448979592))) + 
#>     exp(-1.09861228866811 + ifelse(is.na(Sepal.Length), 0, 0.452746052315965 - 
#>         ((Sepal.Length - 6.588)^2/0.808685714285714)) + ifelse(is.na(Sepal.Width), 
#>         0, 1.13166256707153 - ((Sepal.Width - 2.974)^2/0.208008163265306)) + 
#>         ifelse(is.na(Petal.Length), 0, 0.594398019628377 - ((Petal.Length - 
#>             5.552)^2/0.609175510204082)) + ifelse(is.na(Petal.Width), 
#>         0, 1.29225751662055 - ((Petal.Width - 2.026)^2/0.150865306122449))))
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
