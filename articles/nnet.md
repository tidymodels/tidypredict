# nnet models

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✔ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✔ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

[`nnet::nnet()`](https://rdrr.io/pkg/nnet/man/nnet.html) fits
feed-forward neural networks with a single hidden layer. Each hidden
unit is the weighted sum of the predictors passed through the logistic
squashing function, and the output units are the weighted sums of the
hidden units, left unsquashed when the network was fit with
`linout = TRUE`.

Classification models predict one probability per outcome class, so
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
returns a *named list* of expressions rather than a single expression.
For those models
[`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)
and
[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
are not supported.

## `tidypredict_` functions

``` r

library(nnet)

set.seed(100)
model <- nnet(mpg ~ wt + hp, data = mtcars, size = 3, linout = TRUE, trace = FALSE)
```

- Create the R formula

  ``` r

  tidypredict_fit(model)
  #> 9.63217435895367 + case_when(-0.255509663289361 + wt * -0.309449576415116 + 
  #>     hp * 0.949425509730813 < -15 ~ 0, -0.255509663289361 + wt * 
  #>     -0.309449576415116 + hp * 0.949425509730813 > 15 ~ 1, .default = 1/(1 + 
  #>     exp(-(-0.255509663289361 + wt * -0.309449576415116 + hp * 
  #>         0.949425509730813)))) * 10.1885081177867 + case_when(-0.385719808388316 + 
  #>     wt * 0.522807343827803 + hp * 20.0552277393738 < -15 ~ 0, 
  #>     -0.385719808388316 + wt * 0.522807343827803 + hp * 20.0552277393738 > 
  #>         15 ~ 1, .default = 1/(1 + exp(-(-0.385719808388316 + 
  #>         wt * 0.522807343827803 + hp * 20.0552277393738)))) * 
  #>     0.269942523260361 + case_when(0.42285989779505 + wt * -0.212906496087752 + 
  #>     hp * -0.880839556387573 < -15 ~ 0, 0.42285989779505 + wt * 
  #>     -0.212906496087752 + hp * -0.880839556387573 > 15 ~ 1, .default = 1/(1 + 
  #>     exp(-(0.42285989779505 + wt * -0.212906496087752 + hp * -0.880839556387573)))) * 
  #>     10.480592173546
  ```

- Add the predictions to the original table

  ``` r

  library(dplyr)

  mtcars %>%
    tidypredict_to_column(model) %>%
    glimpse()
  #> Rows: 32
  #> Columns: 12
  #> $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19…
  #> $ cyl  <dbl> 6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4,…
  #> $ disp <dbl> 160.0, 160.0, 108.0, 258.0, 360.0, 225.0, 360.0, 146.7, …
  #> $ hp   <dbl> 110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123, 180,…
  #> $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.…
  #> $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, …
  #> $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20.00, …
  #> $ vs   <dbl> 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1,…
  #> $ am   <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,…
  #> $ gear <dbl> 4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4,…
  #> $ carb <dbl> 4, 4, 1, 1, 2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1, 2,…
  #> $ fit  <dbl> 20.09063, 20.09063, 20.09063, 20.09063, 20.09063, 20.090…
  ```

- Confirm that the results match the model’s
  [`predict()`](https://rdrr.io/r/stats/predict.html) results

  ``` r

  tidypredict_test(model, mtcars)
  #> tidypredict test results
  #> Difference threshold: 1e-12
  #> 
  #>  All results are within the difference threshold
  ```

## Classification

``` r

set.seed(100)
cls_model <- nnet(Species ~ ., data = iris, size = 3, trace = FALSE)

fit <- tidypredict_fit(cls_model)
names(fit)
#> [1] "setosa"     "versicolor" "virginica"
```

``` r

probs <- sapply(fit, \(f) rlang::eval_tidy(f, iris))
all.equal(unname(probs), unname(predict(cls_model, iris, type = "raw")))
#> [1] TRUE
```

## parsnip

`parsnip` fitted models are also supported by `tidypredict`:

``` r

library(parsnip)

set.seed(100)
p_model <- mlp(mode = "regression", hidden_units = 3, epochs = 100) %>%
  set_engine("nnet") %>%
  fit(mpg ~ wt + hp, data = mtcars)
```

``` r

tidypredict_fit(p_model)
#> 9.63217435895367 + case_when(-0.255509663289361 + wt * -0.309449576415116 + 
#>     hp * 0.949425509730813 < -15 ~ 0, -0.255509663289361 + wt * 
#>     -0.309449576415116 + hp * 0.949425509730813 > 15 ~ 1, .default = 1/(1 + 
#>     exp(-(-0.255509663289361 + wt * -0.309449576415116 + hp * 
#>         0.949425509730813)))) * 10.1885081177867 + case_when(-0.385719808388316 + 
#>     wt * 0.522807343827803 + hp * 20.0552277393738 < -15 ~ 0, 
#>     -0.385719808388316 + wt * 0.522807343827803 + hp * 20.0552277393738 > 
#>         15 ~ 1, .default = 1/(1 + exp(-(-0.385719808388316 + 
#>         wt * 0.522807343827803 + hp * 20.0552277393738)))) * 
#>     0.269942523260361 + case_when(0.42285989779505 + wt * -0.212906496087752 + 
#>     hp * -0.880839556387573 < -15 ~ 0, 0.42285989779505 + wt * 
#>     -0.212906496087752 + hp * -0.880839556387573 > 15 ~ 1, .default = 1/(1 + 
#>     exp(-(0.42285989779505 + wt * -0.212906496087752 + hp * -0.880839556387573)))) * 
#>     10.480592173546
```

Note that `parsnip` runs the class probabilities of `predict.nnet()`
through a second softmax, so the expressions returned for a
classification
[`mlp()`](https://parsnip.tidymodels.org/reference/mlp.html) model match
`predict(model, type = "prob")` rather than
`predict(model$fit, type = "raw")`.

## Parse model spec

Here is an example of the model spec:

``` r

pm <- parse_model(model)
str(pm, 2)
#> List of 3
#>  $ general:List of 6
#>   ..$ model    : chr "nnet"
#>   ..$ version  : num 2
#>   ..$ type     : chr "nnet"
#>   ..$ n_units  : int 7
#>   ..$ n_outputs: num 1
#>   ..$ softmax  : logi FALSE
#>  $ inputs :List of 2
#>   ..$ :List of 2
#>   ..$ :List of 2
#>  $ units  :List of 4
#>   ..$ :List of 3
#>   ..$ :List of 3
#>   ..$ :List of 3
#>   ..$ :List of 3
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_nnet" "list"
```
