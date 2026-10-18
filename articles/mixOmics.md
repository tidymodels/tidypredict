# mixOmics PLS models

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✔\* |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✔\* |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

\* Only for regression models with a single outcome.

`mixOmics` fits partial least squares models with
[`pls()`](https://parsnip.tidymodels.org/reference/pls.html) and
`spls()`, and the discriminant analysis variants with `plsda()` and
`splsda()`. All four collapse down to one set of regression coefficients
per outcome, so the generated expression is a plain linear combination
of the predictors.

For a single-outcome regression model,
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
returns one expression. For a model with multiple outcome columns it
returns a *named list* of expressions, one per outcome. For the
discriminant variants it returns a named list of class-probability
expressions (the softmax of the predicted dummy outcomes, matching what
`plsmod` predicts). Since those two cases return a list,
[`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)
and
[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
only work for single-outcome regression models.

The number of components (`ncomp`) is baked into the coefficients, and
the sparse variants simply give the predictors that were not selected a
coefficient of zero.

## `tidypredict_` functions

``` r

x <- as.matrix(mtcars[c("cyl", "disp", "hp", "drat")])
model <- mixOmics::pls(x, mtcars$mpg, ncomp = 2)
```

- Create the R formula

  ``` r

  tidypredict_fit(model)
  #> 24.2937361352851 + (ifelse(is.na(cyl), 6.1875, cyl) * -0.889931447343478) + 
  #>     (ifelse(is.na(disp), 230.721875, disp) * -0.0130565637350741) + 
  #>     (ifelse(is.na(hp), 146.6875, hp) * -0.0228109587521556) + 
  #>     (ifelse(is.na(drat), 3.5965625, drat) * 2.1303277985918)
  ```

- Add the prediction to the original table

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
  #> $ fit  <dbl> 22.66417, 22.66417, 25.40424, 19.63776, 15.19254, 19.500…
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

## Discriminant analysis

`plsda()` and `splsda()` return one probability expression per class:

``` r

da_model <- mixOmics::splsda(as.matrix(iris[1:4]), iris$Species, ncomp = 2)

fit <- tidypredict_fit(da_model)
names(fit)
#> [1] "setosa"     "versicolor" "virginica"
fit[["setosa"]]
#> 1/(1 + exp(1.96606184899222 + (ifelse(is.na(Sepal.Length), 5.84333333333333, 
#>     Sepal.Length) * -0.0303959537594359) + (ifelse(is.na(Sepal.Width), 
#>     3.05733333333333, Sepal.Width) * -0.492023767406961) + (ifelse(is.na(Petal.Length), 
#>     3.758, Petal.Length) * 0.0179469395936271) + (ifelse(is.na(Petal.Width), 
#>     1.19933333333333, Petal.Width) * -0.0152407670638333) - (0.391460011724112 + 
#>     (ifelse(is.na(Sepal.Length), 5.84333333333333, Sepal.Length) * 
#>         -0.118750531861652) + (ifelse(is.na(Sepal.Width), 3.05733333333333, 
#>     Sepal.Width) * 0.378311039737146) + (ifelse(is.na(Petal.Length), 
#>     3.758, Petal.Length) * -0.084556881358667) + (ifelse(is.na(Petal.Width), 
#>     1.19933333333333, Petal.Width) * -0.16933234787912))) + exp(-1.35752186071634 + 
#>     (ifelse(is.na(Sepal.Length), 5.84333333333333, Sepal.Length) * 
#>         0.149146485621088) + (ifelse(is.na(Sepal.Width), 3.05733333333333, 
#>     Sepal.Width) * 0.113712727669814) + (ifelse(is.na(Petal.Length), 
#>     3.758, Petal.Length) * 0.06660994176504) + (ifelse(is.na(Petal.Width), 
#>     1.19933333333333, Petal.Width) * 0.184573114942953) - (0.391460011724112 + 
#>     (ifelse(is.na(Sepal.Length), 5.84333333333333, Sepal.Length) * 
#>         -0.118750531861652) + (ifelse(is.na(Sepal.Width), 3.05733333333333, 
#>     Sepal.Width) * 0.378311039737146) + (ifelse(is.na(Petal.Length), 
#>     3.758, Petal.Length) * -0.084556881358667) + (ifelse(is.na(Petal.Width), 
#>     1.19933333333333, Petal.Width) * -0.16933234787912))))
```

## parsnip

`parsnip` fitted models are also supported by `tidypredict`. The
`mixOmics` engine of
[`pls()`](https://parsnip.tidymodels.org/reference/pls.html) works for
both regression and classification:

``` r

library(parsnip)
library(plsmod)

p_model <- pls(num_comp = 2) %>%
  set_engine("mixOmics") %>%
  set_mode("regression") %>%
  fit(mpg ~ disp + hp + drat, data = mtcars)

tidypredict_fit(p_model)
#> 18.9698741155116 + (ifelse(is.na(disp), 230.721875, disp) * -0.0183282632913477) + 
#>     (ifelse(is.na(hp), 146.6875, hp) * -0.0323705835435153) + 
#>     (ifelse(is.na(drat), 3.5965625, drat) * 2.80763705068415)
```

`mixOmics` models are fit from a numeric matrix, so a model fit directly
can only refer to the matrix columns it was given. Categorical
predictors therefore work through the `parsnip` interface, which keeps
the formula around and lets the dummy columns be written in terms of the
original factors:

``` r

cars <- transform(mtcars, gear = factor(gear))

c_model <- pls(num_comp = 2) %>%
  set_engine("mixOmics") %>%
  set_mode("regression") %>%
  fit(mpg ~ disp + hp + gear, data = cars)

tidypredict_fit(c_model)
#> 28.3990491890354 + (ifelse(is.na(disp), 230.721875, disp) * -0.0212765260717577) + 
#>     (ifelse(is.na(hp), 146.6875, hp) * -0.0321040699674561) + 
#>     (ifelse(is.na(gear), 0.375, ifelse(gear == "4", 1, 0)) * 
#>         1.89118556186025) + (ifelse(is.na(gear), 0.15625, ifelse(gear == 
#>     "5", 1, 0)) * 3.84388465523581)
```

## Parse model spec

Here is an example of the model spec:

``` r

pm <- parse_model(model)
str(pm, 2)
#> List of 2
#>  $ general:List of 5
#>   ..$ model  : chr "mixo_pls"
#>   ..$ version: num 2
#>   ..$ type   : chr "regression"
#>   ..$ is_glm : num 0
#>   ..$ ncomp  : num 2
#>  $ terms  :List of 5
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_regression" "list"
```
