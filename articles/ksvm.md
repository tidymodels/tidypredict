# Support vector machines, using kernlab

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✔ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✔ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

## How it works

Only linear (`vanilladot`) kernels are supported. With a linear kernel
the decision function collapses to a weighted sum of the predictors,
which is exactly the shape `tidypredict` can write out.

``` r

library(kernlab)
library(dplyr)
library(tidypredict)

model <- ksvm(
  mpg ~ wt + hp + disp,
  data = mtcars,
  kernel = "vanilladot",
  type = "eps-svr"
)
#>  Setting default kernel parameters
```

## Under the hood

The parser multiplies the support vectors by their coefficients to
recover one weight per predictor, undoes the scaling
[`ksvm()`](https://rdrr.io/pkg/kernlab/man/ksvm.html) applied while
fitting, and adds the bias term.

``` r

pm <- parse_model(model)
str(pm, 2)
#> List of 2
#>  $ general:List of 4
#>   ..$ model  : chr "ksvm"
#>   ..$ version: num 2
#>   ..$ type   : chr "regression"
#>   ..$ is_glm : num 0
#>  $ terms  :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_regression" "list"
```

The parsed model is transformed into a `dplyr`, a.k.a. Tidy Eval,
formula.

``` r

tidypredict_fit(model)
#> 35.9868850984331 + (wt * -3.52132505992065) + (hp * -0.0344671444570161) + 
#>     (disp * 0.000700491865519246)
```

From there, the Tidy Eval formula can be used anywhere it can be
evaluated. `tidypredict` provides three paths:

- Use directly inside `dplyr`,
  `mutate(mtcars, !! tidypredict_fit(model))`
- Use `tidypredict_to_column(model)` to add it to a piped command set
- Use `tidypredict_sql(model, con)` to retrieve the SQL statement

``` r

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
#> $ fit  <dbl> 23.081706, 22.183768, 24.687620, 21.055166, 18.093954, 2…
```

``` r

tidypredict_sql(model, dbplyr::simulate_mssql())
#> <SQL> ((35.9868850984331 + ([wt] * -3.52132505992065)) + ([hp] * -0.0344671444570161)) + ([disp] * 0.000700491865519246)
```

## How it performs

``` r

tidypredict_test(model, mtcars)
#> tidypredict test results
#> Difference threshold: 1e-12
#> 
#>  All results are within the difference threshold
```

## Classification

Binary classification models are supported, and return the probability
of the second outcome level.
[`ksvm()`](https://rdrr.io/pkg/kernlab/man/ksvm.html) turns its decision
values into probabilities with a fitted sigmoid, so the model has to be
fitted with `prob.model = TRUE` for that sigmoid to exist.

``` r

df <- mtcars
df$vs <- factor(df$vs)

model_class <- ksvm(
  vs ~ wt + mpg,
  data = df,
  kernel = "vanilladot",
  type = "C-svc",
  prob.model = TRUE
)
#>  Setting default kernel parameters

tidypredict_fit(model_class)
#> 1/(1 + exp(-(-2.97721401151508 + (wt * -0.368454175940519) + 
#>     (mpg * 0.200709025600128))))
```

## parsnip

`tidypredict` also supports `ksvm` model objects fitted via the
`parsnip` package, using
[`svm_linear()`](https://parsnip.tidymodels.org/reference/svm_linear.html)
with the `"kernlab"` engine.

``` r

library(parsnip)

parsnip_model <- svm_linear(mode = "regression") |>
  set_engine("kernlab") |>
  fit(mpg ~ wt + hp, data = mtcars)

tidypredict_fit(parsnip_model)
#> 35.8783013722882 + (wt * -3.43692889425989) + (hp * -0.0345944994000948)
```

## Limitations

- Only the linear `vanilladot` kernel is supported. Non-linear kernels
  cannot be written as a single formula over the columns.
- Only binary classification is supported, and it requires
  `prob.model = TRUE`.
- Prediction intervals are not supported.
- Non-syntactic column names are risky on the matrix interface,
  `ksvm(x, y)`, because
  [`ksvm()`](https://rdrr.io/pkg/kernlab/man/ksvm.html) mangles them
  with [`make.names()`](https://rdrr.io/r/base/make.names.html) and
  keeps no record of the originals. See the [supported
  models](https://tidypredict.tidymodels.org/articles/models.md) article
  for details.
