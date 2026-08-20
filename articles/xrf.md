# RuleFit, using xrf

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✔ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✔ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

## How it works

A RuleFit model is a regularized linear model over two kinds of terms:
rules extracted from a boosted tree ensemble, and the original
predictors entered linearly. Both kinds translate cleanly, so the whole
model becomes a single formula.

``` r

library(xrf)
library(dplyr)
library(tidypredict)

df <- mtcars
df$cyl <- factor(df$cyl)

model <- xrf(
  mpg ~ wt + hp + cyl,
  df,
  family = "gaussian",
  xgb_control = list(nrounds = 5, max_depth = 3)
)
```

## Under the hood

The parser reads the fitted `glmnet` coefficients and the rules they
belong to. Each rule becomes a
[`dplyr::if_else()`](https://dplyr.tidyverse.org/reference/if_else.html)
indicator multiplied by its coefficient, and the linear terms are added
on top.

``` r

pm <- parse_model(model)
str(pm, 2)
#> List of 2
#>  $ general:List of 6
#>   ..$ model  : chr "xrf"
#>   ..$ version: num 2
#>   ..$ type   : chr "regression"
#>   ..$ is_glm : num 1
#>   ..$ family : chr "gaussian"
#>   ..$ link   : chr "identity"
#>  $ terms  :List of 19
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
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
#> 21.4354600220349 + (hp * -0.00485570018743864) + (ifelse(cyl == 
#>     "4", 1, 0) * 0.728811208763373) + (ifelse(cyl == "8", 1, 
#>     0) * -2.52977053354706) + (ifelse(wt < 2.31999993, 1, 0) * 
#>     5.00384814595596) + (ifelse(wt < 2.46499991, 1, 0) * ifelse(wt >= 
#>     2.31999993, 1, 0) * 0.298737461778169) + (ifelse(wt >= 2.46499991, 
#>     1, 0) * ifelse(cyl == "8", 1, 0) * ifelse(cyl != "6", 1, 
#>     0) * -7.15444309275421e-05) + (ifelse(hp >= 123, 1, 0) * 
#>     ifelse(hp >= 180, 1, 0) * ifelse(wt < 3.77999997, 1, 0) * 
#>     -1.22640584347711) + (ifelse(hp >= 123, 1, 0) * ifelse(hp >= 
#>     180, 1, 0) * ifelse(wt >= 3.77999997, 1, 0) * -2.74340613610864) + 
#>     (ifelse(hp < 123, 1, 0) * ifelse(wt >= 2.31999993, 1, 0) * 
#>         ifelse(hp < 97, 1, 0) * 0.426427270712896) + (ifelse(hp >= 
#>     123, 1, 0) * ifelse(wt >= 5.25, 1, 0) * ifelse(hp < 230, 
#>     1, 0) * -4.49107556200436) + (ifelse(hp >= 123, 1, 0) * ifelse(wt >= 
#>     5.25, 1, 0) * ifelse(hp >= 230, 1, 0) * 0.0860692216034999) + 
#>     (ifelse(hp < 123, 1, 0) * ifelse(hp < 91, 1, 0) * 2.53529034445322) + 
#>     (ifelse(hp < 123, 1, 0) * ifelse(hp >= 91, 1, 0) * ifelse(wt < 
#>         1.61500001, 1, 0) * 0.811364276852491) + (ifelse(hp >= 
#>     123, 1, 0) * ifelse(wt < 5.25, 1, 0) * ifelse(wt < 3.84500003, 
#>     1, 0) * -0.493065242179932) + (ifelse(wt >= 3.43499994, 1, 
#>     0) * ifelse(hp < 205, 1, 0) * ifelse(hp >= 175, 1, 0) * 0.905140727759907) + 
#>     (ifelse(wt >= 3.43499994, 1, 0) * ifelse(hp >= 205, 1, 0) * 
#>         ifelse(hp >= 230, 1, 0) * -0.794841172984618) + (ifelse(wt < 
#>     3.43499994, 1, 0) * ifelse(wt < 1.93499994, 1, 0) * 2.60840030568605) + 
#>     (ifelse(wt >= 3.43499994, 1, 0) * ifelse(hp < 205, 1, 0) * 
#>         ifelse(hp < 175, 1, 0) * -2.08593440027407)
```

From there, the Tidy Eval formula can be used anywhere it can be
evaluated. `tidypredict` provides three paths:

- Use directly inside `dplyr`, `mutate(df, !! tidypredict_fit(model))`
- Use `tidypredict_to_column(model)` to add it to a piped command set
- Use `tidypredict_sql(model, con)` to retrieve the SQL statement

``` r

df %>%
  tidypredict_to_column(model) %>%
  glimpse()
#> Rows: 32
#> Columns: 12
#> $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19…
#> $ cyl  <fct> 6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4,…
#> $ disp <dbl> 160.0, 160.0, 108.0, 258.0, 360.0, 225.0, 360.0, 146.7, …
#> $ hp   <dbl> 110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123, 180,…
#> $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.…
#> $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, …
#> $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20.00, …
#> $ vs   <dbl> 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1,…
#> $ am   <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,…
#> $ gear <dbl> 4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4,…
#> $ carb <dbl> 4, 4, 1, 1, 2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1, 2,…
#> $ fit  <dbl> 20.90133, 20.90133, 22.43786, 20.90133, 18.46795, 18.839…
```

## How it performs

``` r

tidypredict_test(model, df)
#> tidypredict test results
#> Difference threshold: 1e-12
#> 
#>  All results are within the difference threshold
```

## Classification

Binary classification models, `family = "binomial"`, are supported and
return the probability of the second outcome level through the logistic
link.

``` r

df_bin <- mtcars
df_bin$vs <- factor(df_bin$vs)

model_bin <- xrf(
  vs ~ wt + mpg,
  df_bin,
  family = "binomial",
  xgb_control = list(nrounds = 5, max_depth = 3)
)

tidypredict_test(model_bin, df_bin)
#> tidypredict test results
#> Difference threshold: 1e-12
#> 
#>  All results are within the difference threshold
```

## parsnip

`tidypredict` also supports `xrf` model objects fitted via the `parsnip`
package, using
[`rule_fit()`](https://parsnip.tidymodels.org/reference/rule_fit.html)
from the `rules` package with the `"xrf"` engine.

``` r

library(parsnip)
library(rules)

parsnip_model <- rule_fit(
  mode = "regression",
  trees = 5,
  tree_depth = 3,
  penalty = 0.1
) |>
  set_engine("xrf") |>
  fit(mpg ~ wt + hp + cyl, data = df)

tidypredict_test(parsnip_model, df)
#> tidypredict test results
#> Difference threshold: 1e-12
#> 
#>  All results are within the difference threshold
```

## Limitations

- Multinomial models are not supported; only `family = "gaussian"` and
  `family = "binomial"` are.
- Prediction intervals are not supported.
- Functions and interactions written inside the model formula are not
  supported. Prepare those columns with `dplyr` before fitting.
- The underlying trees come from XGBoost, which stores split thresholds
  as 32-bit floats. See the [float
  precision](https://tidypredict.tidymodels.org/articles/float-precision.md)
  article.
