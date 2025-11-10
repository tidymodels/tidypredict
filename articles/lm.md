# Linear Regression

## Highlights & Limitations

- **Supports prediction intervals**, it uses the
  [`qr.solve()`](https://rdrr.io/r/base/qr.html) function to parse the
  interval coefficient of each term.
- Supports categorical variables and interactions
- Only *treatment* contrast (`contr.treatment`) are supported.
- `offset` is supported
- Categorical variables are supported
- Interactions with `:` and `*` are supported
- In-line functions in the formulas are **not supported**:
  - OK - `wt ~ mpg + am`
  - OK - `wt ~ mpg * am + vs:disp`
  - OK - `mutate(mtcars, newam = paste0(am))` and then
    `wt ~ mpg + newam`
  - Not OK - `wt ~ mpg + as.factor(am)`
  - Not OK - `wt ~ mpg + as.character(am)`

## How it works

``` r
library(dplyr)
library(tidypredict)

df <- mtcars %>%
  mutate(char_cyl = paste0("cyl", cyl)) %>%
  select(mpg, wt, char_cyl, am)

model <- lm(mpg ~ wt + char_cyl, offset = am, data = df)
```

It returns a SQL query that contains the coefficients
(`model$coefficients`) operated against the correct variable or
categorical variable value. In most cases the resulting SQL is one short
`CASE WHEN` statement per coefficient. It appends the `offset` field or
value, if one is provided.

``` r
library(tidypredict)
tidypredict_sql(model, dbplyr::simulate_mssql())
#> <SQL> (((32.4105336886021 + (`wt` * -2.83243330448326)) + (IIF(`char_cyl` = 'cyl6', 1.0, 0.0) * -4.26714873091281)) + (IIF(`char_cyl` = 'cyl8', 1.0, 0.0) * -6.12588309683682)) + `am`
```

Alternatively, use
[`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)
if the results are the be used or previewed in `dplyr`.

``` r
df %>%
  tidypredict_to_column(model) %>%
  head(10)
#>                    mpg    wt char_cyl am      fit
#> Mazda RX4         21.0 2.620     cyl6  1 21.72241
#> Mazda RX4 Wag     21.0 2.875     cyl6  1 21.00014
#> Datsun 710        22.8 2.320     cyl4  1 26.83929
#> Hornet 4 Drive    21.4 3.215     cyl6  0 19.03711
#> Hornet Sportabout 18.7 3.440     cyl8  0 16.54108
#> Valiant           18.1 3.460     cyl6  0 18.34317
#> Duster 360        14.3 3.570     cyl8  0 16.17286
#> Merc 240D         24.4 3.190     cyl4  0 23.37507
#> Merc 230          22.8 3.150     cyl4  0 23.48837
#> Merc 280          19.2 3.440     cyl6  0 18.39981
```

## Prediction intervals

Use
[`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md)
to get the SQL query that operates the prediction interval. The
`interval` defaults to 0.95

``` r
tidypredict_sql_interval(model, dbplyr::simulate_mssql())
#> <SQL> 2.04840714179524 * SQRT(((((((-0.176776695296637) * (-0.176776695296637)) * 6.63799055122669) + ((-0.590557271637747 + `wt` * 0.183559646169165) * (-0.590557271637747 + `wt` * 0.183559646169165)) * 6.63799055122669) + (((-0.126215672528828 + `wt` * 0.0101118696567173) + IIF(`char_cyl` = 'cyl6', 1.0, 0.0) * 0.428266330860589) * ((-0.126215672528828 + `wt` * 0.0101118696567173) + IIF(`char_cyl` = 'cyl6', 1.0, 0.0) * 0.428266330860589)) * 6.63799055122669) + ((((0.386215468111418 + `wt` * -0.230516217152034) + IIF(`char_cyl` = 'cyl6', 1.0, 0.0) * 0.332336511639638) + IIF(`char_cyl` = 'cyl8', 1.0, 0.0) * 0.646203930513815) * (((0.386215468111418 + `wt` * -0.230516217152034) + IIF(`char_cyl` = 'cyl6', 1.0, 0.0) * 0.332336511639638) + IIF(`char_cyl` = 'cyl8', 1.0, 0.0) * 0.646203930513815)) * 6.63799055122669) + 6.63799055122669)
```

Prediction intervals also works in the
[`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md),
just set the `add_interval` argument to `TRUE`.

``` r
df %>%
  tidypredict_to_column(model, add_interval = TRUE) %>%
  head(10)
#>                    mpg    wt char_cyl am      fit    upper    lower
#> Mazda RX4         21.0 2.620     cyl6  1 21.72241 27.41716 16.02765
#> Mazda RX4 Wag     21.0 2.875     cyl6  1 21.00014 26.65467 15.34560
#> Datsun 710        22.8 2.320     cyl4  1 26.83929 32.35180 21.32678
#> Hornet 4 Drive    21.4 3.215     cyl6  0 19.03711 24.68113 13.39309
#> Hornet Sportabout 18.7 3.440     cyl8  0 16.54108 22.07276 11.00940
#> Valiant           18.1 3.460     cyl6  0 18.34317 24.01030 12.67603
#> Duster 360        14.3 3.570     cyl8  0 16.17286 21.67635 10.66938
#> Merc 240D         24.4 3.190     cyl4  0 23.37507 29.06408 17.68606
#> Merc 230          22.8 3.150     cyl4  0 23.48837 29.16231 17.81443
#> Merc 280          19.2 3.440     cyl6  0 18.39981 24.06411 12.73552
```

## Under the hood

The parser reads several parts of the `lm` object to tabulate all of the
needed variables. One entry per coefficient is added to the final table,
those entries will have the results of
[`qr.solve()`](https://rdrr.io/r/base/qr.html) already operated and
placed in the correct column, they will have a `qr_` prefix. There will
be one `qr_` column per coefficient.

Other variables are added at the end. Some variables are not required
for every parsed model. For example, `offset` is listed because it’s
part of the formula (call) of the model, if there were no offset in a
given model, that line would not exist.

``` r
pm <- parse_model(model)
str(pm, 2)
#> List of 2
#>  $ general:List of 7
#>   ..$ model   : chr "lm"
#>   ..$ version : num 2
#>   ..$ type    : chr "regression"
#>   ..$ residual: int 28
#>   ..$ sigma2  : num 6.64
#>   ..$ offset  : symbol am
#>   ..$ is_glm  : num 0
#>  $ terms  :List of 4
#>   ..$ :List of 5
#>   ..$ :List of 5
#>   ..$ :List of 5
#>   ..$ :List of 5
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_regression" "list"
```

The output from
[`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
is transformed into a `dplyr`, a.k.a Tidy Eval, formula. All categorical
variables are operated using
[`if_else()`](https://dplyr.tidyverse.org/reference/if_else.html).

``` r
tidypredict_fit(model)
#> 32.4105336886021 + (wt * -2.83243330448326) + (ifelse(char_cyl == 
#>     "cyl6", 1, 0) * -4.26714873091281) + (ifelse(char_cyl == 
#>     "cyl8", 1, 0) * -6.12588309683682) + am
```

A function to put together the Tidy Eval interval formula is also
supported

``` r
tidypredict_interval(model)
#> 2.04840714179524 * sqrt((-0.176776695296637) * (-0.176776695296637) * 
#>     6.63799055122669 + (-0.590557271637747 + wt * 0.183559646169165) * 
#>     (-0.590557271637747 + wt * 0.183559646169165) * 6.63799055122669 + 
#>     (-0.126215672528828 + wt * 0.0101118696567173 + ifelse(char_cyl == 
#>         "cyl6", 1, 0) * 0.428266330860589) * (-0.126215672528828 + 
#>         wt * 0.0101118696567173 + ifelse(char_cyl == "cyl6", 
#>         1, 0) * 0.428266330860589) * 6.63799055122669 + (0.386215468111418 + 
#>     wt * -0.230516217152034 + ifelse(char_cyl == "cyl6", 1, 0) * 
#>     0.332336511639638 + ifelse(char_cyl == "cyl8", 1, 0) * 0.646203930513815) * 
#>     (0.386215468111418 + wt * -0.230516217152034 + ifelse(char_cyl == 
#>         "cyl6", 1, 0) * 0.332336511639638 + ifelse(char_cyl == 
#>         "cyl8", 1, 0) * 0.646203930513815) * 6.63799055122669 + 
#>     6.63799055122669)
```

From there, the Tidy Eval formula can be used anywhere where it can be
operated. `tidypredict` provides three paths:

- Use directly inside `dplyr`, `mutate(df, !! tidypredict_fit(model))`
- Use `tidypredict_to_column(model)` to a piped command set
- Use `tidypredict_to_sql(model)` to retrieve the SQL statement

The same applies to the prediction interval functions.

## How it performs

Testing the `tidypredict` results is easy. The
[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
function automatically uses the `lm` model object’s data frame, to
compare
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md),
and
[`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md)
to the results given by
[`predict()`](https://rdrr.io/r/stats/predict.html)

``` r
tidypredict_test(model)
#> tidypredict test results
#> Difference threshold: 1e-12
#> 
#>  All results are within the difference threshold
```

To run with prediction intervals set the `include_intervals` argument to
`TRUE`

``` r
tidypredict_test(model, include_intervals = TRUE)
#> tidypredict test results
#> Difference threshold: 1e-12
#> 
#>  All results are within the difference threshold
```

## parsnip

`tidypredict` also supports [`lm()`](https://rdrr.io/r/stats/lm.html)
model objects fitted via the `parsnip` package.

``` r
library(parsnip)

parsnip_model <- linear_reg() %>%
  set_engine("lm") %>%
  fit(mpg ~ wt + cyl, offset = am, data = mtcars)

tidypredict_fit(parsnip_model)
#> 39.686261480253 + (wt * -3.19097213898375) + (cyl * -1.5077949682598)
```
