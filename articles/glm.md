# Generalized Linear Regression

## Highlights & Limitations

- Defaults to 0-to-1 predictions for `binomial` family models. That is
  akin to running `predict(model, type = "response")`
- Only *treatment* contrast (`contr.treatment`) are supported.
- `offset` is supported
- Categorical variables are supported
- In-line functions in the formulas are **not supported**:
  - OK - `wt ~ mpg + am`
  - OK - `mutate(mtcars, newam = paste0(am))` and then
    `wt ~ mpg + newam`
  - Not OK - `wt ~ mpg + as.factor(am)`
  - Not OK - `wt ~ mpg + as.character(am)`
- Interval functions are not supported:
  [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md)
  &
  [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md)

## How it works

``` r
library(tidypredict)
library(dplyr)

df <- mtcars %>%
  mutate(char_cyl = paste0("cyl", cyl)) %>%
  select(wt, char_cyl, am)

model <- glm(am ~ wt + char_cyl, data = df, family = "binomial")
```

It returns a SQL query that contains the coefficients
(`model$coefficients`) operated against the correct variable or
categorical variable value. In most cases the resulting SQL is one short
`CASE WHEN` statement per coefficient. It appends the `offset` field or
value, if one is provided.

For `binomial` models, the
[sigmoid](https://en.wikipedia.org/wiki/Sigmoid_function) equation is
applied. This means that the target SQL database type will need to
support the exponent function.

``` r
library(tidypredict)
tidypredict_sql(model, dbplyr::simulate_mssql())
#> <SQL> 1.0 - 1.0 / (1.0 + EXP(((20.8527831345691 + (`wt` * -7.85934263583836)) + (IIF(`char_cyl` = 'cyl6', 1.0, 0.0) * 3.10462643177453)) + (IIF(`char_cyl` = 'cyl8', 1.0, 0.0) * 5.37942092366097)))
```

Alternatively, use
[`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)
if the results are the be used or previewed in `dplyr`.

``` r
df %>%
  tidypredict_to_column(model) %>%
  head(10)
#>                      wt char_cyl am        fit
#> Mazda RX4         2.620     cyl6  1 0.96662269
#> Mazda RX4 Wag     2.875     cyl6  1 0.79605201
#> Datsun 710        2.320     cyl4  1 0.93208127
#> Hornet 4 Drive    3.215     cyl6  0 0.21242376
#> Hornet Sportabout 3.440     cyl8  0 0.30918450
#> Valiant           3.460     cyl6  0 0.03783629
#> Duster 360        3.570     cyl8  0 0.13875740
#> Merc 240D         3.190     cyl4  0 0.01450687
#> Merc 230          3.150     cyl4  0 0.01975984
#> Merc 280          3.440     cyl6  0 0.04399324
```

## Under the hood

The parser reads several parts of the `glm` object to tabulate all of
the needed variables. One entry per coefficient is added to the final
table. Other variables are added at the end. Some variables are not
required for every parsed model. For example, `offset` is listed because
it’s part of the formula (call) of the model, if there were no offset in
a given model, that line would not exist.

``` r
pm <- parse_model(model)
str(pm, 2)
#> List of 2
#>  $ general:List of 7
#>   ..$ model   : chr "glm"
#>   ..$ version : num 2
#>   ..$ type    : chr "regression"
#>   ..$ residual: int 28
#>   ..$ family  : chr "binomial"
#>   ..$ link    : chr "logit"
#>   ..$ is_glm  : num 1
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
#> 1 - 1/(1 + exp(20.8527831345691 + (wt * -7.85934263583836) + 
#>     (ifelse(char_cyl == "cyl6", 1, 0) * 3.10462643177453) + (ifelse(char_cyl == 
#>     "cyl8", 1, 0) * 5.37942092366097)))
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
