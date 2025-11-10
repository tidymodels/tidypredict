# MARS models via the \`earth\` package

| Function                                                                                                                                                                                                                                                       | Works |
|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔     |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)                                                                                                                                                             | ✔     |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)                                                                                                                                                                       | ✔     |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md)                                                     | ✗     |
| `parsnip`                                                                                                                                                                                                                                                      | ✔     |

## `tidypredict_` functions

``` r
library(earth)
data("etitanic", package = "earth")

model <- earth(age ~ sibsp + parch, data = etitanic, degree = 3)
```

- Create the R formula

  ``` r
  tidypredict_fit(model)
  #> 22.2918960405404 + (ifelse(parch < 2, 2 - parch, 0) * 4.85356462114363) + 
  #>     (ifelse(parch > 2, parch - 2, 0) * 13.0493891423278) + (ifelse(parch > 
  #>     4, parch - 4, 0) * -18.8998708031821) + (ifelse(sibsp > 1, 
  #>     sibsp - 1, 0) * -7.71566779782019) + (ifelse(sibsp > 1, sibsp - 
  #>     1, 0) * ifelse(parch < 1, 1 - parch, 0) * 7.40395975552276) + 
  #>     (ifelse(sibsp > 1, sibsp - 1, 0) * ifelse(parch > 1, parch - 
  #>         1, 0) * 4.41874354843216)
  ```

- SQL output example

  ``` r
  tidypredict_sql(model, dbplyr::simulate_odbc())
  #> <SQL> (((((22.2918960405404 + (CASE WHEN (`parch` < 2.0) THEN (2.0 - `parch`) WHEN NOT (`parch` < 2.0) THEN 0.0 END * 4.85356462114363)) + (CASE WHEN (`parch` > 2.0) THEN (`parch` - 2.0) WHEN NOT (`parch` > 2.0) THEN 0.0 END * 13.0493891423278)) + (CASE WHEN (`parch` > 4.0) THEN (`parch` - 4.0) WHEN NOT (`parch` > 4.0) THEN 0.0 END * -18.8998708031821)) + (CASE WHEN (`sibsp` > 1.0) THEN (`sibsp` - 1.0) WHEN NOT (`sibsp` > 1.0) THEN 0.0 END * -7.71566779782019)) + ((CASE WHEN (`sibsp` > 1.0) THEN (`sibsp` - 1.0) WHEN NOT (`sibsp` > 1.0) THEN 0.0 END * CASE WHEN (`parch` < 1.0) THEN (1.0 - `parch`) WHEN NOT (`parch` < 1.0) THEN 0.0 END) * 7.40395975552276)) + ((CASE WHEN (`sibsp` > 1.0) THEN (`sibsp` - 1.0) WHEN NOT (`sibsp` > 1.0) THEN 0.0 END * CASE WHEN (`parch` > 1.0) THEN (`parch` - 1.0) WHEN NOT (`parch` > 1.0) THEN 0.0 END) * 4.41874354843216)
  ```

- Add the prediction to the original table

  ``` r
  library(dplyr)

  etitanic %>%
    tidypredict_to_column(model) %>%
    glimpse()
  #> Rows: 1,046
  #> Columns: 7
  #> $ pclass   <fct> 1st, 1st, 1st, 1st, 1st, 1st, 1st, 1st, 1st, 1st, 1s…
  #> $ survived <int> 1, 1, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1, 1…
  #> $ sex      <fct> female, male, female, male, female, male, female, ma…
  #> $ age      <dbl> 29.0000, 0.9167, 2.0000, 30.0000, 25.0000, 48.0000, …
  #> $ sibsp    <int> 0, 1, 1, 1, 1, 0, 1, 0, 2, 0, 1, 1, 0, 0, 0, 0, 0, 0…
  #> $ parch    <int> 0, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0…
  #> $ fit      <dbl> 31.99903, 22.29190, 22.29190, 22.29190, 22.29190, 31…
  ```

- Confirm that `tidypredict` results match to the model’s
  [`predict()`](https://rdrr.io/r/stats/predict.html) results

  ``` r
  tidypredict_test(model, etitanic)
  #> tidypredict test results
  #> Difference threshold: 1e-12
  #> 
  #>  All results are within the difference threshold
  ```

## GLM models

`tidypredict` supports the `glm` argument as well:

``` r
model <- earth(survived ~ .,
  data = etitanic,
  glm = list(family = binomial), degree = 2
)

tidypredict_fit(model)
#> 1 - 1/(1 + exp(2.91352600741336 + (ifelse(age > 32, age - 32, 
#>     0) * -0.0375714917713104) + (ifelse(pclass == "2nd", 1, 0) * 
#>     ifelse(sex == "male", 1, 0) * -1.76809447811121) + (ifelse(pclass == 
#>     "3rd", 1, 0) * -5.03005595780694) + (ifelse(pclass == "3rd", 
#>     1, 0) * ifelse(sibsp < 4, 4 - sibsp, 0) * 0.618652747659846) + 
#>     (ifelse(pclass == "3rd", 1, 0) * ifelse(sex == "male", 1, 
#>         0) * 1.22269536265148) + (ifelse(sex == "male", 1, 0) * 
#>     -3.18562450248531) + (ifelse(sex == "male", 1, 0) * ifelse(age < 
#>     16, 16 - age, 0) * 0.241814028713263)))
```

The spec sets the `is_glm` entry to 1, as well as the `family` and
`link` entries.

``` r
str(parse_model(model), 2)
#> List of 2
#>  $ general:List of 6
#>   ..$ model  : chr "earth"
#>   ..$ type   : chr "regression"
#>   ..$ version: num 2
#>   ..$ is_glm : num 1
#>   ..$ family : chr "binomial"
#>   ..$ link   : chr "logit"
#>  $ terms  :List of 8
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

## parsnip

`parsnip` fitted models are also supported by `tidypredict`:

``` r
library(parsnip)

p_model <- mars(mode = "regression", prod_degree = 3) %>%
  set_engine("earth") %>%
  fit(age ~ sibsp + parch, data = etitanic)

tidypredict_fit(p_model)
#> 22.2918960405404 + (ifelse(parch < 2, 2 - parch, 0) * 4.85356462114363) + 
#>     (ifelse(parch > 2, parch - 2, 0) * 13.0493891423278) + (ifelse(parch > 
#>     4, parch - 4, 0) * -18.8998708031821) + (ifelse(sibsp > 1, 
#>     sibsp - 1, 0) * -7.71566779782019) + (ifelse(sibsp > 1, sibsp - 
#>     1, 0) * ifelse(parch < 1, 1 - parch, 0) * 7.40395975552276) + 
#>     (ifelse(sibsp > 1, sibsp - 1, 0) * ifelse(parch > 1, parch - 
#>         1, 0) * 4.41874354843216)
```

## Parse model spec

Here is an example of the model spec:

``` r
pm <- parse_model(model)
str(pm, 2)
#> List of 2
#>  $ general:List of 6
#>   ..$ model  : chr "earth"
#>   ..$ type   : chr "regression"
#>   ..$ version: num 2
#>   ..$ is_glm : num 1
#>   ..$ family : chr "binomial"
#>   ..$ link   : chr "logit"
#>  $ terms  :List of 8
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

``` r
str(pm$terms[1:2])
#> List of 2
#>  $ :List of 4
#>   ..$ label       : chr "(Intercept)"
#>   ..$ coef        : num 2.91
#>   ..$ is_intercept: num 1
#>   ..$ fields      :List of 1
#>   .. ..$ : list()
#>  $ :List of 4
#>   ..$ label       : chr "h(age-32)"
#>   ..$ coef        : num -0.0376
#>   ..$ is_intercept: num 0
#>   ..$ fields      :List of 1
#>   .. ..$ :List of 4
#>   .. .. ..$ type: chr "operation"
#>   .. .. ..$ col : chr "age"
#>   .. .. ..$ val : num 32
#>   .. .. ..$ op  : chr "morethan"
```
