# Model-based boosting, using mboost

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✔ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✔ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✗ |

## How it works

[`mboost::blackboost()`](https://rdrr.io/pkg/mboost/man/blackboost.html)
boosts regression trees. Each boosting iteration contributes one tree,
shrunk by the learning rate `nu`, and the contributions are added to the
model’s offset.

``` r

library(mboost)
library(dplyr)
library(tidypredict)

model <- blackboost(
  mpg ~ wt + cyl,
  data = mtcars,
  control = boost_control(mstop = 10)
)
```

## Under the hood

The parser walks the `partykit` tree behind every boosting iteration and
turns it into one nested
[`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
statement. The learning rate and the offset are folded into the result.

``` r

pm <- parse_model(model)
str(pm, 2)
#> List of 2
#>  $ general       :List of 5
#>   ..$ model  : chr "blackboost"
#>   ..$ type   : chr "tree"
#>   ..$ version: num 3
#>   ..$ nu     : num 0.1
#>   ..$ offset : num 20.1
#>  $ tree_info_list:List of 10
#>   ..$ :List of 10
#>   ..$ :List of 10
#>   ..$ :List of 10
#>   ..$ :List of 10
#>   ..$ :List of 10
#>   ..$ :List of 10
#>   ..$ :List of 10
#>   ..$ :List of 10
#>   ..$ :List of 10
#>   ..$ :List of 10
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_tree" "list"
```

The parsed model is transformed into a `dplyr`, a.k.a. Tidy Eval,
formula.

``` r

tidypredict_fit(model)
#> 20.090625 + 0.1 * (case_when(is.na(wt) ~ NA, wt <= 2.2 ~ 9.97604166666667, 
#>     .default = case_when(is.na(cyl) ~ NA, cyl <= 6 ~ 0.834375000000001, 
#>         .default = -4.990625)) + case_when(is.na(wt) ~ NA, wt <= 
#>     2.2 ~ 8.9784375, .default = case_when(is.na(cyl) ~ NA, cyl <= 
#>     6 ~ 0.750937500000002, .default = -4.4915625)) + case_when(is.na(wt) ~ 
#>     NA, wt <= 2.2 ~ 8.08059375, .default = case_when(is.na(cyl) ~ 
#>     NA, cyl <= 6 ~ 0.675843750000001, .default = -4.04240625)) + 
#>     case_when(is.na(wt) ~ NA, wt <= 2.2 ~ 7.272534375, .default = case_when(is.na(wt) ~ 
#>         NA, wt <= 3.215 ~ 1.021116875, .default = -3.3653984375)) + 
#>     case_when(is.na(wt) ~ NA, wt <= 2.32 ~ 5.95040475892857, 
#>         .default = case_when(is.na(wt) ~ NA, wt <= 3.215 ~ 0.756544909722225, 
#>             .default = -3.02885859375)) + case_when(is.na(wt) ~ 
#>     NA, wt <= 2.2 ~ 5.95024046160714, .default = case_when(is.na(wt) ~ 
#>     NA, wt <= 3.44 ~ 0.233000806632655, .default = -3.24695450520833)) + 
#>     case_when(is.na(wt) ~ NA, wt <= 2.2 ~ 5.35521641544643, .default = case_when(is.na(wt) ~ 
#>         NA, wt <= 3.215 ~ 0.768112017372451, .default = -2.48827616665019)) + 
#>     case_when(is.na(wt) ~ NA, wt <= 2.2 ~ 4.81969477390179, .default = case_when(is.na(wt) ~ 
#>         NA, wt <= 3.46 ~ 0.143947783877994, .default = -2.82521685468915)) + 
#>     case_when(is.na(wt) ~ NA, wt <= 2.32 ~ 3.9568504185555, .default = case_when(is.na(wt) ~ 
#>         NA, wt <= 3.845 ~ -0.506055821571788, .default = -4.26769516922023)) + 
#>     case_when(is.na(wt) ~ NA, wt <= 3.215 ~ case_when(is.na(cyl) ~ 
#>         NA, cyl <= 4 ~ 2.98132077349875, .default = -0.462692282103353), 
#>         .default = case_when(is.na(wt) ~ NA, wt <= 3.845 ~ -1.25978037406472, 
#>             .default = -3.84092565229821)))
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
#> $ fit  <dbl> 20.51214, 20.51214, 21.82222, 20.51214, 17.71102, 18.941…
```

## How it performs

``` r

tidypredict_test(model, df = mtcars)
#> tidypredict test results
#> Difference threshold: 1e-12
#> 
#>  All results are within the difference threshold
```

## Categorical predictors

[`blackboost()`](https://rdrr.io/pkg/mboost/man/blackboost.html) handles
factors natively, and the generated formula uses `%in%` for the
resulting splits.

``` r

df <- transform(mtcars, cyl = factor(cyl))

model_cat <- blackboost(
  mpg ~ wt + cyl,
  data = df,
  control = boost_control(mstop = 10)
)

tidypredict_test(model_cat, df = df)
#> tidypredict test results
#> Difference threshold: 1e-12
#> 
#>  All results are within the difference threshold
```

## Limitations

- Only the [`Gaussian()`](https://rdrr.io/pkg/mboost/man/Family.html)
  family is supported. Other families apply a response function that
  cannot be expressed as a single formula.
- Prediction intervals are not supported.
- There is no `parsnip` engine for
  [`blackboost()`](https://rdrr.io/pkg/mboost/man/blackboost.html).
