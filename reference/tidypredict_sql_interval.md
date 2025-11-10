# Returns a SQL query with formula to calculate predicted interval

Returns a SQL query with formula to calculate predicted interval

## Usage

``` r
tidypredict_sql_interval(model, con, interval = 0.95)
```

## Arguments

- model:

  An R model or a tibble with a parsed model

- con:

  Database connection object. It is used to select the correct SQL
  translation syntax.

- interval:

  The prediction interval, defaults to 0.95

## Examples

``` r
library(dbplyr)

model <- lm(mpg ~ wt + am + cyl, data = mtcars)
tidypredict_sql_interval(model, simulate_dbi())
#> <SQL> 2.04840714179524 * SQRT(((((((-0.176776695296637) * (-0.176776695296637)) * 6.8231093058295) + ((-0.590557271637747 + `wt` * 0.183559646169165) * (-0.590557271637747 + `wt` * 0.183559646169165)) * 6.8231093058295) + (((0.769566489443369 + `wt` * -0.176199380745393) + `am` * -0.498926847360626) * ((0.769566489443369 + `wt` * -0.176199380745393) + `am` * -0.498926847360626)) * 6.8231093058295) + ((((-0.224404416240187 + `wt` * -0.23845787029348) + `am` * -0.0214203331902854) + `cyl` * 0.161662223806132) * (((-0.224404416240187 + `wt` * -0.23845787029348) + `am` * -0.0214203331902854) + `cyl` * 0.161662223806132)) * 6.8231093058295) + 6.8231093058295)
```
