# Returns a SQL query with formula to calculate fitted values

Returns a SQL query with formula to calculate fitted values

## Usage

``` r
tidypredict_sql(model, con)
```

## Arguments

- model:

  An R model or a list with a parsed model

- con:

  Database connection object. It is used to select the correct SQL
  translation syntax.

## Examples

``` r
library(dbplyr)
#> 
#> Attaching package: ‘dbplyr’
#> The following objects are masked from ‘package:dplyr’:
#> 
#>     ident, sql

model <- lm(mpg ~ wt + am + cyl, data = mtcars)
tidypredict_sql(model, simulate_dbi())
#> <SQL> ((39.4179334351865 + (`wt` * -3.12514220026708)) + (`am` * 0.176493157719672)) + (`cyl` * -1.5102456624971)
```
