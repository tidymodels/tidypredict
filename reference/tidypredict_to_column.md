# Adds the prediction columns to a piped command set.

Adds a new column with the results from tidypredict_fit() to a piped
command set. If add_interval is set to TRUE, it will add two additional
columns- one for the lower and another for the upper prediction interval
bounds.

## Usage

``` r
tidypredict_to_column(
  df,
  model,
  add_interval = FALSE,
  interval = 0.95,
  vars = c("fit", "upper", "lower")
)
```

## Arguments

- df:

  A data.frame or tibble

- model:

  An R model or a parsed model inside a data frame

- add_interval:

  Switch that indicates if the prediction interval columns should be
  added. Defaults to FALSE

- interval:

  The prediction interval, defaults to 0.95. Ignored if add_interval is
  set to FALSE

- vars:

  The name of the variables that this function will produce. Defaults to
  "fit", "upper", and "lower".
