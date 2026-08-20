# Tidy the parsed model results

Tidy the parsed model results

## Usage

``` r
# S3 method for class 'pm_regression'
tidy(x, ...)
```

## Arguments

- x:

  A parsed_model object

- ...:

  Reserved for future use

## Value

A tibble with one row per term, containing the `term` name and its
`estimate`.

## Examples

``` r
pm <- parse_model(lm(mpg ~ wt, data = mtcars))
tidy(pm)
#> # A tibble: 2 × 2
#>   term        estimate
#>   <chr>          <dbl>
#> 1 (Intercept)    37.3 
#> 2 wt             -5.34
```
