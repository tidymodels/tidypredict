# Prepares parsed model object

Prepares parsed model object

## Usage

``` r
as_parsed_model(x)
```

## Arguments

- x:

  A parsed model object

## Value

The parsed model with its `parsed_model` and `pm_*` classes set.

## Examples

``` r
pm <- parse_model(lm(mpg ~ wt, data = mtcars))
as_parsed_model(pm)
#> $general
#> $general$model
#> [1] "lm"
#> 
#> $general$version
#> [1] 2
#> 
#> $general$type
#> [1] "regression"
#> 
#> $general$residual
#> [1] 30
#> 
#> $general$sigma2
#> [1] 9.277398
#> 
#> $general$is_glm
#> [1] 0
#> 
#> 
#> $terms
#> $terms[[1]]
#> $terms[[1]]$label
#> [1] "(Intercept)"
#> 
#> $terms[[1]]$coef
#> [1] 37.28513
#> 
#> $terms[[1]]$is_intercept
#> [1] 1
#> 
#> $terms[[1]]$fields
#> $terms[[1]]$fields[[1]]
#> $terms[[1]]$fields[[1]]$type
#> [1] "ordinary"
#> 
#> $terms[[1]]$fields[[1]]$col
#> [1] "(Intercept)"
#> 
#> 
#> 
#> $terms[[1]]$qr
#> $terms[[1]]$qr$qr_1
#> [1] -0.1767767
#> 
#> $terms[[1]]$qr$qr_2
#> [1] -0.5905573
#> 
#> 
#> 
#> $terms[[2]]
#> $terms[[2]]$label
#> [1] "wt"
#> 
#> $terms[[2]]$coef
#> [1] -5.344472
#> 
#> $terms[[2]]$is_intercept
#> [1] 0
#> 
#> $terms[[2]]$fields
#> $terms[[2]]$fields[[1]]
#> $terms[[2]]$fields[[1]]$type
#> [1] "ordinary"
#> 
#> $terms[[2]]$fields[[1]]$col
#> [1] "wt"
#> 
#> 
#> 
#> $terms[[2]]$qr
#> $terms[[2]]$qr$qr_1
#> [1] 0
#> 
#> $terms[[2]]$qr$qr_2
#> [1] 0.1835596
#> 
#> 
#> 
#> 
#> attr(,"class")
#> [1] "parsed_model"  "pm_regression" "parsed_model"  "pm_regression"
#> [5] "list"         
```
