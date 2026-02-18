# Returns a Tidy Eval formula to calculate fitted values

It parses a model or uses an already parsed model to return a Tidy Eval
formula that can then be used inside a dplyr command.

## Usage

``` r
tidypredict_fit(model)
```

## Arguments

- model:

  An R model or a list with a parsed model.

## Examples

``` r
model <- lm(mpg ~ wt + cyl * disp, offset = am, data = mtcars)
tidypredict_fit(model)
#> 46.9691423291322 + (wt * -2.43434983315996) + (cyl * -2.86061920499755) + 
#>     (disp * -0.0765223074908513) + (cyl * disp * 0.00981028601947223) + 
#>     am
```
