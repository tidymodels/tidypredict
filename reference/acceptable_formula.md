# Checks that the formula can be parsed

Uses an S3 method to check that a given formula can be parsed based on
its class. It currently scans for contrasts that are not supported and
in-line functions. (e.g: lm(wt ~ as.factor(am))). Since this function is
meant for function interaction, as opposed to human interaction, a
successful check is silent.

## Usage

``` r
acceptable_formula(model)
```

## Arguments

- model:

  An R model object

## Examples

``` r
model <- lm(mpg ~ wt, mtcars)
acceptable_formula(model)
```
