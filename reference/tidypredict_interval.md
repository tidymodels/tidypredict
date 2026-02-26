# Returns a Tidy Eval formula to calculate prediction interval.

It parses a model or uses an already parsed model to return a Tidy Eval
formula that can then be used inside a dplyr command.

## Usage

``` r
tidypredict_interval(model, interval = 0.95)
```

## Arguments

- model:

  An R model or a list with a parsed model

- interval:

  The prediction interval, defaults to 0.95

## Details

The result still has to be added to and subtracted from the fit to
obtain the upper and lower bound respectively.

## Examples

``` r
model <- lm(mpg ~ wt + cyl * disp, offset = am, data = mtcars)
tidypredict_interval(model)
#> 2.05183051648029 * sqrt((-0.176776695296637) * (-0.176776695296637) * 
#>     6.20704821338125 + (-0.590557271637747 + wt * 0.183559646169165) * 
#>     (-0.590557271637747 + wt * 0.183559646169165) * 6.20704821338125 + 
#>     (-0.257207134290773 + wt * -0.230680634727453 + cyl * 0.161513439412957) * 
#>         (-0.257207134290773 + wt * -0.230680634727453 + cyl * 
#>             0.161513439412957) * 6.20704821338125 + (-0.868335233010594 + 
#>     wt * 0.271667738147758 + cyl * 0.169308509351746 + disp * 
#>     -0.00456516834027639) * (-0.868335233010594 + wt * 0.271667738147758 + 
#>     cyl * 0.169308509351746 + disp * -0.00456516834027639) * 
#>     6.20704821338125 + (-1.53184887490412 + wt * -0.163034819828353 + 
#>     cyl * 0.221405540626368 + disp * 0.0170837100474746 + cyl * 
#>     disp * -0.0020081028047214) * (-1.53184887490412 + wt * -0.163034819828353 + 
#>     cyl * 0.221405540626368 + disp * 0.0170837100474746 + cyl * 
#>     disp * -0.0020081028047214) * 6.20704821338125 + 6.20704821338125)
```
