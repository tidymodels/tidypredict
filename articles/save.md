# Save and re-load models

`tidypredict` splits the translation process in two. It first parses the
model to extract the needed components to produce the prediction. And
second, it uses the object with the parsed information to produce the R
formula. Thanks to this two step process, `tidypredict` does not need to
parse the model every time. `tidypredict`’s functions also accept R
objects that contained already models that have been parsed already.
Additionally, because the parsed model object is made up of a list made
up of basic variables, it is possible to save it in a file. Currently,
the best file format is YAML.

For this article, we will use the following model:

``` r
model <- lm(mpg ~ (wt + disp) * cyl, data = mtcars)
```

## Parse model

The
[`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
function allows to run the first step manually. It will return an R list
object which contains all of the needed information to produce a
prediction calculation. The structure of the parsed model varies based
on what kind of model is being processed. In general, it is consistent
in what kind of information it expects from each model type. For
example, in the example the [`lm()`](https://rdrr.io/r/stats/lm.html)
model object will return variables such as `sigma2`, which would not be
used in other model types, such as decision trees.

``` r
library(tidypredict)

parsed <- parse_model(model)
str(parsed, 2)
#> List of 2
#>  $ general:List of 6
#>   ..$ model   : chr "lm"
#>   ..$ version : num 2
#>   ..$ type    : chr "regression"
#>   ..$ residual: int 26
#>   ..$ sigma2  : num 5.91
#>   ..$ is_glm  : num 0
#>  $ terms  :List of 6
#>   ..$ :List of 5
#>   ..$ :List of 5
#>   ..$ :List of 5
#>   ..$ :List of 5
#>   ..$ :List of 5
#>   ..$ :List of 5
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_regression" "list"
```

Usually, we pass an R model object to functions such as:
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md),
and
[`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md).
These functions also accept a previously parsed model.

``` r
tidypredict_fit(parsed)
#> 53.5256637443325 + (wt * -6.38154597431605) + (disp * -0.0458426921825963) + 
#>     (cyl * -3.63025567939439) + (wt * cyl * 0.535604359938273) + 
#>     (disp * cyl * 0.00540618405824794)
```

## Saving the model

Saving the model is quite easy, use the package such as `yaml` to write
the model object as a YAML file. Any format that can persist a ragged
list object should work as well.

``` r
library(yaml)

write_yaml(parsed, "my_model.yml")
```

## Re-load the model

In a new R session, we can read the YAML file into our environment.

``` r
library(tidypredict)
library(yaml)

loaded_model <- read_yaml("my_model")

loaded_model <- as_parsed_model(loaded_model)
```

The preview of the file looks exactly as the preview of the original
parsed model.

``` r
str(loaded_model, 2)
#> List of 2
#>  $ general:List of 6
#>   ..$ model   : chr "lm"
#>   ..$ version : num 2
#>   ..$ type    : chr "regression"
#>   ..$ residual: int 26
#>   ..$ sigma2  : num 5.91
#>   ..$ is_glm  : num 0
#>  $ terms  :List of 6
#>   ..$ :List of 5
#>   ..$ :List of 5
#>   ..$ :List of 5
#>   ..$ :List of 5
#>   ..$ :List of 5
#>   ..$ :List of 5
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_regression" "list"
```

`tidypredict` is able to read the new R variable and use it to create
the formula.

``` r
tidypredict_fit(loaded_model)
#> 53.5256637 + (wt * -6.381546) + (disp * -0.0458427) + (cyl * 
#>     -3.6302557) + (wt * cyl * 0.5356044) + (disp * cyl * 0.0054062)
```

The same variable can be used with other `tidypredict` functions, such
as
[`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md)

``` r
tidypredict_sql(loaded_model, dbplyr::simulate_odbc())
#> <SQL> ((((53.5256637 + (`wt` * -6.381546)) + (`disp` * -0.0458427)) + (`cyl` * -3.6302557)) + ((`wt` * `cyl`) * 0.5356044)) + ((`disp` * `cyl`) * 0.0054062)
```

## `broom`

The `parsed_model` object integrates with
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) from `broom`.

``` r
tidy(loaded_model)
#> # A tibble: 6 × 2
#>   term        estimate
#>   <chr>          <dbl>
#> 1 (Intercept) 53.5    
#> 2 wt          -6.38   
#> 3 disp        -0.0458 
#> 4 cyl         -3.63   
#> 5 wt:cyl       0.536  
#> 6 disp:cyl     0.00541
```
