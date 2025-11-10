# Non-R Models

A model that is trained in any language are able to integrate with
`tidypredict`, and thus with `broom`. The requirement is that the model
in that language is exported using the parse model spec. The easiest
file format would be YAML.

## python example

A model that was fitted using `sklearn`’s `linear_model`. The model is
based on diabetes data. Ten baseline variables, age, sex, body mass
index, average blood pressure, and six blood serum measurements were
obtained for each of n = 442 diabetes patients, as well as the response
of interest, a quantitative measure of disease progression one year
after baseline. The model’s results were converted to YAML by the same
python script, I copied and pasted the top part here:

    general:
      is_glm: 0
      model: lm
      residual: 0
      sigma2: 0
      type: regression
      version: 2.0
    terms:
    - coef: 152.76430691633442
      fields:
      - col: (Intercept)
        type: ordinary
      is_intercept: 1
      label: (Intercept)

## Read in R

The YAML data can be read in R by using the `yaml` package. In this
example, we have copy-pasted most of the models inside a variable called
`sklearn_model`. Because `yaml` requires local YAML variables to be
split by line, we use
[`strsplit()`](https://rdrr.io/r/base/strsplit.html).

``` r
library(yaml)

sklearn_model <- strsplit("general:
  is_glm: 0
  model: lm
  residual: 0
  sigma2: 0
  type: regression
  version: 2.0
terms:
- coef: 152.76430691633442
  fields:
  - col: (Intercept)
    type: ordinary
  is_intercept: 1
  label: (Intercept)
- coef: 0.3034995490660432
  fields:
  - col: age
    type: ordinary
  is_intercept: 0
  label: age
- coef: -237.63931533353403
  fields:
  - col: sex
    type: ordinary
  is_intercept: 0
  label: sex
- coef: 510.5306054362253
  fields:
  - col: bmi
    type: ordinary
  is_intercept: 0
  label: bmi
- coef: 327.7369804093466
  fields:
  - col: bp
    type: ordinary
  is_intercept: 0
  label: bp
- coef: -814.1317093725387
  fields:
  - col: s1
    type: ordinary
  is_intercept: 0
  label: s1
", split = "\n")[[1]]
```

Now the model is converted to an R `list` using `yaml.load`.

``` r
sklearn_model <- yaml.load(sklearn_model)

str(sklearn_model, 2)
#> List of 2
#>  $ general:List of 6
#>   ..$ is_glm  : int 0
#>   ..$ model   : chr "lm"
#>   ..$ residual: int 0
#>   ..$ sigma2  : int 0
#>   ..$ type    : chr "regression"
#>   ..$ version : num 2
#>  $ terms  :List of 6
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
```

## `tidypredict`

The `list` object needs to be recognized as a `tidypredict` parsed
model. To do that, we use
[`as_parsed_model()`](https://tidypredict.tidymodels.org/reference/as_parsed_model.md)

``` r
library(tidypredict)

spm <- as_parsed_model(sklearn_model)

class(spm)
#> [1] "parsed_model"  "pm_regression" "list"
```

The `spm` variable now works just as any parsed model inside R. Use
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
to view the resulting formula.

``` r
tidypredict_fit(spm)
#> 152.764306916334 + (age * 0.303499549066043) + (sex * -237.639315333534) + 
#>     (bmi * 510.530605436225) + (bp * 327.736980409347) + (s1 * 
#>     -814.131709372539)
```

Now, the model can run **inside a database**

``` r
tidypredict_sql(spm, dbplyr::simulate_mssql())
#> <SQL> ((((152.764306916334 + (`age` * 0.303499549066043)) + (`sex` * -237.639315333534)) + (`bmi` * 510.530605436225)) + (`bp` * 327.736980409347)) + (`s1` * -814.131709372539)
```

## `broom`

Now that we have a `parsed_model` object, it is possible to use
`broom`’s [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
function. This means that we are able to integrate a totally external
model, with `broom`.

``` r
tidy(spm)
#> # A tibble: 6 × 2
#>   term        estimate
#>   <chr>          <dbl>
#> 1 (Intercept)  153.   
#> 2 age            0.303
#> 3 sex         -238.   
#> 4 bmi          511.   
#> 5 bp           328.   
#> 6 s1          -814.
```
