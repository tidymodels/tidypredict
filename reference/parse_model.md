# Converts an R model object into a parsed model

Parses a fitted R model's structure and extracts the components needed
to create a dplyr formula for prediction. The parsed model can be
serialized (e.g., saved to YAML) and later used to generate predictions
without the original model object.

## Usage

``` r
parse_model(model)
```

## Arguments

- model:

  An R model object.

## Value

A parsed model object with class `parsed_model` and a model-specific
subclass (e.g., `pm_xgb`, `pm_tree`, `pm_regression`). The object
contains:

- `$general`: List with model metadata including `model` (model type),
  `type` (used for S3 dispatch), `version` (parsed model format
  version), and model-specific parameters.

- Model-specific fields containing coefficients, tree structures, etc.

## Parsed model versions

The `$general$version` field indicates the parsed model format:

- **Version 1**: Original format. Linear models store coefficients in a
  data frame. Tree models use flat
  [`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
  expressions where all leaf conditions are at the same level.

- **Version 2**: Improved coefficient storage for linear models (lm,
  earth). Tree models still use flat
  [`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html).

- **Version 3**: Current format. Tree models (rpart, ranger,
  randomForest, xgboost, lightgbm, catboost, partykit, cubist) use
  nested
  [`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
  expressions that mirror the tree structure. This produces more
  efficient SQL and R code because conditions are evaluated
  hierarchically rather than checking all leaf paths.

When loading a parsed model saved with an older version, tidypredict
automatically uses the appropriate formula builder for backwards
compatibility.

## Model types

Each parsed model has a type that determines the S3 class used for
dispatch:

- `pm_regression`: Linear models (lm, glm, earth, glmnet)

- `pm_tree`: Single trees and forests (rpart, partykit, ranger,
  randomForest, cubist)

- `pm_xgb`: XGBoost gradient boosting models

- `pm_lgb`: LightGBM gradient boosting models

- `pm_catboost`: CatBoost gradient boosting models

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
df <- mutate(mtcars, cyl = paste0("cyl", cyl))
model <- lm(mpg ~ wt + cyl * disp, offset = am, data = df)
parse_model(model)
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
#> [1] 25
#> 
#> $general$sigma2
#> [1] 5.414866
#> 
#> $general$offset
#> am
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
#> [1] 39.41278
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
#> $terms[[1]]$qr$qr_3
#> [1] -0.1262157
#> 
#> $terms[[1]]$qr$qr_4
#> [1] 0.3862155
#> 
#> $terms[[1]]$qr$qr_5
#> [1] -0.1536563
#> 
#> $terms[[1]]$qr$qr_6
#> [1] -0.02235892
#> 
#> $terms[[1]]$qr$qr_7
#> [1] 1.040622
#> 
#> 
#> 
#> $terms[[2]]
#> $terms[[2]]$label
#> [1] "wt"
#> 
#> $terms[[2]]$coef
#> [1] -1.619173
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
#> $terms[[2]]$qr$qr_3
#> [1] 0.01011187
#> 
#> $terms[[2]]$qr$qr_4
#> [1] -0.2305162
#> 
#> $terms[[2]]$qr$qr_5
#> [1] 0.3054412
#> 
#> $terms[[2]]$qr$qr_6
#> [1] -0.09551174
#> 
#> $terms[[2]]$qr$qr_7
#> [1] 0.156527
#> 
#> 
#> 
#> $terms[[3]]
#> $terms[[3]]$label
#> [1] "cylcyl6"
#> 
#> $terms[[3]]$coef
#> [1] -18.41701
#> 
#> $terms[[3]]$is_intercept
#> [1] 0
#> 
#> $terms[[3]]$fields
#> $terms[[3]]$fields[[1]]
#> $terms[[3]]$fields[[1]]$type
#> [1] "conditional"
#> 
#> $terms[[3]]$fields[[1]]$col
#> [1] "cyl"
#> 
#> $terms[[3]]$fields[[1]]$val
#> [1] "cyl6"
#> 
#> $terms[[3]]$fields[[1]]$op
#> [1] "equal"
#> 
#> 
#> 
#> $terms[[3]]$qr
#> $terms[[3]]$qr$qr_1
#> [1] 0
#> 
#> $terms[[3]]$qr$qr_2
#> [1] 0
#> 
#> $terms[[3]]$qr$qr_3
#> [1] 0.4282663
#> 
#> $terms[[3]]$qr$qr_4
#> [1] 0.3323365
#> 
#> $terms[[3]]$qr$qr_5
#> [1] 0.1509332
#> 
#> $terms[[3]]$qr$qr_6
#> [1] 1.88383
#> 
#> $terms[[3]]$qr$qr_7
#> [1] -1.412292
#> 
#> 
#> 
#> $terms[[4]]
#> $terms[[4]]$label
#> [1] "cylcyl8"
#> 
#> $terms[[4]]$coef
#> [1] -16.20665
#> 
#> $terms[[4]]$is_intercept
#> [1] 0
#> 
#> $terms[[4]]$fields
#> $terms[[4]]$fields[[1]]
#> $terms[[4]]$fields[[1]]$type
#> [1] "conditional"
#> 
#> $terms[[4]]$fields[[1]]$col
#> [1] "cyl"
#> 
#> $terms[[4]]$fields[[1]]$val
#> [1] "cyl8"
#> 
#> $terms[[4]]$fields[[1]]$op
#> [1] "equal"
#> 
#> 
#> 
#> $terms[[4]]$qr
#> $terms[[4]]$qr$qr_1
#> [1] 0
#> 
#> $terms[[4]]$qr$qr_2
#> [1] 0
#> 
#> $terms[[4]]$qr$qr_3
#> [1] 0
#> 
#> $terms[[4]]$qr$qr_4
#> [1] 0.6462039
#> 
#> $terms[[4]]$qr$qr_5
#> [1] 0.7608288
#> 
#> $terms[[4]]$qr$qr_6
#> [1] -0.4039674
#> 
#> $terms[[4]]$qr$qr_7
#> [1] -1.645469
#> 
#> 
#> 
#> $terms[[5]]
#> $terms[[5]]$label
#> [1] "disp"
#> 
#> $terms[[5]]$coef
#> [1] -0.09297857
#> 
#> $terms[[5]]$is_intercept
#> [1] 0
#> 
#> $terms[[5]]$fields
#> $terms[[5]]$fields[[1]]
#> $terms[[5]]$fields[[1]]$type
#> [1] "ordinary"
#> 
#> $terms[[5]]$fields[[1]]$col
#> [1] "disp"
#> 
#> 
#> 
#> $terms[[5]]$qr
#> $terms[[5]]$qr$qr_1
#> [1] 0
#> 
#> $terms[[5]]$qr$qr_2
#> [1] 0
#> 
#> $terms[[5]]$qr$qr_3
#> [1] 0
#> 
#> $terms[[5]]$qr$qr_4
#> [1] 0
#> 
#> $terms[[5]]$qr$qr_5
#> [1] -0.005178979
#> 
#> $terms[[5]]$qr$qr_6
#> [1] 0.002289148
#> 
#> $terms[[5]]$qr$qr_7
#> [1] -0.01330082
#> 
#> 
#> 
#> $terms[[6]]
#> $terms[[6]]$label
#> [1] "cylcyl6:disp"
#> 
#> $terms[[6]]$coef
#> [1] 0.1113389
#> 
#> $terms[[6]]$is_intercept
#> [1] 0
#> 
#> $terms[[6]]$fields
#> $terms[[6]]$fields[[1]]
#> $terms[[6]]$fields[[1]]$type
#> [1] "conditional"
#> 
#> $terms[[6]]$fields[[1]]$col
#> [1] "cyl"
#> 
#> $terms[[6]]$fields[[1]]$val
#> [1] "cyl6"
#> 
#> $terms[[6]]$fields[[1]]$op
#> [1] "equal"
#> 
#> 
#> $terms[[6]]$fields[[2]]
#> $terms[[6]]$fields[[2]]$type
#> [1] "ordinary"
#> 
#> $terms[[6]]$fields[[2]]$col
#> [1] "disp"
#> 
#> 
#> 
#> $terms[[6]]$qr
#> $terms[[6]]$qr$qr_1
#> [1] 0
#> 
#> $terms[[6]]$qr$qr_2
#> [1] 0
#> 
#> $terms[[6]]$qr$qr_3
#> [1] 0
#> 
#> $terms[[6]]$qr$qr_4
#> [1] 0
#> 
#> $terms[[6]]$qr$qr_5
#> [1] 0
#> 
#> $terms[[6]]$qr$qr_6
#> [1] -0.01081956
#> 
#> $terms[[6]]$qr$qr_7
#> [1] 0.01266668
#> 
#> 
#> 
#> $terms[[7]]
#> $terms[[7]]$label
#> [1] "cylcyl8:disp"
#> 
#> $terms[[7]]$coef
#> [1] 0.08795571
#> 
#> $terms[[7]]$is_intercept
#> [1] 0
#> 
#> $terms[[7]]$fields
#> $terms[[7]]$fields[[1]]
#> $terms[[7]]$fields[[1]]$type
#> [1] "conditional"
#> 
#> $terms[[7]]$fields[[1]]$col
#> [1] "cyl"
#> 
#> $terms[[7]]$fields[[1]]$val
#> [1] "cyl8"
#> 
#> $terms[[7]]$fields[[1]]$op
#> [1] "equal"
#> 
#> 
#> $terms[[7]]$fields[[2]]
#> $terms[[7]]$fields[[2]]$type
#> [1] "ordinary"
#> 
#> $terms[[7]]$fields[[2]]$col
#> [1] "disp"
#> 
#> 
#> 
#> $terms[[7]]$qr
#> $terms[[7]]$qr$qr_1
#> [1] 0
#> 
#> $terms[[7]]$qr$qr_2
#> [1] 0
#> 
#> $terms[[7]]$qr$qr_3
#> [1] 0
#> 
#> $terms[[7]]$qr$qr_4
#> [1] 0
#> 
#> $terms[[7]]$qr$qr_5
#> [1] 0
#> 
#> $terms[[7]]$qr$qr_6
#> [1] 0
#> 
#> $terms[[7]]$qr$qr_7
#> [1] 0.01324096
#> 
#> 
#> 
#> 
#> attr(,"class")
#> [1] "parsed_model"  "pm_regression" "list"         
```
