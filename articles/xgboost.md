# XGBoost models

| Function                                                                                                                                                                                                                                                       | Works |
|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔     |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)                                                                                                                                                             | ✔     |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)                                                                                                                                                                       | ✔     |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md)                                                     | ✗     |
| `parsnip`                                                                                                                                                                                                                                                      | ✔     |

## `tidypredict_` functions

``` r
library(xgboost)

logregobj <- function(preds, dtrain) {
  labels <- xgboost::getinfo(dtrain, "label")
  preds <- 1 / (1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)
  return(list(grad = grad, hess = hess))
}

xgb_bin_data <- xgboost::xgb.DMatrix(
  as.matrix(mtcars[, -9]),
  label = mtcars$am
)

model <- xgboost::xgb.train(
  params = list(max_depth = 2, objective = "binary:logistic", base_score = 0.5),
  data = xgb_bin_data,
  nrounds = 50
)
```

- Create the R formula

  ``` r
  tidypredict_fit(model)
  #> 1 - 1/(1 + exp(case_when(wt < 3.19000006 ~ case_when(qsec < 19.4400005 ~ 
  #>     0.428571463, .default = 0), .default = -0.436363667) + case_when(wt < 
  #>     3.1500001 ~ 0.311573088, .default = case_when(hp < 230 ~ 
  #>     -0.392053694, .default = -0.0240745768)) + case_when(gear < 
  #>     4 ~ -0.355945677, .default = case_when(wt < 3.1500001 ~ 0.325712085, 
  #>     .default = -0.0384863913)) + case_when(gear < 4 ~ -0.309683114, 
  #>     .default = case_when(wt < 3.1500001 ~ 0.283893973, .default = -0.032039877)) + 
  #>     case_when(gear < 4 ~ -0.275577009, .default = case_when(wt < 
  #>         3.1500001 ~ 0.252453178, .default = -0.0266750772)) + 
  #>     case_when(gear < 4 ~ -0.248323873, .default = case_when(qsec < 
  #>         17.0499992 ~ 0.261978835, .default = -0.00959526002)) + 
  #>     case_when(gear < 4 ~ -0.225384533, .default = case_when(wt < 
  #>         3.1500001 ~ 0.218285918, .default = -0.0373593047)) + 
  #>     case_when(gear < 4 ~ -0.205454513, .default = case_when(qsec < 
  #>         18.8999996 ~ 0.196076646, .default = -0.0544253439)) + 
  #>     case_when(wt < 3.1500001 ~ 0.149246693, .default = case_when(qsec < 
  #>         17.4200001 ~ 0.0354709327, .default = -0.226075932)) + 
  #>     case_when(gear < 4 ~ -0.184417158, .default = case_when(wt < 
  #>         3.1500001 ~ 0.176768288, .default = -0.0237750355)) + 
  #>     case_when(gear < 4 ~ -0.168993726, .default = case_when(qsec < 
  #>         18.6100006 ~ 0.155569643, .default = -0.0325752236)) + 
  #>     case_when(wt < 3.1500001 ~ 0.119126029, .default = -0.105012275) + 
  #>     case_when(qsec < 17.2999992 ~ 0.117254697, .default = -0.0994235724) + 
  #>     case_when(wt < 3.19000006 ~ 0.097100094, .default = -0.10567718) + 
  #>     case_when(wt < 3.19000006 ~ 0.0824323222, .default = -0.091120176) + 
  #>     case_when(qsec < 17.6000004 ~ 0.0854752287, .default = -0.0764453933) + 
  #>     case_when(wt < 3.19000006 ~ 0.0749477893, .default = -0.0799863264) + 
  #>     case_when(qsec < 17.8199997 ~ 0.0728750378, .default = -0.0646049976) + 
  #>     case_when(wt < 3.19000006 ~ 0.0682478622, .default = -0.0711427554) + 
  #>     case_when(wt < 3.19000006 ~ 0.0579533465, .default = -0.0613371208) + 
  #>     case_when(qsec < 18.2999992 ~ 0.0595484748, .default = -0.0546668135) + 
  #>     case_when(wt < 3.19000006 ~ 0.0535288528, .default = -0.0558333211) + 
  #>     case_when(wt < 3.19000006 ~ 0.0454574414, .default = -0.048143398) + 
  #>     case_when(qsec < 18.6000004 ~ 0.0422042683, .default = -0.0454404354) + 
  #>     case_when(wt < 3.19000006 ~ 0.0420555808, .default = -0.0449385941) + 
  #>     case_when(qsec < 18.6000004 ~ 0.0393446013, .default = -0.0425945036) + 
  #>     case_when(wt < 3.19000006 ~ 0.0391179025, .default = -0.0420661867) + 
  #>     case_when(qsec < 18.5200005 ~ 0.0304145869, .default = -0.031833414) + 
  #>     case_when(wt < 3.19000006 ~ 0.0362136625, .default = -0.038949281) + 
  #>     case_when(qsec < 18.5200005 ~ 0.0295153651, .default = -0.0307046026) + 
  #>     case_when(drat < 3.8499999 ~ -0.0306891855, .default = 0.0288283136) + 
  #>     case_when(qsec < 18.5200005 ~ 0.0271221269, .default = -0.0281750448) + 
  #>     case_when(qsec < 18.5200005 ~ 0.0228891298, .default = -0.0238814205) + 
  #>     case_when(drat < 3.8499999 ~ -0.0296511576, .default = 0.0280048084) + 
  #>     case_when(qsec < 18.5200005 ~ 0.0214707125, .default = -0.0224219449) + 
  #>     case_when(qsec < 18.5200005 ~ 0.0181306079, .default = -0.0190209728) + 
  #>     case_when(wt < 3.19000006 ~ 0.0379650332, .default = -0.0395050682) + 
  #>     case_when(qsec < 18.5200005 ~ 0.0194106717, .default = -0.0202215631) + 
  #>     case_when(qsec < 18.5200005 ~ 0.0164139606, .default = -0.0171694476) + 
  #>     case_when(qsec < 18.5200005 ~ 0.013879573, .default = -0.0145772668) + 
  #>     case_when(qsec < 18.5200005 ~ 0.0117362784, .default = -0.0123759825) + 
  #>     case_when(wt < 3.19000006 ~ 0.0388614088, .default = -0.0400568396) + 
  #>     -0.000357544719 + -0.000285989838 + -0.000228823963 + -0.00018303754 + 
  #>     -0.000146419203 + -0.000117138377 + -9.37248842e-05 + -7.49547908e-05 + 
  #>     log(0.5/(1 - 0.5))))
  ```

- Add the prediction to the original table

  ``` r
  library(dplyr)

  mtcars %>%
    tidypredict_to_column(model) %>%
    glimpse()
  #> Rows: 32
  #> Columns: 12
  #> $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19…
  #> $ cyl  <dbl> 6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4,…
  #> $ disp <dbl> 160.0, 160.0, 108.0, 258.0, 360.0, 225.0, 360.0, 146.7, …
  #> $ hp   <dbl> 110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123, 180,…
  #> $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.…
  #> $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, …
  #> $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20.00, …
  #> $ vs   <dbl> 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1,…
  #> $ am   <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,…
  #> $ gear <dbl> 4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4,…
  #> $ carb <dbl> 4, 4, 1, 1, 2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1, 2,…
  #> $ fit  <dbl> 0.98574329, 0.98574329, 0.93896617, 0.01079918, 0.046325…
  ```

- Confirm that `tidypredict` results match to the model’s
  [`predict()`](https://rdrr.io/r/stats/predict.html) results. The
  `xg_df` argument expects the `xgb.DMatrix` data set.

  ``` r
  tidypredict_test(model, mtcars, xg_df = xgb_bin_data)
  #> tidypredict test results
  #> Difference threshold: 1e-12
  #> 
  #> Fitted records above the threshold: 5
  #> 
  #> Max difference: 7.11972936162653e-08
  ```

Please be aware that XGBoost converts data into 32-bit floats
internally. This could lead to prediction discrepancies at exact split
boundaries. Always verify that predictions match using
[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md).
See the [float
precision](https://tidypredict.tidymodels.org/articles/float-precision.md)
article for more details.

## parsnip

`parsnip` fitted models are also supported by `tidypredict`:

``` r
library(parsnip)

p_model <- boost_tree(mode = "regression") %>%
  set_engine("xgboost") %>%
  fit(am ~ ., data = mtcars)
```

``` r
tidypredict_test(p_model, mtcars, xg_df = xgb_bin_data)
#> tidypredict test results
#> Difference threshold: 1e-12
#> 
#> Fitted records above the threshold: 12
#> 
#> Max difference: 1.04374733855828e-07
```

## Parse model spec

Here is an example of the model spec:

``` r
pm <- parse_model(model)
str(pm, 2)
#> List of 2
#>  $ general:List of 8
#>   ..$ model        : chr "xgb.Booster"
#>   ..$ type         : chr "xgb"
#>   ..$ params       :List of 5
#>   ..$ feature_names: chr [1:10] "mpg" "cyl" "disp" "hp" ...
#>   ..$ niter        : int 50
#>   ..$ nfeatures    : int 10
#>   ..$ booster_name : chr "gbtree"
#>   ..$ version      : num 3
#>  $ trees  :List of 50
#>   ..$ 0 :List of 3
#>   ..$ 1 :List of 3
#>   ..$ 2 :List of 3
#>   ..$ 3 :List of 3
#>   ..$ 4 :List of 3
#>   ..$ 5 :List of 3
#>   ..$ 6 :List of 3
#>   ..$ 7 :List of 3
#>   ..$ 8 :List of 3
#>   ..$ 9 :List of 3
#>   ..$ 10:List of 3
#>   ..$ 11:List of 2
#>   ..$ 12:List of 2
#>   ..$ 13:List of 2
#>   ..$ 14:List of 2
#>   ..$ 15:List of 2
#>   ..$ 16:List of 2
#>   ..$ 17:List of 2
#>   ..$ 18:List of 2
#>   ..$ 19:List of 2
#>   ..$ 20:List of 2
#>   ..$ 21:List of 2
#>   ..$ 22:List of 2
#>   ..$ 23:List of 2
#>   ..$ 24:List of 2
#>   ..$ 25:List of 2
#>   ..$ 26:List of 2
#>   ..$ 27:List of 2
#>   ..$ 28:List of 2
#>   ..$ 29:List of 2
#>   ..$ 30:List of 2
#>   ..$ 31:List of 2
#>   ..$ 32:List of 2
#>   ..$ 33:List of 2
#>   ..$ 34:List of 2
#>   ..$ 35:List of 2
#>   ..$ 36:List of 2
#>   ..$ 37:List of 2
#>   ..$ 38:List of 2
#>   ..$ 39:List of 2
#>   ..$ 40:List of 2
#>   ..$ 41:List of 2
#>   ..$ 42:List of 1
#>   ..$ 43:List of 1
#>   ..$ 44:List of 1
#>   ..$ 45:List of 1
#>   ..$ 46:List of 1
#>   ..$ 47:List of 1
#>   ..$ 48:List of 1
#>   ..$ 49:List of 1
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_xgb" "list"
```

``` r
str(pm$trees[1])
#> List of 1
#>  $ 0:List of 3
#>   ..$ :List of 2
#>   .. ..$ prediction: num -0.436
#>   .. ..$ path      :List of 1
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "wt"
#>   .. .. .. ..$ val    : num 3.19
#>   .. .. .. ..$ op     : chr "less"
#>   .. .. .. ..$ missing: logi TRUE
#>   ..$ :List of 2
#>   .. ..$ prediction: num 0.429
#>   .. ..$ path      :List of 2
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "qsec"
#>   .. .. .. ..$ val    : num 19.4
#>   .. .. .. ..$ op     : chr "more-equal"
#>   .. .. .. ..$ missing: logi FALSE
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "wt"
#>   .. .. .. ..$ val    : num 3.19
#>   .. .. .. ..$ op     : chr "more-equal"
#>   .. .. .. ..$ missing: logi FALSE
#>   ..$ :List of 2
#>   .. ..$ prediction: num 0
#>   .. ..$ path      :List of 2
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "qsec"
#>   .. .. .. ..$ val    : num 19.4
#>   .. .. .. ..$ op     : chr "less"
#>   .. .. .. ..$ missing: logi TRUE
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "wt"
#>   .. .. .. ..$ val    : num 3.19
#>   .. .. .. ..$ op     : chr "more-equal"
#>   .. .. .. ..$ missing: logi FALSE
```
