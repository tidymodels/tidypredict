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
  #> 1 - 1/(1 + exp(case_when(wt >= 3.18000007 ~ -0.436363667, (qsec < 
  #>     19.1849995 | is.na(qsec)) & (wt < 3.18000007 | is.na(wt)) ~ 
  #>     0.428571463, qsec >= 19.1849995 & (wt < 3.18000007 | is.na(wt)) ~ 
  #>     0) + case_when((wt < 3.01250005 | is.na(wt)) ~ 0.311573088, 
  #>     (hp < 222.5 | is.na(hp)) & wt >= 3.01250005 ~ -0.392053694, 
  #>     hp >= 222.5 & wt >= 3.01250005 ~ -0.0240745768) + case_when((gear < 
  #>     3.5 | is.na(gear)) ~ -0.355945677, (wt < 3.01250005 | is.na(wt)) & 
  #>     gear >= 3.5 ~ 0.325712085, wt >= 3.01250005 & gear >= 3.5 ~ 
  #>     -0.0384863913) + case_when((gear < 3.5 | is.na(gear)) ~ -0.309683114, 
  #>     (wt < 3.01250005 | is.na(wt)) & gear >= 3.5 ~ 0.283893973, 
  #>     wt >= 3.01250005 & gear >= 3.5 ~ -0.032039877) + case_when((gear < 
  #>     3.5 | is.na(gear)) ~ -0.275577009, (wt < 3.01250005 | is.na(wt)) & 
  #>     gear >= 3.5 ~ 0.252453178, wt >= 3.01250005 & gear >= 3.5 ~ 
  #>     -0.0266750772) + case_when((gear < 3.5 | is.na(gear)) ~ -0.248323873, 
  #>     (qsec < 17.6599998 | is.na(qsec)) & gear >= 3.5 ~ 0.261978835, 
  #>     qsec >= 17.6599998 & gear >= 3.5 ~ -0.00959526002) + case_when((gear < 
  #>     3.5 | is.na(gear)) ~ -0.225384533, (wt < 3.01250005 | is.na(wt)) & 
  #>     gear >= 3.5 ~ 0.218285918, wt >= 3.01250005 & gear >= 3.5 ~ 
  #>     -0.0373593047) + case_when((gear < 3.5 | is.na(gear)) ~ -0.205454513, 
  #>     (qsec < 18.7550011 | is.na(qsec)) & gear >= 3.5 ~ 0.196076646, 
  #>     qsec >= 18.7550011 & gear >= 3.5 ~ -0.0544253439) + case_when((wt < 
  #>     3.01250005 | is.na(wt)) ~ 0.149246693, (qsec < 17.4099998 | 
  #>     is.na(qsec)) & wt >= 3.01250005 ~ 0.0354709327, qsec >= 17.4099998 & 
  #>     wt >= 3.01250005 ~ -0.226075932) + case_when((gear < 3.5 | 
  #>     is.na(gear)) ~ -0.184417158, (wt < 3.01250005 | is.na(wt)) & 
  #>     gear >= 3.5 ~ 0.176768288, wt >= 3.01250005 & gear >= 3.5 ~ 
  #>     -0.0237750355) + case_when((gear < 3.5 | is.na(gear)) ~ -0.168993726, 
  #>     (qsec < 18.6049995 | is.na(qsec)) & gear >= 3.5 ~ 0.155569643, 
  #>     qsec >= 18.6049995 & gear >= 3.5 ~ -0.0325752236) + case_when((wt < 
  #>     3.01250005 | is.na(wt)) ~ 0.119126029, wt >= 3.01250005 ~ 
  #>     -0.105012275) + case_when((qsec < 17.1749992 | is.na(qsec)) ~ 
  #>     0.117254697, qsec >= 17.1749992 ~ -0.0994235724) + case_when((wt < 
  #>     3.18000007 | is.na(wt)) ~ 0.097100094, wt >= 3.18000007 ~ 
  #>     -0.10567718) + case_when((wt < 3.18000007 | is.na(wt)) ~ 
  #>     0.0824323222, wt >= 3.18000007 ~ -0.091120176) + case_when((qsec < 
  #>     17.5100002 | is.na(qsec)) ~ 0.0854752287, qsec >= 17.5100002 ~ 
  #>     -0.0764453933) + case_when((wt < 3.18000007 | is.na(wt)) ~ 
  #>     0.0749477893, wt >= 3.18000007 ~ -0.0799863264) + case_when((qsec < 
  #>     17.7099991 | is.na(qsec)) ~ 0.0728750378, qsec >= 17.7099991 ~ 
  #>     -0.0646049976) + case_when((wt < 3.18000007 | is.na(wt)) ~ 
  #>     0.0682478622, wt >= 3.18000007 ~ -0.0711427554) + case_when((wt < 
  #>     3.18000007 | is.na(wt)) ~ 0.0579533465, wt >= 3.18000007 ~ 
  #>     -0.0613371208) + case_when((qsec < 18.1499996 | is.na(qsec)) ~ 
  #>     0.0595484748, qsec >= 18.1499996 ~ -0.0546668135) + case_when((wt < 
  #>     3.18000007 | is.na(wt)) ~ 0.0535288528, wt >= 3.18000007 ~ 
  #>     -0.0558333211) + case_when((wt < 3.18000007 | is.na(wt)) ~ 
  #>     0.0454574414, wt >= 3.18000007 ~ -0.048143398) + case_when((qsec < 
  #>     18.5600014 | is.na(qsec)) ~ 0.0422042683, qsec >= 18.5600014 ~ 
  #>     -0.0454404354) + case_when((wt < 3.18000007 | is.na(wt)) ~ 
  #>     0.0420555808, wt >= 3.18000007 ~ -0.0449385941) + case_when((qsec < 
  #>     18.5600014 | is.na(qsec)) ~ 0.0393446013, qsec >= 18.5600014 ~ 
  #>     -0.0425945036) + case_when((wt < 3.18000007 | is.na(wt)) ~ 
  #>     0.0391179025, wt >= 3.18000007 ~ -0.0420661867) + case_when((qsec < 
  #>     18.4099998 | is.na(qsec)) ~ 0.0304145869, qsec >= 18.4099998 ~ 
  #>     -0.031833414) + case_when((wt < 3.18000007 | is.na(wt)) ~ 
  #>     0.0362136625, wt >= 3.18000007 ~ -0.038949281) + case_when((qsec < 
  #>     18.4099998 | is.na(qsec)) ~ 0.0295153651, qsec >= 18.4099998 ~ 
  #>     -0.0307046026) + case_when((drat < 3.80999994 | is.na(drat)) ~ 
  #>     -0.0306891855, drat >= 3.80999994 ~ 0.0288283136) + case_when((qsec < 
  #>     18.4099998 | is.na(qsec)) ~ 0.0271221269, qsec >= 18.4099998 ~ 
  #>     -0.0281750448) + case_when((qsec < 18.4099998 | is.na(qsec)) ~ 
  #>     0.0228891298, qsec >= 18.4099998 ~ -0.0238814205) + case_when((drat < 
  #>     3.80999994 | is.na(drat)) ~ -0.0296511576, drat >= 3.80999994 ~ 
  #>     0.0280048084) + case_when((qsec < 18.4099998 | is.na(qsec)) ~ 
  #>     0.0214707125, qsec >= 18.4099998 ~ -0.0224219449) + case_when((qsec < 
  #>     18.4099998 | is.na(qsec)) ~ 0.0181306079, qsec >= 18.4099998 ~ 
  #>     -0.0190209728) + case_when((wt < 3.18000007 | is.na(wt)) ~ 
  #>     0.0379650332, wt >= 3.18000007 ~ -0.0395050682) + case_when((qsec < 
  #>     18.4099998 | is.na(qsec)) ~ 0.0194106717, qsec >= 18.4099998 ~ 
  #>     -0.0202215631) + case_when((qsec < 18.4099998 | is.na(qsec)) ~ 
  #>     0.0164139606, qsec >= 18.4099998 ~ -0.0171694476) + case_when((qsec < 
  #>     18.4099998 | is.na(qsec)) ~ 0.013879573, qsec >= 18.4099998 ~ 
  #>     -0.0145772668) + case_when((qsec < 18.4099998 | is.na(qsec)) ~ 
  #>     0.0117362784, qsec >= 18.4099998 ~ -0.0123759825) + case_when((wt < 
  #>     3.18000007 | is.na(wt)) ~ 0.0388614088, wt >= 3.18000007 ~ 
  #>     -0.0400568396) + log(0.5/(1 - 0.5))))
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
  #> $ fit  <dbl> 0.98576418, 0.98576418, 0.92735110, 0.01081509, 0.046390…
  ```

- Confirm that `tidypredict` results match to the model’s
  [`predict()`](https://rdrr.io/r/stats/predict.html) results. The
  `xg_df` argument expects the `xgb.DMatrix` data set.

  ``` r
  tidypredict_test(model, mtcars, xg_df = xgb_bin_data)
  #> tidypredict test results
  #> Difference threshold: 1e-12
  #> 
  #>  All results are within the difference threshold
  ```

Please be aware that xgboost converts data into 32-bit floats
internally. This could possibly lead to splits being done incorrectly.
Always verify that the predictions match up with model predictions. [See
this issue](https://github.com/tidymodels/tidypredict/issues/45) for
more information.

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
#> Fitted records above the threshold: 15
#> 
#> Fit max  difference:
#> Lower max difference:
#> Upper max difference:8.06462707725331e-08
```

## Parse model spec

Here is an example of the model spec:

``` r
pm <- parse_model(model)
str(pm, 2)
#> List of 2
#>  $ general:List of 7
#>   ..$ model        : chr "xgb.Booster"
#>   ..$ type         : chr "xgb"
#>   ..$ niter        : num 50
#>   ..$ params       :List of 4
#>   ..$ feature_names: chr [1:10] "mpg" "cyl" "disp" "hp" ...
#>   ..$ nfeatures    : int 10
#>   ..$ version      : num 1
#>  $ trees  :List of 42
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
#>   .. .. .. ..$ val    : num 3.18
#>   .. .. .. ..$ op     : chr "less"
#>   .. .. .. ..$ missing: logi FALSE
#>   ..$ :List of 2
#>   .. ..$ prediction: num 0.429
#>   .. ..$ path      :List of 2
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "qsec"
#>   .. .. .. ..$ val    : num 19.2
#>   .. .. .. ..$ op     : chr "more-equal"
#>   .. .. .. ..$ missing: logi TRUE
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "wt"
#>   .. .. .. ..$ val    : num 3.18
#>   .. .. .. ..$ op     : chr "more-equal"
#>   .. .. .. ..$ missing: logi TRUE
#>   ..$ :List of 2
#>   .. ..$ prediction: num 0
#>   .. ..$ path      :List of 2
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "qsec"
#>   .. .. .. ..$ val    : num 19.2
#>   .. .. .. ..$ op     : chr "less"
#>   .. .. .. ..$ missing: logi FALSE
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "wt"
#>   .. .. .. ..$ val    : num 3.18
#>   .. .. .. ..$ op     : chr "more-equal"
#>   .. .. .. ..$ missing: logi TRUE
```
