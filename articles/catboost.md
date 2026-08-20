# CatBoost models

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✔ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✔ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

## `tidypredict_` functions

``` r

library(catboost)

# Prepare data
X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
y <- mtcars$hp

pool <- catboost.load_pool(
  X,
  label = y,
  feature_names = as.list(c("mpg", "cyl", "disp"))
)

model <- catboost.train(
  pool,
  params = list(
    iterations = 10L,
    depth = 3L,
    learning_rate = 0.5,
    loss_function = "RMSE",
    logging_level = "Silent",
    allow_writing_files = FALSE
  )
)
```

- Create the R formula

  ``` r

  tidypredict_fit(model)
  #> case_when(cyl <= 7.00000023841858 ~ case_when(disp <= 142.900001525879 ~ 
  #>     case_when(disp <= 73.3999977111816 ~ -10.2109375, .default = -22.4244791666667), 
  #>     .default = case_when(disp <= 73.3999977111816 ~ 0, .default = -11.6136363636364)), 
  #>     .default = case_when(disp <= 142.900001525879 ~ case_when(disp <= 
  #>         73.3999977111816 ~ 0, .default = 0), .default = case_when(disp <= 
  #>         73.3999977111816 ~ 0, .default = 25.7463235294118))) + 
  #>     case_when(disp <= 241.500007629395 ~ case_when(disp <= 350.500015258789 ~ 
  #>         case_when(disp <= 114.050006866455 ~ -16.8217592592593, 
  #>             .default = -8.66318317099567), .default = case_when(disp <= 
  #>         114.050006866455 ~ 0, .default = 0)), .default = case_when(disp <= 
  #>         350.500015258789 ~ case_when(disp <= 114.050006866455 ~ 
  #>         0, .default = 8.54042598444336), .default = case_when(disp <= 
  #>         114.050006866455 ~ 0, .default = 15.0981617647059))) + 
  #>     case_when(mpg <= 16.100001335144 ~ case_when(mpg <= 15.1000008583069 ~ 
  #>         case_when(mpg <= 26.6500005722046 ~ 20.1624199886566, 
  #>             .default = 0), .default = case_when(mpg <= 26.6500005722046 ~ 
  #>         0.967519011736923, .default = 0)), .default = case_when(mpg <= 
  #>         15.1000008583069 ~ case_when(mpg <= 26.6500005722046 ~ 
  #>         0, .default = 0), .default = case_when(mpg <= 26.6500005722046 ~ 
  #>         -5.55855336794383, .default = -11.7137405960648))) + 
  #>     case_when(mpg <= 20.350001335144 ~ case_when(mpg <= 23.5999994277954 ~ 
  #>         case_when(disp <= 78.8500022888183 ~ 0, .default = 6.89136822493766), 
  #>         .default = case_when(disp <= 78.8500022888183 ~ 0, .default = 0)), 
  #>         .default = case_when(mpg <= 23.5999994277954 ~ case_when(disp <= 
  #>             78.8500022888183 ~ 0, .default = -4.38832757685116), 
  #>             .default = case_when(disp <= 78.8500022888183 ~ -9.6996753833912, 
  #>                 .default = -6.45346095341689))) + case_when(mpg <= 
  #>     15.1000008583069 ~ case_when(disp <= 302.500015258789 ~ 15.8714952840688, 
  #>     .default = 4.60180426510644), .default = case_when(disp <= 
  #>     302.500015258789 ~ -4.31877485969645, .default = -2.29917164587966)) + 
  #>     case_when(mpg <= 15.1000008583069 ~ case_when(mpg <= 14.8500008583069 ~ 
  #>         case_when(mpg <= 15.3500008583069 ~ 3.16374043226067, 
  #>             .default = 0), .default = case_when(mpg <= 15.3500008583069 ~ 
  #>         13.8875583735602, .default = 0)), .default = case_when(mpg <= 
  #>         14.8500008583069 ~ case_when(mpg <= 15.3500008583069 ~ 
  #>         0, .default = 0), .default = case_when(mpg <= 15.3500008583069 ~ 
  #>         -4.10483269954833, .default = -1.86758300594439))) + 
  #>     case_when(mpg <= 15.1000008583069 ~ case_when(disp <= 380.000015258789 ~ 
  #>         case_when(disp <= 78.8500022888183 ~ 0, .default = 12.4223339987522), 
  #>         .default = case_when(disp <= 78.8500022888183 ~ 0, .default = -1.42116288460309)), 
  #>         .default = case_when(disp <= 380.000015258789 ~ case_when(disp <= 
  #>             78.8500022888183 ~ -5.72816707113319, .default = -0.208602028436654), 
  #>             .default = case_when(disp <= 78.8500022888183 ~ 0, 
  #>                 .default = -1.21225568741093))) + case_when(cyl <= 
  #>     7.00000023841858 ~ case_when(mpg <= 23.5999994277954 ~ case_when(disp <= 
  #>     241.500007629395 ~ 1.75364491647211, .default = -2.15905609774181), 
  #>     .default = case_when(disp <= 241.500007629395 ~ -4.52541367990288, 
  #>         .default = 0)), .default = case_when(mpg <= 23.5999994277954 ~ 
  #>     case_when(disp <= 241.500007629395 ~ 0, .default = 2.8034218920811), 
  #>     .default = case_when(disp <= 241.500007629395 ~ 0, .default = 0))) + 
  #>     case_when(disp <= 87.0500068664551 ~ case_when(cyl <= 7.00000023841858 ~ 
  #>         case_when(mpg <= 11.8500008583069 ~ 0, .default = -3.59506778936443), 
  #>         .default = case_when(mpg <= 11.8500008583069 ~ 0, .default = 0)), 
  #>         .default = case_when(cyl <= 7.00000023841858 ~ case_when(mpg <= 
  #>             11.8500008583069 ~ 0, .default = 0.130749917403693), 
  #>             .default = case_when(mpg <= 11.8500008583069 ~ -2.74671544251141, 
  #>                 .default = 2.78451974222454))) + case_when(disp <= 
  #>     145.85001373291 ~ case_when(cyl <= 5.00000023841858 ~ case_when(disp <= 
  #>     466.000015258789 ~ -0.217088634121561, .default = 0), .default = case_when(disp <= 
  #>     466.000015258789 ~ 6.47088371722999, .default = 0)), .default = case_when(cyl <= 
  #>     5.00000023841858 ~ case_when(disp <= 466.000015258789 ~ -5.20113031092882, 
  #>     .default = 0), .default = case_when(disp <= 466.000015258789 ~ 
  #>     0.142842983663108, .default = -1.9983577212557))) + 146.6875
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
  #> $ fit  <dbl> 112.09608, 112.09608, 92.76673, 125.38699, 190.22023, 12…
  ```

- Confirm that `tidypredict` results match the model’s
  [`predict()`](https://rdrr.io/r/stats/predict.html) results. The
  `xg_df` argument expects the matrix data set.

  ``` r

  tidypredict_test(model, xg_df = X)
  #> tidypredict test results
  #> Difference threshold: 1e-12
  #> 
  #>  All results are within the difference threshold
  ```

## Supported objectives

CatBoost supports many objective functions. The following objectives are
supported by `tidypredict`:

`tidypredict` always returns predictions on the response scale, so the
link implied by the objective is inverted for you. This does not always
agree with
[`catboost.predict()`](https://rdrr.io/pkg/catboost/man/catboost.predict.html),
whose default `prediction_type = "RawFormulaVal"` returns the
untransformed score. The equivalent
[`catboost.predict()`](https://rdrr.io/pkg/catboost/man/catboost.predict.html)
call is listed for each group below.

### Regression objectives (identity transform)

Equivalent to `prediction_type = "RawFormulaVal"`.

- `RMSE` (default)
- `MAE`
- `Quantile`
- `MAPE`
- `Huber`
- `LogCosh`
- `Expectile`

### Log-link regression objectives (exponential transform)

Equivalent to `prediction_type = "Exponent"`.

- `Poisson`
- `Tweedie`

### Binary classification (sigmoid transform)

Equivalent to `prediction_type = "Probability"`.

- `Logloss`
- `CrossEntropy`

### Multiclass classification

- `MultiClass` (softmax transform)
- `MultiClassOneVsAll` (sigmoid per class)

## Binary classification example

``` r

X_bin <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
y_bin <- mtcars$am

pool_bin <- catboost.load_pool(
  X_bin,
  label = y_bin,
  feature_names = as.list(c("mpg", "cyl", "disp"))
)

model_bin <- catboost.train(
  pool_bin,
  params = list(
    iterations = 10L,
    depth = 3L,
    learning_rate = 0.5,
    loss_function = "Logloss",
    logging_level = "Silent",
    allow_writing_files = FALSE
  )
)

tidypredict_test(model_bin, xg_df = X_bin)
#> tidypredict test results
#> Difference threshold: 1e-12
#> 
#>  All results are within the difference threshold
```

## Multiclass classification example

``` r

X_multi <- data.matrix(iris[, 1:4])
y_multi <- as.integer(iris$Species) - 1L

pool_multi <- catboost.load_pool(
  X_multi,
  label = y_multi,
  feature_names = as.list(colnames(iris)[1:4])
)

model_multi <- catboost.train(
  pool_multi,
  params = list(
    iterations = 10L,
    depth = 3L,
    learning_rate = 0.5,
    loss_function = "MultiClass",
    logging_level = "Silent",
    allow_writing_files = FALSE
  )
)

# Multiclass returns a list of formulas, one per class
formulas <- tidypredict_fit(model_multi)
names(formulas)
#> [1] "class_0" "class_1" "class_2"
```

Test multiclass predictions:

``` r

tidypredict_test(model_multi, xg_df = X_multi)
#> tidypredict test results (multiclass: 3 classes)
#> Difference threshold: 1e-12
#> 
#>  All results are within the difference threshold
```

## Categorical features

CatBoost models can use categorical features with one-hot encoding.

### With parsnip and bonsai (recommended)

When using parsnip and bonsai, categorical features are handled
automatically:

``` r

library(parsnip)
library(bonsai)

df_cat <- data.frame(
  num_feat = mtcars$mpg,
  cat_feat = factor(ifelse(mtcars$am == 1, "manual", "auto")),
  target = mtcars$hp
)

model_spec <- boost_tree(trees = 10, tree_depth = 3) |>
  set_engine("catboost", logging_level = "Silent", one_hot_max_size = 10) |>
  set_mode("regression")

model_fit <- fit(model_spec, target ~ num_feat + cat_feat, data = df_cat)

# Categorical features are handled automatically
tidypredict_fit(model_fit)
#> case_when(num_feat <= 19.4500017166138 ~ case_when(cat_feat != 
#>     "manual" ~ case_when(num_feat <= 22.1500005722046 ~ 0.801145815426329, 
#>     .default = 0), .default = case_when(num_feat <= 22.1500005722046 ~ 
#>     1.83374995901249, .default = 0)), .default = case_when(cat_feat != 
#>     "manual" ~ case_when(num_feat <= 22.1500005722046 ~ -0.51824998841621, 
#>     .default = -0.818249981710687), .default = case_when(num_feat <= 
#>     22.1500005722046 ~ -0.354642849215972, .default = -1.44243746775901))) + 
#>     case_when(num_feat <= 21.2000017166138 ~ case_when(cat_feat != 
#>         "manual" ~ case_when(num_feat <= 16.100001335144 ~ 1.2002477008358, 
#>         .default = 0.0857384359596435), .default = case_when(num_feat <= 
#>         16.100001335144 ~ 1.81174495999619, .default = -0.219992852344537)), 
#>         .default = case_when(cat_feat != "manual" ~ case_when(num_feat <= 
#>             16.100001335144 ~ 0, .default = -0.943187122031322), 
#>             .default = case_when(num_feat <= 16.100001335144 ~ 
#>                 0, .default = -1.38558622778375))) + case_when(num_feat <= 
#>     15.3500008583069 ~ case_when(cat_feat != "manual" ~ case_when(num_feat <= 
#>     22.1500005722046 ~ 1.28753320737988, .default = 0), .default = case_when(num_feat <= 
#>     22.1500005722046 ~ 1.38500250715022, .default = 0)), .default = case_when(cat_feat != 
#>     "manual" ~ case_when(num_feat <= 22.1500005722046 ~ -0.12498855656414, 
#>     .default = -0.797112736938237), .default = case_when(num_feat <= 
#>     22.1500005722046 ~ 0.128929276853228, .default = -1.38304897148005))) + 
#>     case_when(num_feat <= 21.2000017166138 ~ case_when(num_feat <= 
#>         15.3500008583069 ~ case_when(cat_feat != "manual" ~ 1.26049501062926, 
#>         .default = 1.37461498857877), .default = case_when(cat_feat != 
#>         "manual" ~ 0.0826099759077479, .default = 0.299197257933534)), 
#>         .default = case_when(num_feat <= 15.3500008583069 ~ case_when(cat_feat != 
#>             "manual" ~ 0, .default = 0), .default = case_when(cat_feat != 
#>             "manual" ~ -0.919114475104545, .default = -1.32930321991692))) + 
#>     case_when(num_feat <= 16.100001335144 ~ case_when(cat_feat != 
#>         "manual" ~ 1.1255318828183, .default = 1.7708775572066), 
#>         .default = case_when(cat_feat != "manual" ~ -0.391087771499875, 
#>             .default = -1.11728512846467)) + case_when(num_feat <= 
#>     19.4500017166138 ~ case_when(cat_feat != "manual" ~ 0.724507238084902, 
#>     .default = 1.74962702699511), .default = case_when(cat_feat != 
#>     "manual" ~ -0.896653865664788, .default = -1.0909491224538)) + 
#>     case_when(num_feat <= 16.8499994277954 ~ case_when(num_feat <= 
#>         21.2000017166138 ~ case_when(cat_feat != "manual" ~ 1.07507321302013, 
#>         .default = 1.72863150314045), .default = case_when(cat_feat != 
#>         "manual" ~ 0, .default = 0)), .default = case_when(num_feat <= 
#>         21.2000017166138 ~ case_when(cat_feat != "manual" ~ -0.0206418356182447, 
#>         .default = -0.18999134448798), .default = case_when(cat_feat != 
#>         "manual" ~ -0.88128265688268, .default = -1.25212058591477))) + 
#>     case_when(num_feat <= 20.350001335144 ~ case_when(num_feat <= 
#>         16.100001335144 ~ case_when(cat_feat != "manual" ~ 1.06171125150465, 
#>         .default = 1.70788792556642), .default = case_when(cat_feat != 
#>         "manual" ~ 0.0749724038426973, .default = 0.231429255543504)), 
#>         .default = case_when(num_feat <= 16.100001335144 ~ case_when(cat_feat != 
#>             "manual" ~ 0, .default = 0), .default = case_when(cat_feat != 
#>             "manual" ~ -0.866174954530946, .default = -1.19395256766462))) + 
#>     case_when(num_feat <= 17.5500001907349 ~ case_when(cat_feat != 
#>         "manual" ~ case_when(num_feat <= 13.800000667572 ~ 0.996643797927786, 
#>         .default = 0.733609344823911), .default = case_when(num_feat <= 
#>         13.800000667572 ~ 0, .default = 1.68739327091772)), .default = case_when(cat_feat != 
#>         "manual" ~ case_when(num_feat <= 13.800000667572 ~ 0, 
#>         .default = -0.593103918287044), .default = case_when(num_feat <= 
#>         13.800000667572 ~ 0, .default = -1.01745881947742))) + 
#>     case_when(num_feat <= 17.5500001907349 ~ case_when(num_feat <= 
#>         25.2000017166138 ~ case_when(cat_feat != "manual" ~ 1.00555387207905, 
#>         .default = 1.6671445521193), .default = case_when(cat_feat != 
#>         "manual" ~ 0, .default = 0)), .default = case_when(num_feat <= 
#>         25.2000017166138 ~ case_when(cat_feat != "manual" ~ -0.579759080423866, 
#>         .default = -0.389137236378184), .default = case_when(cat_feat != 
#>         "manual" ~ 0, .default = -1.19950713097063))) + 146.6875
```

### With raw CatBoost

For raw CatBoost models, you need to manually establish the
hash-to-category mapping:

``` r

df_cat <- data.frame(
  num_feat = mtcars$mpg,
  cat_feat = factor(ifelse(mtcars$am == 1, "manual", "auto")),
  target = mtcars$hp
)

pool_cat <- catboost.load_pool(
  df_cat[, c("num_feat", "cat_feat")],
  label = df_cat$target
)

model_cat <- catboost.train(
  pool_cat,
  params = list(
    iterations = 10L,
    depth = 3L,
    learning_rate = 0.5,
    loss_function = "RMSE",
    logging_level = "Silent",
    allow_writing_files = FALSE,
    one_hot_max_size = 10
  )
)

# Parse and set category mapping manually
pm_cat <- parse_model(model_cat)
pm_cat <- set_catboost_categories(pm_cat, model_cat, df_cat)

# Now use the parsed model
tidypredict_fit(pm_cat)
#> case_when(num_feat <= 21.2000017166138 ~ case_when(num_feat <= 
#>     18.4000024795532 ~ case_when(cat_feat != "manual" ~ 14.925, 
#>     .default = 30.5625), .default = case_when(cat_feat != "manual" ~ 
#>     2.74479166666667, .default = -3.75520833333333)), .default = case_when(num_feat <= 
#>     18.4000024795532 ~ case_when(cat_feat != "manual" ~ 0, .default = 0), 
#>     .default = case_when(cat_feat != "manual" ~ -15.9107142857143, 
#>         .default = -23.5681818181818))) + case_when(num_feat <= 
#>     17.9500017166138 ~ case_when(num_feat <= 15.3500008583069 ~ 
#>     case_when(num_feat <= 13.800000667572 ~ 15.0135416666667, 
#>         .default = 19.76875), .default = case_when(num_feat <= 
#>     13.800000667572 ~ 0, .default = 4.58125)), .default = case_when(num_feat <= 
#>     15.3500008583069 ~ case_when(num_feat <= 13.800000667572 ~ 
#>     0, .default = 0), .default = case_when(num_feat <= 13.800000667572 ~ 
#>     0, .default = -12.6083622343566))) + case_when(num_feat <= 
#>     15.1000008583069 ~ case_when(cat_feat != "manual" ~ case_when(num_feat <= 
#>     20.350001335144 ~ 15.4599609375, .default = 0), .default = case_when(num_feat <= 
#>     20.350001335144 ~ 17.24765625, .default = 0)), .default = case_when(cat_feat != 
#>     "manual" ~ case_when(num_feat <= 20.350001335144 ~ -1.76974715625284, 
#>     .default = -7.76240670855119), .default = case_when(num_feat <= 
#>     20.350001335144 ~ 12.684482056769, .default = -10.3744425555505))) + 
#>     case_when(num_feat <= 20.350001335144 ~ case_when(num_feat <= 
#>         15.1000008583069 ~ case_when(num_feat <= 21.4500017166138 ~ 
#>         16.15517578125, .default = 0), .default = case_when(num_feat <= 
#>         21.4500017166138 ~ 2.43866339847021, .default = 0)), 
#>         .default = case_when(num_feat <= 15.1000008583069 ~ case_when(num_feat <= 
#>             21.4500017166138 ~ 0, .default = 0), .default = case_when(num_feat <= 
#>             21.4500017166138 ~ -0.81725027977202, .default = -8.92975554067353))) + 
#>     case_when(num_feat <= 16.100001335144 ~ case_when(cat_feat != 
#>         "manual" ~ 1.74184966725444, .default = 17.1624022513511), 
#>         .default = case_when(cat_feat != "manual" ~ -1.84846928363624, 
#>             .default = -2.5527898625831)) + case_when(num_feat <= 
#>     15.1000008583069 ~ case_when(cat_feat != "manual" ~ case_when(num_feat <= 
#>     16.100001335144 ~ 5.0359026918736, .default = 0), .default = case_when(num_feat <= 
#>     16.100001335144 ~ 10.9270019646749, .default = 0)), .default = case_when(cat_feat != 
#>     "manual" ~ case_when(num_feat <= 16.100001335144 ~ -4.68237897736796, 
#>     .default = -1.12228492220772), .default = case_when(num_feat <= 
#>     16.100001335144 ~ 6.23540028667622, .default = -1.54990813085402))) + 
#>     case_when(num_feat <= 17.9500017166138 ~ case_when(cat_feat != 
#>         "manual" ~ case_when(num_feat <= 15.3500008583069 ~ 1.35878278732009, 
#>         .default = -1.61413378865887), .default = case_when(num_feat <= 
#>         15.3500008583069 ~ 9.56112671909051, .default = 5.45597525084169)), 
#>         .default = case_when(cat_feat != "manual" ~ case_when(num_feat <= 
#>             15.3500008583069 ~ 0, .default = -0.472868525836861), 
#>             .default = case_when(num_feat <= 15.3500008583069 ~ 
#>                 0, .default = -0.941015650875657))) + case_when(num_feat <= 
#>     22.1500005722046 ~ case_when(num_feat <= 11.8500008583069 ~ 
#>     case_when(cat_feat != "manual" ~ -1.27554270637297, .default = 0), 
#>     .default = case_when(cat_feat != "manual" ~ 1.29787175135093, 
#>         .default = 8.12576428739517)), .default = case_when(num_feat <= 
#>     11.8500008583069 ~ case_when(cat_feat != "manual" ~ 0, .default = 0), 
#>     .default = case_when(cat_feat != "manual" ~ -3.90652769980473, 
#>         .default = -2.85706547242369))) + case_when(num_feat <= 
#>     15.1000008583069 ~ case_when(cat_feat != "manual" ~ case_when(num_feat <= 
#>     16.8499994277954 ~ 2.95365536454389, .default = 0), .default = case_when(num_feat <= 
#>     16.8499994277954 ~ 7.3502653432798, .default = 0)), .default = case_when(cat_feat != 
#>     "manual" ~ case_when(num_feat <= 16.8499994277954 ~ -2.19386484538651, 
#>     .default = -1.18252440262858), .default = case_when(num_feat <= 
#>     16.8499994277954 ~ 3.75825780856209, .default = -1.01788803241075))) + 
#>     case_when(num_feat <= 15.1000008583069 ~ case_when(num_feat <= 
#>         14.8500008583069 ~ case_when(num_feat <= 31.4000024795532 ~ 
#>         2.03063806312393, .default = 0), .default = case_when(num_feat <= 
#>         31.4000024795532 ~ 6.43148217536982, .default = 0)), 
#>         .default = case_when(num_feat <= 14.8500008583069 ~ case_when(num_feat <= 
#>             31.4000024795532 ~ 0, .default = 0), .default = case_when(num_feat <= 
#>             31.4000024795532 ~ 0.0318676040224114, .default = -3.35761814041808))) + 
#>     146.6875
```

## Parse model spec

Here is an example of the model spec:

``` r

pm <- parse_model(model)
str(pm, 2)
#> List of 2
#>  $ general:List of 13
#>   ..$ model            : chr "catboost.Model"
#>   ..$ type             : chr "catboost"
#>   ..$ version          : num 3
#>   ..$ params           :List of 1
#>   ..$ num_class        : num 1
#>   ..$ cat_features     : list()
#>   ..$ cat_feature_names: chr(0) 
#>   ..$ feature_names    : chr [1:3] "mpg" "cyl" "disp"
#>   ..$ nfeatures        : int 3
#>   ..$ scale            : int 1
#>   ..$ bias             : num 147
#>   ..$ niter            : int 10
#>   ..$ tree_type        : chr "oblivious"
#>  $ trees  :List of 10
#>   ..$ :List of 8
#>   ..$ :List of 8
#>   ..$ :List of 8
#>   ..$ :List of 8
#>   ..$ :List of 4
#>   ..$ :List of 8
#>   ..$ :List of 8
#>   ..$ :List of 8
#>   ..$ :List of 8
#>   ..$ :List of 8
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_catboost" "list"
```

``` r

str(pm$trees[1], 2)
#> List of 1
#>  $ :List of 8
#>   ..$ :List of 2
#>   ..$ :List of 2
#>   ..$ :List of 2
#>   ..$ :List of 2
#>   ..$ :List of 2
#>   ..$ :List of 2
#>   ..$ :List of 2
#>   ..$ :List of 2
```

## Limitations

- Prediction intervals are not supported
- CatBoost uses 32-bit floats for split thresholds, which may cause
  prediction discrepancies at exact split boundaries. See the [float
  precision](https://tidypredict.tidymodels.org/articles/float-precision.md)
  article for details.
