# LightGBM models

| Function                                                                                                                                                                                                                                                       | Works |
|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔     |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)                                                                                                                                                             | ✔     |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)                                                                                                                                                                       | ✔     |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md)                                                     | ✗     |
| `parsnip`                                                                                                                                                                                                                                                      | ✔     |

## `tidypredict_` functions

``` r
library(lightgbm)

# Prepare data
X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
y <- mtcars$hp

dtrain <- lgb.Dataset(X, label = y, colnames = c("mpg", "cyl", "disp"))

model <- lgb.train(
  params = list(
    num_leaves = 4L,
    learning_rate = 0.5,
    objective = "regression",
    min_data_in_leaf = 1L
  ),
  data = dtrain,
  nrounds = 10L,
  verbose = -1L
)
```

- Create the R formula

  ``` r
  tidypredict_fit(model)
  #> case_when(cyl <= 7 | is.na(cyl) ~ 122.371527777778, .default = case_when(mpg <= 
  #>     15.1 | is.na(mpg) ~ case_when(disp <= 334 | is.na(disp) ~ 
  #>     240.84375, .default = 187.34375), .default = 164.21875)) + 
  #>     case_when(mpg <= 20.35 | is.na(mpg) ~ case_when(mpg <= 15.1 | 
  #>         is.na(mpg) ~ 24.7864583333333, .default = 7.36516196529071), 
  #>         .default = case_when(mpg <= 23.6 | is.na(mpg) ~ -9.47147832598005, 
  #>             .default = -24.4000499589103)) + case_when(mpg <= 
  #>     21.45 | is.na(mpg) ~ case_when(mpg <= 17.55 | is.na(mpg) ~ 
  #>     case_when(mpg <= 15.65 | is.na(mpg) ~ 6.33150075541602, .default = 18.2080434163411), 
  #>     .default = 0.0642609119415283), .default = -11.2250245332718) + 
  #>     case_when(disp <= 334 | is.na(disp) ~ case_when(mpg <= 15.1 | 
  #>         is.na(mpg) ~ 31.5191459655761, .default = -4.16611832639445), 
  #>         .default = case_when(disp <= 380 | is.na(disp) ~ 16.3295566439629, 
  #>             .default = -0.254162490367889)) + case_when(disp <= 
  #>     78.85 | is.na(disp) ~ -10.7901678085327, .default = case_when(mpg <= 
  #>     28.85 | is.na(mpg) ~ case_when(disp <= 334 | is.na(disp) ~ 
  #>     -0.749505842104554, .default = 4.01884852349758), .default = 15.2098321914673)) + 
  #>     case_when(disp <= 78.85 | is.na(disp) ~ -5.39508358637492, 
  #>         .default = case_when(disp <= 466 | is.na(disp) ~ case_when(mpg <= 
  #>             15.1 | is.na(mpg) ~ 4.51956310272216, .default = 0.0956797075012456), 
  #>             .default = -8.61319732666016)) + case_when(mpg <= 
  #>     21.45 | is.na(mpg) ~ case_when(disp <= 153.35 | is.na(disp) ~ 
  #>     case_when(cyl <= 5 | is.na(cyl) ~ 0.427817046642302, .default = 25.0094966888427), 
  #>     .default = -0.436469084024429), .default = -1.67079323530197) + 
  #>     case_when(disp <= 334 | is.na(disp) ~ case_when(mpg <= 15.1 | 
  #>         is.na(mpg) ~ 14.0927782058715, .default = case_when(disp <= 
  #>         288.4 | is.na(disp) ~ -0.208523882286889, .default = -11.3294992446899)), 
  #>         .default = 1.61815292341635) + case_when(mpg <= 13.8 | 
  #>     is.na(mpg) ~ -3.70564748346806, .default = case_when(mpg <= 
  #>     17.55 | is.na(mpg) ~ case_when(disp <= 334 | is.na(disp) ~ 
  #>     -0.805894414583842, .default = 9.19054534534613), .default = -0.580966327060014)) + 
  #>     case_when(disp <= 380 | is.na(disp) ~ case_when(mpg <= 17.55 | 
  #>         is.na(mpg) ~ 1.89159697956509, .default = case_when(mpg <= 
  #>         18.95 | is.na(mpg) ~ -6.20051162441571, .default = 0.834156103432178)), 
  #>         .default = -2.94233404099941)
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
  #> $ fit  <dbl> 107.75256, 107.75256, 95.22895, 107.75256, 186.49246, 11…
  ```

- Confirm that `tidypredict` results match to the model’s
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

LightGBM supports many objective functions. The following objectives are
supported by `tidypredict`:

### Regression objectives (identity transform)

- `regression` / `regression_l2` (default)
- `regression_l1`
- `huber`
- `fair`
- `quantile`
- `mape`

### Regression objectives (exp transform)

- `poisson`
- `gamma`
- `tweedie`

### Binary classification (sigmoid transform)

- `binary`
- `cross_entropy`

### Multiclass classification

- `multiclass` (softmax transform)
- `multiclassova` (per-class sigmoid)

## Binary classification example

``` r
X_bin <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
y_bin <- mtcars$am

dtrain_bin <- lgb.Dataset(X_bin, label = y_bin, colnames = c("mpg", "cyl", "disp"))

model_bin <- lgb.train(
  params = list(
    num_leaves = 4L,
    learning_rate = 0.5,
    objective = "binary",
    min_data_in_leaf = 1L
  ),
  data = dtrain_bin,
  nrounds = 10L,
  verbose = -1L
)

tidypredict_test(model_bin, xg_df = X_bin)
#> tidypredict test results
#> Difference threshold: 1e-12
#> 
#>  All results are within the difference threshold
```

## Multiclass classification

For multiclass models,
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
returns a named list of formulas, one for each class:

``` r
X_iris <- data.matrix(iris[, 1:4])
colnames(X_iris) <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
y_iris <- as.integer(iris$Species) - 1L

dtrain_iris <- lgb.Dataset(X_iris, label = y_iris, colnames = colnames(X_iris))

model_multi <- lgb.train(
  params = list(
    num_leaves = 4L,
    learning_rate = 0.5,
    objective = "multiclass",
    num_class = 3L,
    min_data_in_leaf = 1L
  ),
  data = dtrain_iris,
  nrounds = 5L,
  verbose = -1L
)

fit_formulas <- tidypredict_fit(model_multi)
names(fit_formulas)
#> [1] "class_0" "class_1" "class_2"
```

Each formula produces the predicted probability for that class:

``` r
iris %>%
  mutate(
    prob_setosa = !!fit_formulas$class_0,
    prob_versicolor = !!fit_formulas$class_1,
    prob_virginica = !!fit_formulas$class_2
  ) %>%
  select(Species, starts_with("prob_")) %>%
  head()
#>   Species prob_setosa prob_versicolor prob_virginica
#> 1  setosa   0.9786973      0.01046491      0.0108378
#> 2  setosa   0.9786973      0.01046491      0.0108378
#> 3  setosa   0.9786973      0.01046491      0.0108378
#> 4  setosa   0.9786973      0.01046491      0.0108378
#> 5  setosa   0.9786973      0.01046491      0.0108378
#> 6  setosa   0.9786973      0.01046491      0.0108378
```

Note:
[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
does not support multiclass models. Use
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
directly.

## Categorical features

LightGBM supports native categorical features. When a feature is marked
as categorical, `tidypredict` generates appropriate `%in%` conditions:

``` r
set.seed(123)
n <- 200
cat_data <- data.frame(
  cat_feat = sample(0:3, n, replace = TRUE),
  y = NA
)
cat_data$y <- ifelse(cat_data$cat_feat %in% c(0, 1), 10, -10) + rnorm(n, sd = 2)

X_cat <- matrix(cat_data$cat_feat, ncol = 1)
colnames(X_cat) <- "cat_feat"

dtrain_cat <- lgb.Dataset(
  X_cat,
  label = cat_data$y,
  categorical_feature = "cat_feat"
)

model_cat <- lgb.train(
  params = list(
    num_leaves = 4L,
    learning_rate = 1.0,
    objective = "regression",
    min_data_in_leaf = 1L
  ),
  data = dtrain_cat,
  nrounds = 2L,
  verbose = -1L
)

tidypredict_fit(model_cat)
#> case_when(cat_feat %in% 0:1 ~ 9.22111156962135, .default = -9.19527530561794) + 
#>     case_when(cat_feat %in% 0:1 ~ 0.837108638881579, .default = -0.837108347632668)
```

## parsnip

`parsnip` fitted models (via the `bonsai` package) are also supported by
`tidypredict`:

``` r
library(parsnip)
library(bonsai)

p_model <- boost_tree(
  trees = 10,
  tree_depth = 3,
  min_n = 1
) %>%
  set_engine("lightgbm") %>%
  set_mode("regression") %>%
  fit(hp ~ mpg + cyl + disp, data = mtcars)

# Extract the underlying lgb.Booster
lgb_model <- p_model$fit

tidypredict_test(lgb_model, xg_df = X)
#> tidypredict test results
#> Difference threshold: 1e-12
#> 
#>  All results are within the difference threshold
```

## Parse model spec

Here is an example of the model spec:

``` r
pm <- parse_model(model)
str(pm, 2)
#> List of 2
#>  $ general:List of 9
#>   ..$ model                 : chr "lgb.Booster"
#>   ..$ type                  : chr "lgb"
#>   ..$ version               : num 3
#>   ..$ params                :List of 8
#>   ..$ feature_names         : chr [1:3] "mpg" "cyl" "disp"
#>   ..$ nfeatures             : int 3
#>   ..$ num_class             : int 1
#>   ..$ num_tree_per_iteration: int 1
#>   ..$ niter                 : int 10
#>  $ trees  :List of 10
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>   ..$ :List of 4
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_lgb" "list"
```

``` r
str(pm$trees[1])
#> List of 1
#>  $ :List of 4
#>   ..$ :List of 3
#>   .. ..$ prediction: num 122
#>   .. ..$ linear    : NULL
#>   .. ..$ path      :List of 1
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "cyl"
#>   .. .. .. ..$ val    : num 7
#>   .. .. .. ..$ op     : chr "less-equal"
#>   .. .. .. ..$ missing: logi TRUE
#>   ..$ :List of 3
#>   .. ..$ prediction: num 241
#>   .. ..$ linear    : NULL
#>   .. ..$ path      :List of 3
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "cyl"
#>   .. .. .. ..$ val    : num 7
#>   .. .. .. ..$ op     : chr "more"
#>   .. .. .. ..$ missing: logi FALSE
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "mpg"
#>   .. .. .. ..$ val    : num 15.1
#>   .. .. .. ..$ op     : chr "less-equal"
#>   .. .. .. ..$ missing: logi TRUE
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "disp"
#>   .. .. .. ..$ val    : num 334
#>   .. .. .. ..$ op     : chr "less-equal"
#>   .. .. .. ..$ missing: logi TRUE
#>   ..$ :List of 3
#>   .. ..$ prediction: num 187
#>   .. ..$ linear    : NULL
#>   .. ..$ path      :List of 3
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "cyl"
#>   .. .. .. ..$ val    : num 7
#>   .. .. .. ..$ op     : chr "more"
#>   .. .. .. ..$ missing: logi FALSE
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "mpg"
#>   .. .. .. ..$ val    : num 15.1
#>   .. .. .. ..$ op     : chr "less-equal"
#>   .. .. .. ..$ missing: logi TRUE
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "disp"
#>   .. .. .. ..$ val    : num 334
#>   .. .. .. ..$ op     : chr "more"
#>   .. .. .. ..$ missing: logi FALSE
#>   ..$ :List of 3
#>   .. ..$ prediction: num 164
#>   .. ..$ linear    : NULL
#>   .. ..$ path      :List of 2
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "cyl"
#>   .. .. .. ..$ val    : num 7
#>   .. .. .. ..$ op     : chr "more"
#>   .. .. .. ..$ missing: logi FALSE
#>   .. .. ..$ :List of 5
#>   .. .. .. ..$ type   : chr "conditional"
#>   .. .. .. ..$ col    : chr "mpg"
#>   .. .. .. ..$ val    : num 15.1
#>   .. .. .. ..$ op     : chr "more"
#>   .. .. .. ..$ missing: logi FALSE
```

## Limitations

- Ranking objectives (`lambdarank`, `rank_xendcg`) are not supported
- Prediction intervals are not supported
- [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  does not support multiclass models
