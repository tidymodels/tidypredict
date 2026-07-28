# Oblique Random Forest, using aorsf

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✗ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✔ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

Only regression models with numeric predictors are supported.
Classification requires a voting mechanism that cannot be expressed as a
single formula, and oblique splits on categorical predictors operate on
an internal encoding that is not reproduced here.

## How it works

Here is a simple
[`orsf()`](https://docs.ropensci.org/aorsf/reference/orsf.html) model
using the `mtcars` dataset:

``` r

library(dplyr)
library(tidypredict)
library(aorsf)

model <- orsf(mtcars, mpg ~ ., n_tree = 5)
```

## Under the hood

Unlike axis-aligned forests, each split in an oblique random forest is a
linear combination of (standardized) predictors compared against a
cutpoint. `tidypredict` reads the trees stored in the fitted forest,
folds the forest’s centering and scaling into the split coefficients,
and turns each tree into a nested
[`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
statement. The trees are then averaged.

``` r

tidypredict_fit(model)
#> (case_when(3.60801211635648 * vs + -3.66260984300838 * wt + 0.84109533190524 * 
#>     drat + -0.86426993142018 * carb <= -6.36065505089175 ~ case_when(-0.279347919999303 * 
#>     vs + -2.43472422259486 * cyl + -1.53141538025346 * drat + 
#>     1.96682379319858 * am <= -20.890841546162 ~ case_when(0.940955334080965 * 
#>     cyl + 0.0956253841837064 * qsec + -0.0238807926628254 * disp <= 
#>     0.642952410929747 ~ 14.5375, .default = 16.56), .default = 21.9125), 
#>     .default = case_when(-0.144022263042667 * disp <= -11.3777587803707 ~ 
#>         25.86, .default = 31.0666666666667)) + case_when(0.530381783199017 * 
#>     gear + 2.25752944144112 * vs + -0.198247392529822 * qsec + 
#>     -0.0341743962751912 * disp <= -4.97256371024265 ~ case_when(-1.06421141630588 * 
#>     qsec + 3.40854802689151 * drat + -1.2228056108415 * carb + 
#>     -2.50373320531426 * cyl <= -28.3559671262114 ~ case_when(-2.16919281134838 * 
#>     wt + -0.179364980450563 * carb + 2.16307897004316 * vs <= 
#>     -12.1057221813812 ~ 11.26, .default = case_when(-2.21136767317935 * 
#>     drat + 1.97393024382369 * vs <= -6.96580817051496 ~ 14.6571428571429, 
#>     .default = 16.68)), .default = 20.85), .default = 26.2888888888889) + 
#>     case_when(-0.0132590726063641 * disp + -3.09360679297839 * 
#>         wt + 1.24739815160256 * am + -0.844277980493477 * carb <= 
#>         -16.2413398586462 ~ case_when(0.422670549391509 * drat + 
#>         -3.29311245511077 * wt <= -12.1053691056689 ~ 12.8, .default = 16.34), 
#>         .default = case_when(-5.13581158714948 * wt + -1.43747181787514 * 
#>             carb + 0.105053420827927 * cyl + 2.09924563666986 * 
#>             gear <= -2.94913856558917 ~ case_when(3.22585061994598 * 
#>             drat + -0.622442445879116 * vs + -0.752332579060569 * 
#>             carb <= 9.57148710154704 ~ 20.5142857142857, .default = case_when(0.280000000000003 * 
#>             am <= 0 ~ 23.24, .default = 23.52)), .default = 31.88)) + 
#>     case_when(-0.878292824088656 * carb + -0.0214159679912521 * 
#>         disp + -1.08710426307896 * qsec + -0.0255174538412039 * 
#>         hp <= -30.3574664749034 ~ case_when(0.128971922357518 * 
#>         cyl + 0.715456250101413 * am + -0.0167927351227881 * 
#>         qsec + -0.0240400149668777 * disp <= -6.89624282212838 ~ 
#>         case_when(-2.42827268310204 * wt + -1.10595839788598 * 
#>             am <= -9.32456710311185 ~ 13.0666666666667, .default = 15.6833333333333), 
#>         .default = case_when(-5.45440422557098 * wt <= -20.3449277613797 ~ 
#>             16.6, .default = 18.04)), .default = 21.0666666666667) + 
#>     case_when(0.00970647863948493 * hp + -0.973356496416391 * 
#>         carb + -1.00188342527797 * qsec + -3.45211711941148 * 
#>         cyl <= -42.3478285672315 ~ case_when(13.6619900136149 * 
#>         drat + -12.4342545685188 * vs + -0.0504003935315676 * 
#>         hp <= 30.1498854315577 ~ 12.44, .default = case_when(-1.47948905787478 * 
#>         am + -0.00693086039423572 * disp <= -3.04957857346372 ~ 
#>         14.9, .default = 16.5833333333333)), .default = case_when(7.48806396071419 * 
#>         vs + 9.78371125911463 * drat + 0.0383272700135065 * hp <= 
#>         45.9662492476846 ~ 21.76, .default = case_when(-0.0558201660441368 * 
#>         hp + -3.63079789140072 * drat <= -19.5356435084838 ~ 
#>         27.12, .default = 28.44))))/5L
```

From there, the Tidy Eval formula can be used anywhere where it can be
operated. `tidypredict` provides three paths:

- Use directly inside `dplyr`,
  `mutate(mtcars, !! tidypredict_fit(model))`
- Use `tidypredict_to_column(model)` to a piped command set
- Use `tidypredict_sql(model)` to retrieve the SQL statement

## A note on split boundaries

`aorsf` uses observed linear-combination values from the training data
as split cutpoints. A training row can therefore land exactly on a split
boundary, where floating-point differences between `aorsf`’s internal
traversal and the generated formula may send it down a different branch.
This affects only rows that coincide with a training cutpoint; on new
data the formula reproduces
[`predict()`](https://rdrr.io/r/stats/predict.html) exactly.

## parsnip

`tidypredict` also supports `aorsf` model objects fitted via the
`parsnip` package (using the `bonsai` extension).

``` r

library(parsnip)
library(bonsai)

parsnip_model <- rand_forest(mode = "regression", trees = 5) %>%
  set_engine("aorsf") %>%
  fit(mpg ~ ., data = mtcars)

tidypredict_fit(parsnip_model)
#> (case_when(0.0196071721873916 * hp + -3.9720550232519 * carb + 
#>     8.60426828110149 * gear + -0.683831515555862 * vs <= 23.3137072451098 ~ 
#>     case_when(0.0190460629187708 * disp + 0.806270082268856 * 
#>         qsec + -4.94112323945076 * wt + 1.96679304378293 * drat <= 
#>         7.69401929401893 ~ case_when(-0.376468986860399 * qsec + 
#>         0.164137189391162 * drat <= -6.04665919994007 ~ 13.94, 
#>         .default = 15.04), .default = case_when(0.011343142310457 * 
#>         hp + -0.275398685923428 * qsec + 2.0834187969114 * vs <= 
#>         -2.70223573008677 ~ 18.45, .default = case_when(-1.34333668192066 * 
#>         carb + -1.50119579165363 * qsec <= -31.697515589157 ~ 
#>         18.2, .default = 20.74))), .default = 25.45) + case_when(-2.39339659828858 * 
#>     carb + 2.33109871643029 * am + 0.930349615885421 * cyl + 
#>     9.8549248433136 * drat <= 34.6398166879475 ~ case_when(-0.470729819352388 * 
#>     cyl + 3.09388171250035 * gear + -2.18802842059769 * wt + 
#>     -1.42951257181257 * carb <= -7.04347856261502 ~ 12.8125, 
#>     .default = case_when(0.235262692802054 * gear + -5.25104434447303 * 
#>         wt <= -17.357804466581 ~ 17.6166666666667, .default = 19)), 
#>     .default = case_when(0.357752743739972 * qsec + 33.5354246462141 * 
#>         drat <= 139.651402444805 ~ 22.125, .default = 31.98)) + 
#>     case_when(-0.0440270302206912 * disp + -1.57902638385922 * 
#>         carb + 0.01757973613961 * hp + 0.151557657147848 * vs <= 
#>         -7.53542603238095 ~ case_when(-0.640463950860864 * qsec + 
#>         -1.50665730147199 * vs + -2.74849156377637 * cyl + -0.933782800030739 * 
#>         carb <= -34.9355244601654 ~ case_when(-0.336660555121158 * 
#>         qsec + -0.0303401830236199 * disp <= -14.4277124700952 ~ 
#>         14.525, .default = 16.4666666666667), .default = case_when(-2.13085106382978 * 
#>         carb + -0.548936170212752 * vs + 5.02765957446806 * gear <= 
#>         10.8212765957446 ~ 18.26, .default = 19.8333333333333)), 
#>         .default = 28.3571428571429) + case_when(-0.917968908101507 * 
#>     cyl + 3.83865193910176 * am + 4.86326569484682 * vs + -1.75411199395579 * 
#>     wt <= -3.13249600266024 ~ case_when(-0.0528304923638516 * 
#>     hp + 3.18409575242937 * am + -0.110332704050221 * qsec + 
#>     -0.735815417254629 * carb <= -12.5948296211181 ~ case_when(-0.0302740348005677 * 
#>     disp + -0.0174008021446286 * hp <= -17.3227598055144 ~ 12.12, 
#>     .default = 16.1666666666667), .default = case_when(-1.94299739846774 * 
#>     cyl + -0.868715704327313 * drat + -0.662597346776915 * wt <= 
#>     -16.6381298910434 ~ 19.325, .default = 23.15)), .default = 31.5285714285714) + 
#>     case_when(-0.389568908246694 * carb + -3.6667904239376 * 
#>         wt + -0.00903401095979056 * disp + 1.75643837070596 * 
#>         vs <= -12.6107082972698 ~ case_when(-0.274024754826135 * 
#>         gear + -0.307307137129027 * qsec + -1.20865464760728 * 
#>         cyl + -0.0170841468055182 * disp <= -20.6683733455337 ~ 
#>         case_when(-1.21208447538508 * carb + 3.98424130678293 * 
#>             am + -0.00961203543527863 * disp <= -8.21255030388785 ~ 
#>             13, .default = 16.3125), .default = 18.9428571428571), 
#>         .default = 26.36))/5L
```
