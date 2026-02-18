# Changelog

## tidypredict (development version)

### New Model Supports

- Added support for CatBoost models (`catboost.Model`). Supports
  regression, binary classification, and multiclass classification
  objectives including RMSE, MAE, Quantile, MAPE, Poisson, Logloss,
  CrossEntropy, MultiClass, and MultiClassOneVsAll. Categorical features
  are handled automatically for parsnip/bonsai models; for raw CatBoost
  models use
  [`set_catboost_categories()`](https://tidypredict.tidymodels.org/reference/set_catboost_categories.md).
  (#TBD)

- Added support for LightGBM models (`lgb.Booster`). Supports
  regression, binary classification, multiclass classification, and
  categorical features. (#TBD)

### Improvements

- [`glm()`](https://rdrr.io/r/stats/glm.html) models now support
  additional families and link functions: Gamma family with inverse
  link, inverse.gaussian family with 1/mu^2 link, probit link, cloglog
  link, and sqrt link.
  ([\#203](https://github.com/tidymodels/tidypredict/issues/203),
  [\#204](https://github.com/tidymodels/tidypredict/issues/204),
  [\#205](https://github.com/tidymodels/tidypredict/issues/205),
  [\#206](https://github.com/tidymodels/tidypredict/issues/206),
  [\#207](https://github.com/tidymodels/tidypredict/issues/207))

- xgboost support now includes additional objectives: `binary:hinge`,
  `reg:absoluteerror`, `reg:gamma`, `reg:pseudohubererror`, and
  `reg:squaredlogerror`.
  ([\#184](https://github.com/tidymodels/tidypredict/issues/184))

### Bug Fixes

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now correctly incorporates `base_score` for xgboost models with
  `count:poisson` and `reg:tweedie` objectives. Previously, predictions
  were incorrect when `base_score` was not the default value.
  ([\#184](https://github.com/tidymodels/tidypredict/issues/184))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now correctly handles xgboost DART booster models with
  `rate_drop > 0`. DART uses tree weight normalization during training,
  and these weights are now properly applied to each tree’s predictions.
  ([\#183](https://github.com/tidymodels/tidypredict/issues/183))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now correctly handles xgboost models with stump trees (single leaf, no
  splits).
  ([\#182](https://github.com/tidymodels/tidypredict/issues/182))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now uses the correct split operator (`<=` instead of `<`) for
  randomForest models.
  ([\#192](https://github.com/tidymodels/tidypredict/issues/192))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now uses the correct split operator (`<=` instead of `<`) for ranger
  models. Previously, predictions were incorrect when data values
  exactly matched split values.
  ([\#189](https://github.com/tidymodels/tidypredict/issues/189))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now correctly averages tree predictions for ranger models instead of
  summing them. Previously, predictions were `num.trees` times too
  large. ([\#190](https://github.com/tidymodels/tidypredict/issues/190))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now uses the correct split operator (`<=` instead of `<`) for
  randomForest models.
  ([\#192](https://github.com/tidymodels/tidypredict/issues/192))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now correctly handles partykit stump trees (models with no splits).
  ([\#196](https://github.com/tidymodels/tidypredict/issues/196))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now works with
  [`glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html) models
  that use family function syntax (e.g., `family = gaussian()`) instead
  of string syntax (e.g., `family = "gaussian"`).
  ([\#197](https://github.com/tidymodels/tidypredict/issues/197))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now works with models that use family function syntax (e.g.,
  `family = gaussian()`) instead of string syntax (e.g.,
  `family = "gaussian"`).
  ([\#202](https://github.com/tidymodels/tidypredict/issues/202))

## tidypredict 1.0.1

CRAN release: 2025-12-13

### Bug Fixes

- Fixed bug where `base_score` wasn’t extracted correctly xgboost for
  version 3 or higher.
  ([\#173](https://github.com/tidymodels/tidypredict/issues/173))

## tidypredict 1.0.0

CRAN release: 2025-11-29

### Breaking Changes

- Random forest implementations (ranger and randomForest) will now
  produce a single formula instead of a list of expressions.
  ([\#84](https://github.com/tidymodels/tidypredict/issues/84))

### New Model Supports

- Added support for glmnet models.
  ([\#165](https://github.com/tidymodels/tidypredict/issues/165))

### Improvements

- xgboost models with objectives `"reg:tweedie"` and `"count:poisson"`
  are now supported.
  ([\#72](https://github.com/tidymodels/tidypredict/issues/72),
  [@SimonCoulombe](https://github.com/SimonCoulombe))

- tree based models now uses `.default` argument in produced
  [`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
  code when applicable.
  ([\#153](https://github.com/tidymodels/tidypredict/issues/153))

- Speed up
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  for partykit and ranger packages.
  ([\#125](https://github.com/tidymodels/tidypredict/issues/125))

- Speed up
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  for xgboost models.
  ([\#130](https://github.com/tidymodels/tidypredict/issues/130))

- randomForest models now support regression outcomes.
  ([\#77](https://github.com/tidymodels/tidypredict/issues/77))

- An informative error will now be thrown if a lm model cannot be
  processed due to having linear combinations of predictors.
  ([\#124](https://github.com/tidymodels/tidypredict/issues/124))

- linear models such as [`lm()`](https://rdrr.io/r/stats/lm.html) and
  [`glm()`](https://rdrr.io/r/stats/glm.html) now work with interactions
  created with `*` and `:`.
  ([\#74](https://github.com/tidymodels/tidypredict/issues/74))

- Cubist rules will return simplified rules whenever possible to avoid
  multiplying by 0 and 1.
  ([\#152](https://github.com/tidymodels/tidypredict/issues/152))

- Make work with xgboost version \> 2.0.0.0.
  ([\#169](https://github.com/tidymodels/tidypredict/issues/169))

### Bug Fixes

- Fixed a bug where the intercept was added incorrectly to the result
  for cubist models.
  ([\#58](https://github.com/tidymodels/tidypredict/issues/58))

- Fixed bug where tidypredict would error on Cubist models without
  conditions.
  ([\#127](https://github.com/tidymodels/tidypredict/issues/127))

- Fixed bug where Cubst models incorrectly combined rules and
  committees.
  ([\#134](https://github.com/tidymodels/tidypredict/issues/134))

## tidypredict 0.5.1

CRAN release: 2024-12-19

- Exported a number of internal functions to be used in {orbital}
  package

## tidypredict 0.5

CRAN release: 2023-01-18

- Changes maintainer to Edgar Ruiz

- Updates author’s email addresses.

- Removes dependency with `stringr`

- Fixes issue with `earth` parsed_models
  ([\#108](https://github.com/tidymodels/tidypredict/issues/108))

- Addresses issues with XGBoost models

- Improvements to XGBoosts tests

## tidypredict 0.4.9

CRAN release: 2022-05-25

- Fixes issue handling GLM Binomial earth models
  ([\#97](https://github.com/tidymodels/tidypredict/issues/97))

- Adds capability to handle single simple Cubist models
  ([\#57](https://github.com/tidymodels/tidypredict/issues/57))

- Fixed parenthesis issue in the creation of the interval formula
  ([\#76](https://github.com/tidymodels/tidypredict/issues/76))

- Fixed bug in SQL query generation for XGBoost models with objective
  `binary:logistic`.

- Re-licensed package from GPL-3 to MIT. See [consent from copyright
  holders here](https://github.com/tidymodels/tidypredict/issues/95).

## tidypredict 0.4.8

CRAN release: 2020-10-28

- CRAN submission for a broken test case.

## tidypredict 0.4.7

CRAN release: 2020-10-05

- Change to with with version 5.1.2 and above of the `earth` package. As
  a result, `tidypredict` will only parse objects created by this and
  later versions of `earth`.

## tidypredict 0.4.6

CRAN release: 2020-07-23

- Small release for `xgboost` changes.

## tidypredict 0.4.5

CRAN release: 2020-02-10

- Switches maintainer to Max Kuhn

## tidypredict 0.4.3

CRAN release: 2019-09-03

- Adds support for categorical predictors in `partykit`

- Fixes `parsnip` tests to meet standards of new CRAN version

## tidypredict 0.4.2

CRAN release: 2019-07-15

- Simplifies tests that verify `ranger`

- Adds fit method for parsed `xgboost` models

- Sets conditional requirement for `xgboost`, for test and vignette

## tidypredict 0.4.0

CRAN release: 2019-07-12

### New features

- Parses `ranger` classification models.

- Adds method support for `broom`’s
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) function.
  Regression models only

- Adds
  [`as_parsed_model()`](https://tidypredict.tidymodels.org/reference/as_parsed_model.md)
  function. It adds the proper class components to the list.

- Adds initial support for `partykit`’s `ctree()` model

- Adds support for `parsnip` fitted models: `lm`, `randomForest`,
  `ranger`, and `earth`

- Adds support for xgb.Booster models provided by the `xgboost` package
  ([@Athospd](https://github.com/Athospd),
  [\#43](https://github.com/tidymodels/tidypredict/issues/43))

- Adds support for
  [`Cubist::cubist()`](http://topepo.github.io/Cubist/reference/cubist.default.md)
  models (# 36)

## tidypredict 0.3.0

CRAN release: 2019-01-10

### New features

- Adds support for MARS models provided by the `earth` package

### Improvements

- New parsed models are now list objects as opposed to data frames.

- tidypredict_to_column() no longer supports `ranger` and `randomForest`
  because of the multiple queries generated by multiple trees.

- All functions that read the parsed models and create the tidy eval
  formula now use the list object.

- Most of the code that depends on dplyr programming has been removed.

- Removes dependencies on: tidyr, tibble

- The `x/y` interface for `earth` models can now be used.

### Bug Fixes

- It now returns all of the trees instead of just one for tree based
  models (`randomForest` & `ranger`)
  ([\#29](https://github.com/tidymodels/tidypredict/issues/29))

## tidypredict 0.2.1

CRAN release: 2018-12-20

### Bug Fixes

- tibble 2.0.0 compatibility fix ([@krlmlr](https://github.com/krlmlr))

## tidypredict 0.2.0

CRAN release: 2018-02-25

### New features

- Add support for
  [`ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  models.

### Bug fixes

- Using `x ~.` in a randomForest() formula fails
  ([\#18](https://github.com/tidymodels/tidypredict/issues/18)
  [@washcycle](https://github.com/washcycle)).
