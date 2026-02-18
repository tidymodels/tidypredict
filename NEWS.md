# tidypredict (development version)

## New Model Supports

- Added support for CatBoost models (`catboost.Model`). Supports regression, binary classification, and multiclass classification objectives including RMSE, MAE, Quantile, MAPE, Poisson, Logloss, CrossEntropy, MultiClass, and MultiClassOneVsAll. Categorical features are handled automatically for parsnip/bonsai models; for raw CatBoost models use `set_catboost_categories()`. (#TBD)

- Added support for LightGBM models (`lgb.Booster`). Supports regression, binary classification, multiclass classification, and categorical features. (#TBD)

## Improvements

- xgboost support now includes additional objectives: `binary:hinge`, `reg:absoluteerror`, `reg:gamma`, `reg:pseudohubererror`, and `reg:squaredlogerror`. (#184)

## Bug Fixes

- `tidypredict_fit()` now correctly incorporates `base_score` for xgboost models with `count:poisson` and `reg:tweedie` objectives. Previously, predictions were incorrect when `base_score` was not the default value. (#184)

- `tidypredict_fit()` now correctly handles xgboost DART booster models with `rate_drop > 0`. DART uses tree weight normalization during training, and these weights are now properly applied to each tree's predictions. (#183)

- `tidypredict_fit()` now correctly handles xgboost models with stump trees (single leaf, no splits). (#182)

- `tidypredict_fit()` now uses the correct split operator (`<=` instead of `<`) for randomForest models. (#192)

- `tidypredict_fit()` now uses the correct split operator (`<=` instead of `<`) for ranger models. Previously, predictions were incorrect when data values exactly matched split values. (#189)

- `tidypredict_fit()` now correctly averages tree predictions for ranger models instead of summing them. Previously, predictions were `num.trees` times too large. (#190)

- `tidypredict_fit()` now correctly averages tree predictions for LightGBM models with `boosting="rf"` instead of summing them. (#185)

- `tidypredict_fit()` now correctly handles partykit stump trees (models with no splits). (#196)

- `tidypredict_fit()` now works with `glmnet()` models that use family function syntax (e.g., `family = gaussian()`) instead of string syntax (e.g., `family = "gaussian"`). (#197)

- `tidypredict_fit()` now works with models that use family function syntax (e.g., `family = gaussian()`) instead of string syntax (e.g., `family = "gaussian"`). (#202)

# tidypredict 1.0.1

## Bug Fixes

- Fixed bug where `base_score` wasn't extracted correctly xgboost for version 3 or higher. (#173)

# tidypredict 1.0.0

## Breaking Changes

- Random forest implementations (ranger and randomForest) will now produce a single formula instead of a list of expressions. (#84)

## New Model Supports

- Added support for glmnet models. (#165)

## Improvements

- xgboost models with objectives `"reg:tweedie"` and `"count:poisson"` are now supported. (#72, @SimonCoulombe)

- tree based models now uses `.default` argument in produced `case_when()` code when applicable. (#153)

- Speed up `tidypredict_fit()` for partykit and ranger packages. (#125)

- Speed up `tidypredict_fit()` for xgboost models. (#130)

- randomForest models now support regression outcomes. (#77)

- An informative error will now be thrown if a lm model cannot be processed due to having linear combinations of predictors. (#124)

- linear models such as `lm()` and `glm()` now work with interactions created with `*` and `:`. (#74) 

- Cubist rules will return simplified rules whenever possible to avoid multiplying by 0 and 1. (#152)

- Make work with xgboost version > 2.0.0.0. (#169)

## Bug Fixes

- Fixed a bug where the intercept was added incorrectly to the result for cubist models. (#58)

- Fixed bug where tidypredict would error on Cubist models without conditions. (#127)

- Fixed bug where Cubst models incorrectly combined rules and committees. (#134)

# tidypredict 0.5.1

- Exported a number of internal functions to be used in {orbital} package

# tidypredict 0.5

- Changes maintainer to Edgar Ruiz

- Updates author's email addresses.

- Removes dependency with `stringr`

- Fixes issue with `earth` parsed_models (#108)

- Addresses issues with XGBoost models

- Improvements to XGBoosts tests

# tidypredict 0.4.9

- Fixes issue handling GLM Binomial earth models (#97)

- Adds capability to handle single simple Cubist models (#57)

- Fixed parenthesis issue in the creation of the interval formula (#76)

- Fixed bug in SQL query generation for XGBoost models with objective `binary:logistic`.

- Re-licensed package from GPL-3 to MIT. See [consent from copyright holders here](https://github.com/tidymodels/tidypredict/issues/95).

# tidypredict 0.4.8

- CRAN submission for a broken test case. 

# tidypredict 0.4.7

- Change to with with version 5.1.2 and above of the `earth` package. As a result, `tidypredict` will only parse objects created by this and later versions of `earth`. 

# tidypredict 0.4.6

- Small release for `xgboost` changes. 

# tidypredict 0.4.5

- Switches maintainer to Max Kuhn

# tidypredict 0.4.3

- Adds support for categorical predictors in `partykit`

- Fixes `parsnip` tests to meet standards of new CRAN version

# tidypredict 0.4.2

- Simplifies tests that verify `ranger` 

- Adds fit method for parsed `xgboost` models

- Sets conditional requirement for `xgboost`, for test and vignette 

# tidypredict 0.4.0

## New features

- Parses `ranger` classification models.

- Adds method support for `broom`'s `tidy()` function.  Regression models only

- Adds `as_parsed_model()` function. It adds the proper class components to the list.

- Adds initial support for `partykit`'s `ctree()` model

- Adds support for `parsnip` fitted models: `lm`, `randomForest`, `ranger`, and `earth`

- Adds support for xgb.Booster models provided by the `xgboost` package (@Athospd, #43)

- Adds support for `Cubist::cubist()` models (# 36)

# tidypredict 0.3.0

## New features

- Adds support for MARS models provided by the `earth` package

## Improvements

- New parsed models are now list objects as opposed to data frames.

- tidypredict_to_column() no longer supports `ranger` and `randomForest` because of the multiple queries generated by multiple trees.

- All functions that read the parsed models and create the tidy eval formula now use the list object.  

- Most of the code that depends on dplyr programming has been removed.

- Removes dependencies on: tidyr, tibble

- The `x/y` interface for `earth` models can now be used. 

## Bug Fixes

- It now returns all of the trees instead of just one for tree based models (`randomForest` & `ranger`) (#29)

# tidypredict 0.2.1

## Bug Fixes

- tibble 2.0.0 compatibility fix (@krlmlr)

# tidypredict 0.2.0

## New features

- Add support for `ranger()` models.

## Bug fixes

- Using `x ~.` in a randomForest() formula fails (#18 @washcycle).
