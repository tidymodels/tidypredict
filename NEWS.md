# tidypredict (development version)

- Speed up `tidypredict_fit()` for partykit and ranger packages. (#125)

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
