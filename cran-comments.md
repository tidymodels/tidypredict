## Release summary

* Adds method support for `broom`'s `tidy()` function.  Regression models only
* Adds `as_parsed_model()` function. It adds the proper class components to the list
* Adds initial support for `partykit`'s `ctree()` model
* Adds support for `parsnip` fitted models: `lm`, `randomForest`, `ranger`, and `earth`
* Adds support for xgb.Booster models provided by the `xgboost` package
* Adds support for `Cubist::cubist()` models 

## Test environments
* Local windows 10 install, R 3.6.0
* Ubuntu 18.04.2 LTS, R 3.6.1
* Ubuntu 14.04 (on travis-ci)

## R CMD check results
* 0 errors | 0 warnings | 0 notes
