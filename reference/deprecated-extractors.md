# Deprecated model extractors

**\[deprecated\]**

These functions have been replaced by generics with documented return
shapes. See
[tidypredict_extractors](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md).

|  |  |
|----|----|
| Deprecated | Replacement |
| `.extract_xgb_trees()`, `.extract_lgb_trees()`, `.extract_catboost_trees()`, `.extract_rf_trees()`, `.extract_ranger_trees()` | [`tidypredict_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md) |
| `.extract_rf_classprob()`, `.extract_ranger_classprob()` | [`tidypredict_class_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md) |
| `.extract_rpart_classprob()`, `.extract_partykit_classprob()`, `.extract_earth_multiclass()`, `.extract_glmnet_multiclass()` | [`tidypredict_class_exprs()`](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md) |

## Usage

``` r
.extract_xgb_trees(model)

.extract_lgb_trees(model)

.extract_catboost_trees(model)

.extract_rf_trees(model)

.extract_ranger_trees(model)

.extract_rf_classprob(model)

.extract_ranger_classprob(model)

.extract_rpart_classprob(model)

.extract_partykit_classprob(model)

.extract_earth_multiclass(model)

.extract_glmnet_multiclass(model, penalty = NULL)
```

## Arguments

- model:

  A fitted model object.

- penalty:

  The penalty value to use for coefficient extraction.

## Value

The same values these functions have always returned. Note that
[`tidypredict_class_exprs()`](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md)
returns language objects where `.extract_earth_multiclass()` and
`.extract_glmnet_multiclass()` return strings.
