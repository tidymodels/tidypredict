# Set categorical feature mappings for CatBoost model

CatBoost stores categorical features as hash values internally. This
function establishes the mapping between hash values and category names
by examining a data frame with the same factor columns used during
training.

## Usage

``` r
set_catboost_categories(parsed_model, model, data)
```

## Arguments

- parsed_model:

  A parsed CatBoost model from
  [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)

- model:

  The original CatBoost model object

- data:

  A data frame containing factor columns matching the categorical
  features used in the model. The factor levels must match those from
  training.

## Value

The parsed model with category mappings added

## Details

This function is only needed when using raw CatBoost models (trained
with `catboost.train()`). When using parsnip/bonsai, categorical
features are handled automatically and this function is not required.

## Examples

``` r
# For raw CatBoost models with categorical features:
pm <- parse_model(catboost_model)
#> Error: object 'catboost_model' not found
pm <- set_catboost_categories(pm, catboost_model, training_data)
#> Error: object 'pm' not found
tidypredict_fit(pm)
#> Error: object 'pm' not found

# For parsnip/bonsai models, this is not needed:
# tidypredict_fit(parsnip_model_fit)  # works automatically
```
