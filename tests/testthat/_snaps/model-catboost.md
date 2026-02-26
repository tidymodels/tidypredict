# unsupported objective throws error

    Code
      tidypredict_fit(pm)
    Condition
      Error in `build_fit_formula_catboost_nested()`:
      ! Unsupported objective: "UnsupportedObjective".
      i Supported objectives: "RMSE", "MAE", "Quantile", "MAPE", "Poisson", "Huber", "LogCosh", "Expectile", "Tweedie", "Logloss", "CrossEntropy", "MultiClass", and "MultiClassOneVsAll".

# empty trees throws error

    Code
      tidypredict_fit(pm)
    Condition
      Error in `build_fit_formula_catboost_nested()`:
      ! Model has no trees.

# tidypredict_test requires matrix

    Code
      tidypredict_test(model)
    Condition
      Error in `catboost_model()`:
      ! CatBoost models require a matrix for predictions.
      i Pass the prediction matrix via the `xg_df` argument.

# .extract_catboost_trees errors on non-catboost model

    Code
      .extract_catboost_trees(lm(mpg ~ wt, data = mtcars))
    Condition
      Error in `.extract_catboost_trees()`:
      ! `model` must be <catboost.Model>, not a <lm> object.

# multiclass model requires num_class >= 2

    Code
      tidypredict_fit(pm)
    Condition
      Error in `build_fit_formula_catboost_multiclass_nested()`:
      ! Multiclass model must have num_class >= 2.

# set_catboost_categories validates parsed_model argument

    Code
      set_catboost_categories("not a parsed model", model, data.frame())
    Condition
      Error in `set_catboost_categories()`:
      ! `parsed_model` must be a parsed CatBoost model.

# set_catboost_categories validates model argument

    Code
      set_catboost_categories(pm, "not a model", data.frame())
    Condition
      Error in `set_catboost_categories()`:
      ! `model` must be a <catboost.Model>, not a string.

# set_catboost_categories errors when column not found in data

    Code
      set_catboost_categories(pm, model, wrong_data)
    Condition
      Error in `set_catboost_categories()`:
      ! Column "cat_feat" not found in `data`.

# set_catboost_categories errors when column is not a factor

    Code
      set_catboost_categories(pm, model, wrong_data)
    Condition
      Error in `set_catboost_categories()`:
      ! Column "cat_feat" must be a factor.

# categorical features without mapping throws error

    Code
      tidypredict_fit(pm)
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `build_nested_catboost_categorical()`:
      ! No category mapping found for hash -254607792.
      i For raw CatBoost models, use `set_catboost_categories()`.

# parsnip model without xlevels throws error

    Code
      tidypredict_fit(model_fit)
    Condition
      Error in `setup_catboost_parsnip_categories()`:
      ! Model has categorical features but no factor level information.
      i Ensure the model was fit with factor columns, not character columns.

