# unsupported objective throws error

    Code
      tidypredict_fit(pm)
    Condition
      Error in `build_fit_formula_lgb()`:
      ! Unsupported objective: "unsupported_objective".
      i Supported objectives: "regression", "regression_l2", "regression_l1", "huber", "fair", "quantile", "mape", "poisson", "gamma", "tweedie", "binary", "cross_entropy", "multiclass", and "multiclassova".

# empty trees throws error

    Code
      tidypredict_fit(pm)
    Condition
      Error in `build_fit_formula_lgb()`:
      ! Model has no trees.

# multiclass with num_class < 2 throws error

    Code
      tidypredict_fit(pm)
    Condition
      Error in `build_fit_formula_lgb_multiclass()`:
      ! Multiclass model must have num_class >= 2.

# multiclass with NULL num_class throws error

    Code
      tidypredict_fit(pm)
    Condition
      Error in `build_fit_formula_lgb_multiclass()`:
      ! Multiclass model must have num_class >= 2.

# get_lgb_case_fun errors on unknown type

    Code
      get_lgb_case_fun(condition)
    Condition
      Error in `get_lgb_case_fun()`:
      ! Unknown condition type: "unknown_type"

# get_lgb_case_fun errors on unknown set operator

    Code
      get_lgb_case_fun(condition)
    Condition
      Error in `build_lgb_set_expr()`:
      ! Unknown operator for set: "unknown_op"

# tidypredict_test errors for multiclass model

    Code
      tidypredict_test(model, xg_df = X)
    Condition
      Error in `lgb_booster()`:
      ! tidypredict_test does not support multiclass LightGBM models.
      i Use tidypredict_fit() directly for multiclass predictions.

# tidypredict_test errors when matrix not provided

    Code
      tidypredict_test(model)
    Condition
      Error in `lgb_booster()`:
      ! LightGBM models require a matrix for predictions.
      i Pass the prediction matrix via the `xg_df` argument.

# .extract_lgb_trees errors on non-lgb.Booster

    Code
      .extract_lgb_trees(list())
    Condition
      Error in `.extract_lgb_trees()`:
      ! `model` must be <lgb.Booster>, not an empty list.

