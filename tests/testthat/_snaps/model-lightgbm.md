# a categorical split with default_left set is refused (#288)

    Code
      tidypredict:::get_lgb_tree(tree_df)
    Condition
      Error in `map()`:
      i In index: 1.
      Caused by error in `check_lgb_categorical_default_left()`:
      ! A categorical split cannot set default_left.
      i This is an internal error that was detected in the tidypredict package.
        Please report it at <https://github.com/tidymodels/tidypredict/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

---

    Code
      tidypredict:::build_nested_lgb_tree(tree_df)
    Condition
      Error in `check_lgb_categorical_default_left()`:
      ! A categorical split cannot set default_left.
      i This is an internal error that was detected in the tidypredict package.
        Please report it at <https://github.com/tidymodels/tidypredict/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

# unsupported objective throws error

    Code
      tidypredict_fit(pm)
    Condition
      Error in `lgb_check_objective()`:
      ! Unsupported objective: "unsupported_objective".
      i Supported objectives: "regression", "regression_l2", "regression_l1", "huber", "fair", "quantile", "mape", "poisson", "gamma", "tweedie", "binary", "cross_entropy", "multiclass", and "multiclassova".

# empty trees throws error

    Code
      tidypredict_fit(pm)
    Condition
      Error in `assemble_lgb_formula()`:
      ! Model has no trees.

# multiclass with num_class < 2 throws error

    Code
      tidypredict_fit(pm)
    Condition
      Error in `lgb_combine()`:
      ! Multiclass model must have num_class >= 2.

# multiclass with NULL num_class throws error

    Code
      tidypredict_fit(pm)
    Condition
      Error in `lgb_combine()`:
      ! Multiclass model must have num_class >= 2.

# build_lgb_nested_condition errors on unknown type

    Code
      tidypredict:::build_lgb_nested_condition(condition)
    Condition
      Error in `tidypredict:::build_lgb_nested_condition()`:
      ! Unknown path element type: "unknown_type"

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

# tidypredict_trees errors on non-lgb.Booster

    Code
      tidypredict_trees(list())
    Condition
      Error in `tidypredict_trees()`:
      ! `tidypredict_trees()` is not available for models of class <list>.

