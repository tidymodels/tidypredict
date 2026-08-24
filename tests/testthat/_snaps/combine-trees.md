# check_trees_arg() rejects a non-list and an empty list

    Code
      check_trees_arg(1:3)
    Condition
      Error:
      ! `trees` must be a non-empty list of expressions, not an integer vector.
    Code
      check_trees_arg(list())
    Condition
      Error:
      ! `trees` must be a non-empty list of expressions, not an empty list.

# lightgbm refuses to combine a multiclass fit

    Code
      tidypredict_combine_trees(model, tidypredict_trees(model))
    Condition
      Error in `tidypredict_combine_trees()`:
      ! Multiclass lightgbm trees cannot be combined into one expression.
      i The fit is one expression per class.
      i Use `tidypredict_fit()` for the whole model instead.

# boosted C5.0 refuses to combine its trees

    Code
      tidypredict_combine_trees(model, list(1))
    Condition
      Error in `tidypredict_combine_trees()`:
      ! Boosted C5.0 trees cannot be recombined arithmetically.
      i Each trial votes with a class label and a confidence, so there are no per-tree numbers to sum or average.
      i Use `tidypredict_fit()` for the whole model instead.

