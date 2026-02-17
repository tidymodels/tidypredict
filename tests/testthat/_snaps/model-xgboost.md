# unsupported objective throws error

    Code
      tidypredict_fit(pm)
    Condition
      Error in `build_fit_formula_xgb()`:
      ! Objective "unsupported_objective" is not supported.
      i Supported objectives: "binary:hinge", "binary:logistic", "binary:logitraw", "count:poisson", "reg:absoluteerror", "reg:gamma", "reg:logistic", "reg:pseudohubererror", "reg:squarederror", "reg:squaredlogerror", "reg:tweedie".
      i Multiclass objectives ("multi:softmax", "multi:softprob") are not supported.

# NULL objective warns user

    Code
      tidypredict_fit(pm)
    Condition
      Warning:
      If the objective is a custom function, please explicitly apply it to the output.
    Output
      case_when(TRUE ~ 5)

# tidypredict_test xg_df argument is required

    Code
      tidypredict_test(model, mtcars)
    Condition
      Error in `xgb.DMatrix()`:
      ! xgb.DMatrix does not support construction from NULL

# .extract_xgb_trees errors on non-xgb.Booster

    Code
      .extract_xgb_trees(list())
    Condition
      Error in `.extract_xgb_trees()`:
      ! `model` must be <xgb.Booster>, not an empty list.

