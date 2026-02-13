# unsupported objective throws error

    Code
      tidypredict_fit(pm)
    Condition
      Error in `build_fit_formula_xgb()`:
      ! Only objectives "binary:logistic", "reg:squarederror", "reg:logistic", "binary:logitraw" are supported yet.

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
      Error:
      ! ! Could not evaluate cli `{}` expression: `x`.
      Caused by error in `eval(expr, envir = envir)`:
      ! object 'x' not found

