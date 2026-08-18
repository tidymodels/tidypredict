# returns the right output

    Code
      round_print(tf)
    Output
      [1] "24.29374 + (ifelse(is.na(cyl), 6.1875, cyl) * -0.8899314) + (ifelse(is.na(disp), 230.7219, disp) * -0.01305656) + (ifelse(is.na(hp), 146.6875, hp) * -0.02281096) + (ifelse(is.na(drat), 3.596562, drat) * 2.130328)"

# training data containing NA is rejected

    Code
      tidypredict_fit(model)
    Condition
      Error in `parse_model_mixomics()`:
      ! Models fit on data with missing values are not supported.

# tidypredict_test errors for discriminant and multivariate models

    Code
      tidypredict_test(da, iris)
    Condition
      Error in `tidypredict_test()`:
      ! `tidypredict_test()` does not support this mixOmics model.
      i Use `tidypredict_fit()` directly for multiclass predictions.

---

    Code
      tidypredict_test(mv, mtcars)
    Condition
      Error in `tidypredict_test()`:
      ! `tidypredict_test()` does not support this mixOmics model.
      i Use `tidypredict_fit()` directly for multivariate outcomes.

