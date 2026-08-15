# returns the right output

    Code
      round_print(tf)
    Output
      [1] "24.29374 + (cyl * -0.8899314) + (disp * -0.01305656) + (hp * -0.02281096) + (drat * 2.130328)"

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

