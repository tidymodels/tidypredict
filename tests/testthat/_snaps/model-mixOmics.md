# returns the right output

    Code
      round_print(tf)
    Output
      [1] "24.29374 + (cyl * -0.8899314) + (disp * -0.01305656) + (hp * -0.02281096) + (drat * 2.130328)"

# an ordered factor is rejected with parsnip (#393)

    Code
      tidypredict_fit(model)
    Condition
      Error in `acceptable_ordered()`:
      ! The treatment contrast is the only one supported at this time. Field(s) with an invalid contrast are: "f".

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

