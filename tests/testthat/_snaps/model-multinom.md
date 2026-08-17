# returns the right output

    Code
      lapply(lps, round_print)
    Output
      [[1]]
      [1] "0"
      
      [[2]]
      [1] "18.69037 + (Sepal.Length * -5.458424) + (Sepal.Width * -8.707401) + (Petal.Length * 14.24477) + (Petal.Width * -3.097684)"
      
      [[3]]
      [1] "-23.83628 + (Sepal.Length * -7.923634) + (Sepal.Width * -15.37077) + (Petal.Length * 23.65978) + (Petal.Width * 15.1353)"
      

# an ordered predictor factor is rejected

    Code
      tidypredict_fit(model)
    Condition
      Error in `acceptable_lm()`:
      ! The treatment contrast is the only one supported at this time. Field(s) with an invalid contrast are: "f".

# tidypredict_test errors for multinom models

    Code
      tidypredict_test(model, iris)
    Condition
      Error in `tidypredict_test()`:
      ! `tidypredict_test()` does not support `nnet::multinom()` models.
      i Use `tidypredict_fit()` directly for multiclass predictions.

# inline functions in the formula are rejected

    Code
      tidypredict_fit(model)
    Condition
      Error in `acceptable_lm()`:
      x Functions inside the formula are not supported.
      i Functions detected: "log".  Use `dplyr` transformations to prepare the data.

