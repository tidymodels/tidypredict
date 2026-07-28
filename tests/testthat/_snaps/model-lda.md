# returns the right output

    Code
      lapply(lps, round_print)
    Output
      [[1]]
      [1] "-15.47784 + (Sepal.Length * 6.314758) + (Sepal.Width * 12.13932) + (Petal.Length * -16.94642) + (Petal.Width * -20.77005)"
      
      [[2]]
      [1] "-2.021974 + (Sepal.Length * -1.531199) + (Sepal.Width * -4.376043) + (Petal.Length * 4.695665) + (Petal.Width * 3.062585)"
      
      [[3]]
      [1] "-33.53769 + (Sepal.Length * -4.783559) + (Sepal.Width * -7.763274) + (Petal.Length * 12.25076) + (Petal.Width * 17.70747)"
      

# tidypredict_test errors for lda models

    Code
      tidypredict_test(model, iris)
    Condition
      Error in `tidypredict_test()`:
      ! `tidypredict_test()` does not support `MASS::lda()` models.
      i Use `tidypredict_fit()` directly for multiclass predictions.

# inline functions in the formula are rejected

    Code
      tidypredict_fit(model)
    Condition
      Error in `acceptable_lm()`:
      x Functions inside the formula are not supported.
      i Functions detected: "log".  Use `dplyr` transformations to prepare the data.

