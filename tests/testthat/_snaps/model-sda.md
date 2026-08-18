# returns the right output

    Code
      lapply(lps, round_print)
    Output
      [[1]]
      [1] "-13.15071 + (Sepal.Length * 5.732661) + (Sepal.Width * 11.36043) + (Petal.Length * -16.80848) + (Petal.Width * -16.62401)"
      
      [[2]]
      [1] "-2.174039 + (Sepal.Length * -1.383276) + (Sepal.Width * -4.206341) + (Petal.Length * 4.571061) + (Petal.Width * 2.497056)"
      
      [[3]]
      [1] "-32.23721 + (Sepal.Length * -4.349385) + (Sepal.Width * -7.154089) + (Petal.Length * 12.23742) + (Petal.Width * 14.12696)"
      

# tidypredict_test errors for sda models

    Code
      tidypredict_test(model, iris)
    Condition
      Error in `tidypredict_test()`:
      ! `tidypredict_test()` does not support `sda::sda()` models.
      i Use `tidypredict_fit()` directly for multiclass predictions.

# an ordered factor is rejected with parsnip (#393)

    Code
      tidypredict_fit(model)
    Condition
      Error in `acceptable_ordered()`:
      ! The treatment contrast is the only one supported at this time. Field(s) with an invalid contrast are: "f".

