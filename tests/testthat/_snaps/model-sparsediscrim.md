# returns the right output

    Code
      lapply(lps, round_print)
    Output
      [[1]]
      [1] "-107.9308 + (Sepal.Length * 19.27549) + (Sepal.Width * 30.31482) + (Petal.Length * 8.055807) + (Petal.Width * 5.993568)"
      
      [[2]]
      [1] "-174.2806 + (Sepal.Length * 22.85644) + (Sepal.Width * 24.49593) + (Petal.Length * 23.47314) + (Petal.Width * 32.30679)"
      
      [[3]]
      [1] "-258.6928 + (Sepal.Length * 25.36695) + (Sepal.Width * 26.29996) + (Petal.Length * 30.59223) + (Petal.Width * 49.36166)"
      

# inline formula functions are rejected

    Code
      parse_model(model)
    Condition
      Error in `acceptable_lm()`:
      x Functions inside the formula are not supported.
      i Functions detected: "log".  Use `dplyr` transformations to prepare the data.

# tidypredict_test errors for sparsediscrim models

    Code
      tidypredict_test(model, iris)
    Condition
      Error in `tidypredict_test()`:
      ! `tidypredict_test()` does not support sparsediscrim models.
      i Use `tidypredict_fit()` directly for multiclass predictions.

