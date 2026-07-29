# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "20.090625"

# regression predictions match native predict

    Code
      tidypredict_test(model, mtcars)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

# tidypredict_test errors for classification nullmodel

    Code
      tidypredict_test(model, iris)
    Condition
      Error in `tidypredict_test()`:
      ! `tidypredict_test()` does not support classification `parsnip::nullmodel()` models.
      i Use `tidypredict_fit()` directly for class probabilities.

