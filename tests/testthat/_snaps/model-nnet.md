# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "0.7 + (case_when(0.1 + (wt * 0.2 + hp * 0.3) < -15 ~ 0, 0.1 + \n    (wt * 0.2 + hp * 0.3) > 15 ~ 1, .default = 1/(1 + exp(-(0.1 + \n    (wt * 0.2 + hp * 0.3))))) * 0.8 + case_when(0.4 + (wt * 0.5 + \n    hp * 0.6) < -15 ~ 0, 0.4 + (wt * 0.5 + hp * 0.6) > 15 ~ 1, \n    .default = 1/(1 + exp(-(0.4 + (wt * 0.5 + hp * 0.6))))) * \n    0.9)"

# multiple non classification outputs are rejected

    Code
      tidypredict_fit(model)
    Condition
      Error in `parse_model()`:
      ! `tidypredict_fit()` does not support `nnet::nnet()` models with multiple outputs that are not a classification.

# tidypredict_test errors for classification nnet models

    Code
      tidypredict_test(model, iris)
    Condition
      Error in `tidypredict_test()`:
      ! `tidypredict_test()` does not support classification `nnet::nnet()` models.
      i Use `tidypredict_fit()` directly for class predictions.

# inline functions in the formula are rejected

    Code
      tidypredict_fit(model)
    Condition
      Error in `acceptable_lm()`:
      x Functions inside the formula are not supported.
      i Functions detected: "log".  Use `dplyr` transformations to prepare the data.

