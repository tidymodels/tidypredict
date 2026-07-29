# a single predictor is handled

    Code
      rlang::expr_text(tidypredict_fit(model)[["setosa"]])
    Output
      [1] "exp(-1.09861228866811 + (2.25012937537181 - ((Petal.Width - 0.246)^2/0.0222122448979592)))/(exp(-1.09861228866811 + \n    (2.25012937537181 - ((Petal.Width - 0.246)^2/0.0222122448979592))) + \n    exp(-1.09861228866811 + (1.620738119938 - ((Petal.Width - \n        1.326)^2/0.0782122448979591))) + exp(-1.09861228866811 + \n    (1.29225751662055 - ((Petal.Width - 2.026)^2/0.150865306122449))))"

# kernel density fits are rejected

    Code
      tidypredict_fit(model)
    Condition
      Error in `parse_model()`:
      ! `tidypredict_fit()` does not support `klaR::NaiveBayes()` models fit with kernel density estimates.
      i Refit with `usekernel = FALSE`.

---

    Code
      parse_model(model)
    Condition
      Error in `parse_model()`:
      ! `tidypredict_fit()` does not support `klaR::NaiveBayes()` models fit with kernel density estimates.
      i Refit with `usekernel = FALSE`.

# tidypredict_test errors for NaiveBayes models

    Code
      tidypredict_test(model, iris)
    Condition
      Error in `tidypredict_test()`:
      ! `tidypredict_test()` does not support `klaR::NaiveBayes()` models.
      i Use `tidypredict_fit()` directly for multiclass predictions.

