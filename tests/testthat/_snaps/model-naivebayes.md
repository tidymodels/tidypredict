# a single predictor is handled

    Code
      round_print(tidypredict_fit(model)[["setosa"]])
    Output
      [1] "exp(-1.098612 + (2.250129 - ((Petal.Width - 0.246)^2/0.02221224)))/(exp(-1.098612 + (2.250129 - ((Petal.Width - 0.246)^2/0.02221224))) + exp(-1.098612 + (1.620738 - ((Petal.Width - 1.326)^2/0.07821224))) + exp(-1.098612 + (1.292258 - ((Petal.Width - 2.026)^2/0.1508653))))"

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

