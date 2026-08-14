# a single predictor is handled

    Code
      round_print(tidypredict_fit(model)[["setosa"]])
    Output
      [1] "1/(1 + exp(-1.098612 + ifelse(is.na(Petal.Width), 0, 1.620738 - ((Petal.Width - 1.326)^2/0.07821224)) - (-1.098612 + ifelse(is.na(Petal.Width), 0, 2.250129 - ((Petal.Width - 0.246)^2/0.02221224)))) + exp(-1.098612 + ifelse(is.na(Petal.Width), 0, 1.292258 - ((Petal.Width - 2.026)^2/0.1508653)) - (-1.098612 + ifelse(is.na(Petal.Width), 0, 2.250129 - ((Petal.Width - 0.246)^2/0.02221224)))))"

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

# naive_bayes handles binary outcomes and a single predictor

    Code
      round_print(tf[["0"]])
    Output
      [1] "1/(1 + exp(-0.9007865 + ifelse(is.na(mpg), 0, -1.819132 - ((mpg - 24.39231)^2/76.05154)) - (-0.5212969 + ifelse(is.na(mpg), 0, -1.3439 - ((mpg - 17.14737)^2/29.3986)))))"

# naive_bayes kernel density fits are rejected

    Code
      tidypredict_fit(model)
    Condition
      Error in `parse_model()`:
      ! `tidypredict_fit()` does not support `naivebayes::naive_bayes()` models fit with kernel density estimates.
      i Refit with `usekernel = FALSE`.

---

    Code
      parse_model(model)
    Condition
      Error in `parse_model()`:
      ! `tidypredict_fit()` does not support `naivebayes::naive_bayes()` models fit with kernel density estimates.
      i Refit with `usekernel = FALSE`.

# tidypredict_test errors for naive_bayes models

    Code
      tidypredict_test(model, iris)
    Condition
      Error in `tidypredict_test()`:
      ! `tidypredict_test()` does not support `naivebayes::naive_bayes()` models.
      i Use `tidypredict_fit()` directly for multiclass predictions.

