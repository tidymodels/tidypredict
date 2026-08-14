# returns the right output

    Code
      round_print(tf)
    Output
      [1] "35.31378 + (cyl * -0.8714512) + (hp * -0.0101174) + (wt * -2.594437)"

# errors if more than 1 penalty is selected

    Code
      tidypredict_fit(model)
    Condition
      Error in `parse_model()`:
      ! `tidypredict_fit()` requires that there are only 1 penalty selected, 79 were provided.

---

    Code
      tidypredict_fit(model)
    Condition
      Error in `parse_model()`:
      ! `tidypredict_fit()` requires that there are only 1 penalty selected, 2 were provided.

# rejects a model fit with an offset (#296)

    Code
      tidypredict_fit(model)
    Condition
      Error in `parse_model()`:
      ! Models fit with an `offset` are not supported for glmnet.
      i glmnet stores only a flag, not the offset values, so the prediction cannot be reproduced.

---

    Code
      parse_model(model)
    Condition
      Error in `parse_model()`:
      ! Models fit with an `offset` are not supported for glmnet.
      i glmnet stores only a flag, not the offset values, so the prediction cannot be reproduced.

---

    Code
      tidypredict_fit(model)
    Condition
      Error in `parse_model()`:
      ! Models fit with an `offset` are not supported for glmnet.
      i glmnet stores only a flag, not the offset values, so the prediction cannot be reproduced.

# glmnet are handeld neatly with parsnip

    Code
      round_print(tf)
    Output
      [1] "35.31405 + (cyl * -0.8716234) + (hp * -0.01011579) + (wt * -2.594265)"

# multinomial family is supported (#198)

    Code
      lapply(lps, round_print)
    Output
      [[1]]
      [1] "2.863134 + (Sepal.Width * 0.7426522) + (Petal.Length * -1.359936)"
      
      [[2]]
      [1] "1.384489 + (Sepal.Width * -0.05721863)"
      
      [[3]]
      [1] "-4.247622 + (Petal.Width * 3.329409)"
      

# multinomial errors with multiple penalties

    Code
      tidypredict_fit(model)
    Condition
      Error in `parse_model()`:
      ! `tidypredict_fit()` requires that there are only 1 penalty selected, 100 were provided.

# tidypredict_test errors for multinomial models

    Code
      tidypredict_test(model, iris[, 1:4])
    Condition
      Error in `tidypredict_test()`:
      ! `tidypredict_test()` does not support multinomial glmnet models.
      i Use `tidypredict_fit()` directly for multiclass predictions.

# mgaussian family errors with helpful message (#199)

    Code
      tidypredict_fit(model)
    Condition
      Error in `tidypredict_fit()`:
      ! Multivariate gaussian glmnet models are not supported.
      i Models fit with `family = "mgaussian"` have multiple outcome columns which is not supported.

# .extract_glmnet_multiclass errors on non-multnet model

    Code
      .extract_glmnet_multiclass(model)
    Condition
      Error in `.extract_glmnet_multiclass()`:
      ! `model` must be <multnet>, not an <elnet> object.

# .extract_glmnet_multiclass errors with multiple penalties

    Code
      .extract_glmnet_multiclass(model)
    Condition
      Error in `.extract_glmnet_multiclass()`:
      ! glmnet model has multiple penalty values.
      i Specify a single `penalty` value.

