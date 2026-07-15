# returns the right output

    Code
      round_print(tf)
    Output
      [1] "35.31378 + (cyl * -0.8714512) + (hp * -0.0101174) + (wt * -2.594437)"

# formulas produces correct predictions

    Code
      tidypredict_test(glmnet::glmnet(mtcars[, -1], mtcars$mpg, family = "gaussian",
      lambda = 1), mtcars[, -1])
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(glmnet::glmnet(mtcars[, -8], mtcars$vs, family = "binomial",
      lambda = 1), mtcars[, -1])
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(glmnet::glmnet(mtcars[, -8], mtcars$vs, family = "poisson",
      lambda = 1), mtcars[, -1])
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

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

# glmnet are handeld neatly with parsnip

    Code
      round_print(tf)
    Output
      [1] "35.31405 + (cyl * -0.8716234) + (hp * -0.01011579) + (wt * -2.594265)"

# multinomial family errors with helpful message (#198)

    Code
      tidypredict_fit(model)
    Condition
      Error in `tidypredict_fit()`:
      ! Multinomial glmnet models are not supported.
      i Models fit with `family = "multinomial"` have multiple outcome columns which is not supported.

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

