# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "35.3137765116027 + (cyl * -0.871451193824228) + (hp * -0.0101173960249783) + \n    (wt * -2.59443677687505)"

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
      rlang::expr_text(tf)
    Output
      [1] "35.3140536966127 + (cyl * -0.871623418095165) + (hp * -0.0101157918502673) + \n    (wt * -2.59426484734253)"

