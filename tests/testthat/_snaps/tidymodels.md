# works with parsnip model specification

    Code
      tidypredict_test(model, df = etitanic_fac)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(model, df = etitanic_fac)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

# works with decision_tree() and the C5.0 engine

    Code
      tidypredict_test(model, df = df)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

# works with C5_rules() and the C5.0 engine

    Code
      tidypredict_test(model, df = iris)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

# works with boost_tree() and the C5.0 engine

    Code
      tidypredict_test(model, df = iris)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

# works with linear_reg() and the glm engine

    Code
      tidypredict_test(model, df = mtcars)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

# works with decision_tree() and the rpart engine

    Code
      tidypredict_test(reg, df = mtcars)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

# works with rand_forest() and the partykit engine

    Code
      tidypredict_test(reg, df = mtcars)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_fit(cls)
    Condition
      Error in `cforest_check_regression()`:
      ! Classification models are not supported for cforest.
      i Only regression models can be converted to tidy formulas.
      i Classification requires a voting mechanism that cannot be expressed as a single formula.

# works with rand_forest() and the aorsf engine

    Code
      tidypredict_fit(cls)
    Condition
      Error in `aorsf_check_supported()`:
      ! Classification models are not supported for aorsf.
      i Only regression models can be converted to tidy formulas.
      i Classification requires a voting mechanism that cannot be expressed as a single formula.

# works with linear_reg() and the quantreg engine

    Code
      tidypredict_test(model, df = mtcars)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

