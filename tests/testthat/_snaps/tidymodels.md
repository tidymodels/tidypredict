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

# works with linear_reg() and the quantreg engine

    Code
      tidypredict_test(model, df = mtcars)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

