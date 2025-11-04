# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "1.520331147866 + (wt * -0.372988616484) + (cyl * 0.013885491477)"

# formulas produces correct predictions

    Code
      tidypredict_test(glm(am ~ wt + cyl + disp, data = mtcars, family = "gaussian"),
      mtcars)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(glm(am ~ wt + cyl + disp, data = mtcars, family = "binomial"),
      mtcars)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(glm(am ~ wt * cyl + disp, data = mtcars, family = "gaussian"),
      mtcars)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(glm(am ~ wt * cyl + disp, data = mtcars, family = "binomial"),
      mtcars)
    Condition
      Warning:
      glm.fit: fitted probabilities numerically 0 or 1 occurred
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(glm(am ~ wt:cyl + disp, data = mtcars, family = "gaussian"),
      mtcars)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(glm(am ~ wt:cyl + disp, data = mtcars, family = "binomial"),
      mtcars)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

# tidypredict works when variable names are subset of other variables

    Code
      tidypredict_test(model, mtcars)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

