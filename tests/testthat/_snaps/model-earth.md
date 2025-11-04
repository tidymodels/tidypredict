# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "20.534817535821 + (ifelse(disp < 145, 145 - disp, 0) * 0.148589866311) + \n    (ifelse(disp > 145, disp - 145, 0) * -0.025012854678)"

# formulas produces correct predictions

    Code
      tidypredict_test(earth::earth(age ~ sibsp + parch, data = earth::etitanic),
      earth::etitanic)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(age ~ sibsp + parch, data = earth::etitanic,
      degree = 2), earth::etitanic)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(age ~ sibsp + parch, data = earth::etitanic,
      degree = 3), earth::etitanic)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(age ~ ., data = earth::etitanic), earth::etitanic)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(age ~ ., data = earth::etitanic, pmethod = "backward"),
      earth::etitanic)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(age ~ ., data = earth::etitanic, pmethod = "none"),
      earth::etitanic)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(age ~ ., data = earth::etitanic, pmethod = "exhaustive"),
      earth::etitanic)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(age ~ ., data = earth::etitanic, pmethod = "forward"),
      earth::etitanic)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(age ~ ., data = earth::etitanic, pmethod = "seqrep"),
      earth::etitanic)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(survived ~ age + sibsp, data = earth::etitanic,
      glm = list(family = binomial)), earth::etitanic)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(survived ~ age + sibsp, data = earth::etitanic,
      glm = list(family = binomial), degree = 2), earth::etitanic)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(survived ~ ., data = earth::etitanic, glm = list(
        family = binomial), pmethod = "backward"), earth::etitanic)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(survived ~ ., data = earth::etitanic, glm = list(
        family = binomial), pmethod = "none"), earth::etitanic)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(survived ~ ., data = earth::etitanic, glm = list(
        family = binomial), pmethod = "exhaustive"), earth::etitanic)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(survived ~ ., data = earth::etitanic, glm = list(
        family = binomial), pmethod = "forward"), earth::etitanic)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(survived ~ ., data = earth::etitanic, glm = list(
        family = binomial), pmethod = "seqrep"), earth::etitanic)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(Sepal.Length ~ ., data = iris), iris)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(x = iris[, -1], y = iris$Sepal.Length), iris)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(Sepal.Length ~ ., data = iris, degree = 2,
      pmethod = "none"), iris)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(earth::earth(x = iris[, -1], y = iris$Sepal.Length, degree = 2,
      pmethod = "none"), iris)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

