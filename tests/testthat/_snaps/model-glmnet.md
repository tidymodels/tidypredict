# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "35.3137765116027 + (cyl * -0.871451193824228) + (hp * -0.0101173960249783) + \n    (wt * -2.59443677687505)"

# formulas produces correct predictions

    Code
      tidypredict_test(glmnet::glmnet(mtcars[, -1], mtcars$mpg, lambda = 1), mtcars[,
        -1])
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

