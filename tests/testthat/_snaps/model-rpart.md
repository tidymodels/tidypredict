# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(cyl > 7 & cyl > 5 ~ 15.1, cyl <= 7 & cyl > 5 ~ 19.7428571428571, \n    .default = 26.6636363636364)"

# formulas produce correct predictions - regression

    Code
      tidypredict_test(rpart::rpart(mpg ~ am + cyl + wt, data = mtcars), mtcars)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

# formulas produce correct predictions - classification

    Code
      tidypredict_test(rpart::rpart(Species ~ ., data = iris), iris)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

# categorical predictors work correctly

    Code
      tidypredict_test(rpart::rpart(mpg ~ cyl + wt, data = mtcars2), mtcars2)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

