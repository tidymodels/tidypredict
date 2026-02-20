# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(cyl <= 5 ~ 26.6636363636364, .default = case_when(cyl <= \n    7 ~ 19.7428571428571, .default = 15.1))"

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
      Difference threshold: 0
      
       All results are within the difference threshold

# categorical predictors work correctly

    Code
      tidypredict_test(rpart::rpart(mpg ~ cyl + wt, data = mtcars2), mtcars2)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

# .extract_rpart_classprob errors on non-rpart model

    Code
      .extract_rpart_classprob(list())
    Condition
      Error in `.extract_rpart_classprob()`:
      ! `model` must be <rpart>, not an empty list.

# .extract_rpart_classprob errors on regression model

    Code
      .extract_rpart_classprob(model)
    Condition
      Error in `.extract_rpart_classprob()`:
      ! `model` must be a classification model (method = 'class').

