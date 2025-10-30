# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(Petal.Length < 2.6 ~ \"setosa\", Sepal.Length < 6.25 & \n    Petal.Length >= 2.6 ~ \"versicolor\", Sepal.Length >= 6.25 & \n    Petal.Length >= 2.6 ~ \"virginica\") + case_when(Petal.Width < \n    0.75 ~ \"setosa\", Petal.Width < 1.75 & Petal.Width >= 0.75 ~ \n    \"versicolor\", Petal.Width >= 1.75 & Petal.Width >= 0.75 ~ \n    \"virginica\") + case_when(Petal.Length < 2.35 ~ \"setosa\", \n    Petal.Length < 4.75 & Petal.Length >= 2.35 ~ \"versicolor\", \n    Petal.Length >= 4.75 & Petal.Length >= 2.35 ~ \"virginica\")"

# Model can be saved and re-loaded

    Code
      tidypredict_fit(pm)
    Output
      [[1]]
      case_when(Petal.Length < 2.6 ~ "setosa", Sepal.Length < 6.25 & 
          Petal.Length >= 2.6 ~ "versicolor", Sepal.Length >= 6.25 & 
          Petal.Length >= 2.6 ~ "virginica")
      
      [[2]]
      case_when(Petal.Width < 0.75 ~ "setosa", Petal.Width < 1.75 & 
          Petal.Width >= 0.75 ~ "versicolor", Petal.Width >= 1.75 & 
          Petal.Width >= 0.75 ~ "virginica")
      
      [[3]]
      case_when(Petal.Length < 2.35 ~ "setosa", Petal.Length < 4.75 & 
          Petal.Length >= 2.35 ~ "versicolor", Petal.Length >= 4.75 & 
          Petal.Length >= 2.35 ~ "virginica")
      

# formulas produces correct predictions

    Code
      tidypredict_test(ranger::ranger(mpg ~ ., data = mtcars, num.trees = 3,
      max.depth = 2, seed = 100, num.threads = 2), mtcars)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

