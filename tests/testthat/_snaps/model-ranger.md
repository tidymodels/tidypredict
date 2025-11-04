# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(Petal.Length < 2.6 ~ \"setosa\", Sepal.Length < 6.25 & \n    Petal.Length >= 2.6 ~ \"versicolor\", .default = \"virginica\") + \n    case_when(Petal.Width < 0.75 ~ \"setosa\", Petal.Width < 1.75 & \n        Petal.Width >= 0.75 ~ \"versicolor\", .default = \"virginica\") + \n    case_when(Petal.Length < 2.35 ~ \"setosa\", Petal.Length < \n        4.75 & Petal.Length >= 2.35 ~ \"versicolor\", .default = \"virginica\")"

# formulas produces correct predictions

    Code
      tidypredict_test(ranger::ranger(mpg ~ ., data = mtcars, num.trees = 3,
      max.depth = 2, seed = 100, num.threads = 2), mtcars)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

