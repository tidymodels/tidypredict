# Returns expected case_when() dplyr formula

    Code
      rlang::expr_text(tf[[1]])
    Output
      [1] "case_when(Petal.Length < 2.5 ~ \"setosa\", Petal.Length >= 5.05 & \n    Petal.Length >= 2.5 ~ \"virginica\", Petal.Width >= 1.9 & Petal.Length < \n    5.05 & Petal.Length >= 2.5 ~ \"virginica\", Sepal.Length < \n    4.95 & Petal.Width < 1.9 & Petal.Length < 5.05 & Petal.Length >= \n    2.5 ~ \"virginica\", Petal.Width < 1.75 & Sepal.Length >= 4.95 & \n    Petal.Width < 1.9 & Petal.Length < 5.05 & Petal.Length >= \n    2.5 ~ \"versicolor\", Sepal.Width < 3 & Petal.Width >= 1.75 & \n    Sepal.Length >= 4.95 & Petal.Width < 1.9 & Petal.Length < \n    5.05 & Petal.Length >= 2.5 ~ \"virginica\", Sepal.Width >= \n    3 & Petal.Width >= 1.75 & Sepal.Length >= 4.95 & Petal.Width < \n    1.9 & Petal.Length < 5.05 & Petal.Length >= 2.5 ~ \"versicolor\")"

---

    Code
      rlang::expr_text(tf[[1]])
    Output
      [1] "case_when(Petal.Length < 2.5 ~ \"setosa\", Petal.Length >= 5.05 & \n    Petal.Length >= 2.5 ~ \"virginica\", Petal.Width >= 1.9 & Petal.Length < \n    5.05 & Petal.Length >= 2.5 ~ \"virginica\", Sepal.Length < \n    4.95 & Petal.Width < 1.9 & Petal.Length < 5.05 & Petal.Length >= \n    2.5 ~ \"virginica\", Petal.Width < 1.75 & Sepal.Length >= 4.95 & \n    Petal.Width < 1.9 & Petal.Length < 5.05 & Petal.Length >= \n    2.5 ~ \"versicolor\", Sepal.Width < 3 & Petal.Width >= 1.75 & \n    Sepal.Length >= 4.95 & Petal.Width < 1.9 & Petal.Length < \n    5.05 & Petal.Length >= 2.5 ~ \"virginica\", Sepal.Width >= \n    3 & Petal.Width >= 1.75 & Sepal.Length >= 4.95 & Petal.Width < \n    1.9 & Petal.Length < 5.05 & Petal.Length >= 2.5 ~ \"versicolor\")"

# Model can be saved and re-loaded

    Code
      tidypredict_fit(pm)
    Output
      [[1]]
      case_when(Petal.Length < 2.45 & Sepal.Length < 5.45 ~ "setosa", 
          Petal.Width < 1.6 & Petal.Length >= 2.45 & Sepal.Length < 
              5.45 ~ "versicolor", Petal.Width >= 1.6 & Petal.Length >= 
              2.45 & Sepal.Length < 5.45 ~ "virginica", Petal.Length >= 
              4.7 & Sepal.Length < 5.75 & Sepal.Length >= 5.45 ~ "virginica", 
          Petal.Width < 0.65 & Petal.Length < 4.7 & Sepal.Length < 
              5.75 & Sepal.Length >= 5.45 ~ "setosa", Petal.Width >= 
              0.65 & Petal.Length < 4.7 & Sepal.Length < 5.75 & Sepal.Length >= 
              5.45 ~ "versicolor", Petal.Length < 2.55 & Petal.Length < 
              4.95 & Sepal.Length >= 5.75 & Sepal.Length >= 5.45 ~ 
              "setosa", Petal.Length >= 5.05 & Petal.Length >= 4.95 & 
              Sepal.Length >= 5.75 & Sepal.Length >= 5.45 ~ "virginica", 
          Petal.Width < 1.7 & Petal.Length >= 2.55 & Petal.Length < 
              4.95 & Sepal.Length >= 5.75 & Sepal.Length >= 5.45 ~ 
              "versicolor", Petal.Width >= 1.7 & Petal.Length >= 2.55 & 
              Petal.Length < 4.95 & Sepal.Length >= 5.75 & Sepal.Length >= 
              5.45 ~ "virginica", Sepal.Length < 6.5 & Petal.Length < 
              5.05 & Petal.Length >= 4.95 & Sepal.Length >= 5.75 & 
              Sepal.Length >= 5.45 ~ "virginica", Sepal.Length >= 6.5 & 
              Petal.Length < 5.05 & Petal.Length >= 4.95 & Sepal.Length >= 
              5.75 & Sepal.Length >= 5.45 ~ "versicolor")
      
      [[2]]
      case_when(Petal.Width < 0.7 ~ "setosa", Sepal.Length >= 4.95 & 
          Petal.Length < 4.75 & Petal.Width >= 0.7 ~ "versicolor", 
          Petal.Length < 3.9 & Sepal.Length < 4.95 & Petal.Length < 
              4.75 & Petal.Width >= 0.7 ~ "versicolor", Petal.Length >= 
              3.9 & Sepal.Length < 4.95 & Petal.Length < 4.75 & Petal.Width >= 
              0.7 ~ "virginica", Sepal.Length >= 6.5 & Sepal.Width < 
              2.75 & Petal.Length >= 4.75 & Petal.Width >= 0.7 ~ "virginica", 
          Petal.Width >= 1.75 & Sepal.Width >= 2.75 & Petal.Length >= 
              4.75 & Petal.Width >= 0.7 ~ "virginica", Petal.Width >= 
              1.7 & Sepal.Length < 6.5 & Sepal.Width < 2.75 & Petal.Length >= 
              4.75 & Petal.Width >= 0.7 ~ "virginica", Sepal.Width < 
              2.9 & Petal.Width < 1.75 & Sepal.Width >= 2.75 & Petal.Length >= 
              4.75 & Petal.Width >= 0.7 ~ "virginica", Sepal.Width >= 
              2.9 & Petal.Width < 1.75 & Sepal.Width >= 2.75 & Petal.Length >= 
              4.75 & Petal.Width >= 0.7 ~ "versicolor", Petal.Length >= 
              5.05 & Petal.Width < 1.7 & Sepal.Length < 6.5 & Sepal.Width < 
              2.75 & Petal.Length >= 4.75 & Petal.Width >= 0.7 ~ "versicolor", 
          Sepal.Length < 6.15 & Petal.Length < 5.05 & Petal.Width < 
              1.7 & Sepal.Length < 6.5 & Sepal.Width < 2.75 & Petal.Length >= 
              4.75 & Petal.Width >= 0.7 ~ "virginica", Sepal.Length >= 
              6.15 & Petal.Length < 5.05 & Petal.Width < 1.7 & Sepal.Length < 
              6.5 & Sepal.Width < 2.75 & Petal.Length >= 4.75 & Petal.Width >= 
              0.7 ~ "versicolor")
      
      [[3]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Width >= 1.75 & 
          Petal.Width >= 0.8 ~ "virginica", Sepal.Length >= 7.1 & Petal.Width < 
          1.75 & Petal.Width >= 0.8 ~ "virginica", Petal.Length < 5 & 
          Petal.Width < 1.65 & Sepal.Length < 7.1 & Petal.Width < 1.75 & 
          Petal.Width >= 0.8 ~ "versicolor", Petal.Length >= 5 & Petal.Width < 
          1.65 & Sepal.Length < 7.1 & Petal.Width < 1.75 & Petal.Width >= 
          0.8 ~ "virginica", Sepal.Length < 5.8 & Petal.Width >= 1.65 & 
          Sepal.Length < 7.1 & Petal.Width < 1.75 & Petal.Width >= 
          0.8 ~ "virginica", Sepal.Length >= 5.8 & Petal.Width >= 1.65 & 
          Sepal.Length < 7.1 & Petal.Width < 1.75 & Petal.Width >= 
          0.8 ~ "versicolor")
      
      [[4]]
      case_when(Petal.Length < 2.45 ~ "setosa", Petal.Width < 1.65 & 
          Petal.Length < 4.85 & Petal.Length >= 2.45 ~ "versicolor", 
          Sepal.Length >= 6.05 & Petal.Length >= 4.85 & Petal.Length >= 
              2.45 ~ "virginica", Sepal.Width < 2.85 & Petal.Width >= 
              1.65 & Petal.Length < 4.85 & Petal.Length >= 2.45 ~ "virginica", 
          Sepal.Width >= 2.85 & Petal.Width >= 1.65 & Petal.Length < 
              4.85 & Petal.Length >= 2.45 ~ "versicolor", Sepal.Width >= 
              2.75 & Sepal.Length < 6.05 & Petal.Length >= 4.85 & Petal.Length >= 
              2.45 ~ "virginica", Petal.Width < 1.55 & Sepal.Width < 
              2.75 & Sepal.Length < 6.05 & Petal.Length >= 4.85 & Petal.Length >= 
              2.45 ~ "virginica", Petal.Width >= 1.55 & Sepal.Width < 
              2.75 & Sepal.Length < 6.05 & Petal.Length >= 4.85 & Petal.Length >= 
              2.45 ~ "versicolor")
      
      [[5]]
      case_when(Petal.Length < 2.5 & Sepal.Length < 5.55 ~ "setosa", 
          Sepal.Width < 2.45 & Petal.Length >= 2.5 & Sepal.Length < 
              5.55 ~ "versicolor", Petal.Width < 0.7 & Petal.Length < 
              4.75 & Sepal.Length >= 5.55 ~ "setosa", Petal.Width >= 
              0.7 & Petal.Length < 4.75 & Sepal.Length >= 5.55 ~ "versicolor", 
          Petal.Width < 1.6 & Sepal.Width >= 2.45 & Petal.Length >= 
              2.5 & Sepal.Length < 5.55 ~ "versicolor", Petal.Width >= 
              1.6 & Sepal.Width >= 2.45 & Petal.Length >= 2.5 & Sepal.Length < 
              5.55 ~ "virginica", Petal.Width < 1.65 & Petal.Length < 
              4.95 & Petal.Length >= 4.75 & Sepal.Length >= 5.55 ~ 
              "versicolor", Petal.Width >= 1.65 & Petal.Length < 4.95 & 
              Petal.Length >= 4.75 & Sepal.Length >= 5.55 ~ "virginica", 
          Petal.Width >= 1.7 & Petal.Length >= 4.95 & Petal.Length >= 
              4.75 & Sepal.Length >= 5.55 ~ "virginica", Sepal.Length >= 
              6.05 & Petal.Width < 1.7 & Petal.Length >= 4.95 & Petal.Length >= 
              4.75 & Sepal.Length >= 5.55 ~ "virginica", Petal.Length < 
              5.05 & Sepal.Length < 6.05 & Petal.Width < 1.7 & Petal.Length >= 
              4.95 & Petal.Length >= 4.75 & Sepal.Length >= 5.55 ~ 
              "virginica", Petal.Length >= 5.05 & Sepal.Length < 6.05 & 
              Petal.Width < 1.7 & Petal.Length >= 4.95 & Petal.Length >= 
              4.75 & Sepal.Length >= 5.55 ~ "versicolor")
      
      [[6]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Length >= 5.05 & 
          Sepal.Length >= 6.25 & Petal.Width >= 0.8 ~ "virginica", 
          Petal.Length < 4.85 & Petal.Width < 1.65 & Sepal.Length < 
              6.25 & Petal.Width >= 0.8 ~ "versicolor", Petal.Length >= 
              4.95 & Petal.Width >= 1.65 & Sepal.Length < 6.25 & Petal.Width >= 
              0.8 ~ "virginica", Petal.Width < 1.75 & Petal.Length < 
              5.05 & Sepal.Length >= 6.25 & Petal.Width >= 0.8 ~ "versicolor", 
          Petal.Width >= 1.75 & Petal.Length < 5.05 & Sepal.Length >= 
              6.25 & Petal.Width >= 0.8 ~ "virginica", Petal.Length >= 
              5.35 & Petal.Length >= 4.85 & Petal.Width < 1.65 & Sepal.Length < 
              6.25 & Petal.Width >= 0.8 ~ "virginica", Sepal.Width < 
              3 & Petal.Length < 4.95 & Petal.Width >= 1.65 & Sepal.Length < 
              6.25 & Petal.Width >= 0.8 ~ "virginica", Sepal.Width >= 
              3 & Petal.Length < 4.95 & Petal.Width >= 1.65 & Sepal.Length < 
              6.25 & Petal.Width >= 0.8 ~ "versicolor", Petal.Length < 
              5.05 & Petal.Length < 5.35 & Petal.Length >= 4.85 & Petal.Width < 
              1.65 & Sepal.Length < 6.25 & Petal.Width >= 0.8 ~ "virginica", 
          Petal.Length >= 5.05 & Petal.Length < 5.35 & Petal.Length >= 
              4.85 & Petal.Width < 1.65 & Sepal.Length < 6.25 & Petal.Width >= 
              0.8 ~ "versicolor")
      
      [[7]]
      case_when(Petal.Length < 2.35 ~ "setosa", Petal.Width < 1.35 & 
          Petal.Width < 1.75 & Petal.Length >= 2.35 ~ "versicolor", 
          Sepal.Length >= 6.1 & Petal.Width >= 1.75 & Petal.Length >= 
              2.35 ~ "virginica", Sepal.Width < 3.1 & Sepal.Length < 
              6.1 & Petal.Width >= 1.75 & Petal.Length >= 2.35 ~ "virginica", 
          Sepal.Width >= 3.1 & Sepal.Length < 6.1 & Petal.Width >= 
              1.75 & Petal.Length >= 2.35 ~ "versicolor", Sepal.Length < 
              5.05 & Petal.Length < 5.05 & Petal.Width >= 1.35 & Petal.Width < 
              1.75 & Petal.Length >= 2.35 ~ "virginica", Petal.Width < 
              1.55 & Petal.Length >= 5.05 & Petal.Width >= 1.35 & Petal.Width < 
              1.75 & Petal.Length >= 2.35 ~ "virginica", Petal.Width >= 
              1.55 & Petal.Length >= 5.05 & Petal.Width >= 1.35 & Petal.Width < 
              1.75 & Petal.Length >= 2.35 ~ "versicolor", Sepal.Width >= 
              2.35 & Sepal.Length >= 5.05 & Petal.Length < 5.05 & Petal.Width >= 
              1.35 & Petal.Width < 1.75 & Petal.Length >= 2.35 ~ "versicolor", 
          Sepal.Length < 6.1 & Sepal.Width < 2.35 & Sepal.Length >= 
              5.05 & Petal.Length < 5.05 & Petal.Width >= 1.35 & Petal.Width < 
              1.75 & Petal.Length >= 2.35 ~ "virginica", Sepal.Length >= 
              6.1 & Sepal.Width < 2.35 & Sepal.Length >= 5.05 & Petal.Length < 
              5.05 & Petal.Width >= 1.35 & Petal.Width < 1.75 & Petal.Length >= 
              2.35 ~ "versicolor")
      
      [[8]]
      case_when(Petal.Width < 0.8 & Sepal.Length < 5.3 ~ "setosa", 
          Petal.Width < 1.4 & Petal.Width >= 0.8 & Sepal.Length < 5.3 ~ 
              "versicolor", Petal.Width >= 1.4 & Petal.Width >= 0.8 & 
              Sepal.Length < 5.3 ~ "virginica", Petal.Length < 2.6 & 
              Petal.Width < 1.75 & Sepal.Length >= 5.3 ~ "setosa", 
          Petal.Length >= 4.85 & Petal.Width >= 1.75 & Sepal.Length >= 
              5.3 ~ "virginica", Sepal.Length >= 6.95 & Petal.Length >= 
              2.6 & Petal.Width < 1.75 & Sepal.Length >= 5.3 ~ "virginica", 
          Sepal.Length < 5.95 & Petal.Length < 4.85 & Petal.Width >= 
              1.75 & Sepal.Length >= 5.3 ~ "versicolor", Sepal.Length >= 
              5.95 & Petal.Length < 4.85 & Petal.Width >= 1.75 & Sepal.Length >= 
              5.3 ~ "virginica", Petal.Length < 4.95 & Sepal.Length < 
              6.95 & Petal.Length >= 2.6 & Petal.Width < 1.75 & Sepal.Length >= 
              5.3 ~ "versicolor", Sepal.Width < 2.65 & Petal.Length >= 
              4.95 & Sepal.Length < 6.95 & Petal.Length >= 2.6 & Petal.Width < 
              1.75 & Sepal.Length >= 5.3 ~ "virginica", Sepal.Length >= 
              6.5 & Sepal.Width >= 2.65 & Petal.Length >= 4.95 & Sepal.Length < 
              6.95 & Petal.Length >= 2.6 & Petal.Width < 1.75 & Sepal.Length >= 
              5.3 ~ "versicolor", Sepal.Length < 6.15 & Sepal.Length < 
              6.5 & Sepal.Width >= 2.65 & Petal.Length >= 4.95 & Sepal.Length < 
              6.95 & Petal.Length >= 2.6 & Petal.Width < 1.75 & Sepal.Length >= 
              5.3 ~ "versicolor", Sepal.Length >= 6.15 & Sepal.Length < 
              6.5 & Sepal.Width >= 2.65 & Petal.Length >= 4.95 & Sepal.Length < 
              6.95 & Petal.Length >= 2.6 & Petal.Width < 1.75 & Sepal.Length >= 
              5.3 ~ "virginica")
      
      [[9]]
      case_when(Petal.Length < 2.45 ~ "setosa", Petal.Width < 1.65 & 
          Petal.Length < 4.95 & Petal.Length >= 2.45 ~ "versicolor", 
          Sepal.Length >= 6.05 & Petal.Length >= 4.95 & Petal.Length >= 
              2.45 ~ "virginica", Petal.Length >= 4.85 & Petal.Width >= 
              1.65 & Petal.Length < 4.95 & Petal.Length >= 2.45 ~ "virginica", 
          Petal.Width >= 1.7 & Sepal.Length < 6.05 & Petal.Length >= 
              4.95 & Petal.Length >= 2.45 ~ "virginica", Sepal.Width < 
              3.1 & Petal.Length < 4.85 & Petal.Width >= 1.65 & Petal.Length < 
              4.95 & Petal.Length >= 2.45 ~ "virginica", Sepal.Width >= 
              3.1 & Petal.Length < 4.85 & Petal.Width >= 1.65 & Petal.Length < 
              4.95 & Petal.Length >= 2.45 ~ "versicolor", Petal.Width < 
              1.55 & Petal.Width < 1.7 & Sepal.Length < 6.05 & Petal.Length >= 
              4.95 & Petal.Length >= 2.45 ~ "virginica", Petal.Width >= 
              1.55 & Petal.Width < 1.7 & Sepal.Length < 6.05 & Petal.Length >= 
              4.95 & Petal.Length >= 2.45 ~ "versicolor")
      
      [[10]]
      case_when(Petal.Width < 0.75 ~ "setosa", Sepal.Length >= 7.1 & 
          Petal.Width < 1.75 & Petal.Width >= 0.75 ~ "virginica", Petal.Length >= 
          4.85 & Petal.Width >= 1.75 & Petal.Width >= 0.75 ~ "virginica", 
          Petal.Width < 1.35 & Sepal.Length < 7.1 & Petal.Width < 1.75 & 
              Petal.Width >= 0.75 ~ "versicolor", Sepal.Length < 5.95 & 
              Petal.Length < 4.85 & Petal.Width >= 1.75 & Petal.Width >= 
              0.75 ~ "versicolor", Sepal.Length >= 5.95 & Petal.Length < 
              4.85 & Petal.Width >= 1.75 & Petal.Width >= 0.75 ~ "virginica", 
          Petal.Length < 4.95 & Petal.Width >= 1.35 & Sepal.Length < 
              7.1 & Petal.Width < 1.75 & Petal.Width >= 0.75 ~ "versicolor", 
          Sepal.Width < 2.9 & Petal.Length >= 4.95 & Petal.Width >= 
              1.35 & Sepal.Length < 7.1 & Petal.Width < 1.75 & Petal.Width >= 
              0.75 ~ "virginica", Sepal.Width >= 2.9 & Petal.Length >= 
              4.95 & Petal.Width >= 1.35 & Sepal.Length < 7.1 & Petal.Width < 
              1.75 & Petal.Width >= 0.75 ~ "versicolor")
      
      [[11]]
      case_when(Petal.Length < 2.45 & Sepal.Length < 5.45 ~ "setosa", 
          Petal.Length >= 2.45 & Sepal.Length < 5.45 ~ "versicolor", 
          Petal.Width < 0.65 & Petal.Width < 1.75 & Sepal.Length >= 
              5.45 ~ "setosa", Petal.Width >= 0.65 & Petal.Width < 
              1.75 & Sepal.Length >= 5.45 ~ "versicolor", Sepal.Width < 
              3.15 & Petal.Width >= 1.75 & Sepal.Length >= 5.45 ~ "virginica", 
          Sepal.Length < 6.05 & Sepal.Width >= 3.15 & Petal.Width >= 
              1.75 & Sepal.Length >= 5.45 ~ "versicolor", Sepal.Length >= 
              6.05 & Sepal.Width >= 3.15 & Petal.Width >= 1.75 & Sepal.Length >= 
              5.45 ~ "virginica")
      
      [[12]]
      case_when(Petal.Length < 2.6 ~ "setosa", Petal.Length < 4.95 & 
          Petal.Width < 1.65 & Petal.Length >= 2.6 ~ "versicolor", 
          Petal.Length >= 4.95 & Petal.Width < 1.65 & Petal.Length >= 
              2.6 ~ "virginica", Petal.Length >= 4.85 & Petal.Width >= 
              1.65 & Petal.Length >= 2.6 ~ "virginica", Sepal.Width < 
              3 & Petal.Length < 4.85 & Petal.Width >= 1.65 & Petal.Length >= 
              2.6 ~ "virginica", Sepal.Width >= 3 & Petal.Length < 
              4.85 & Petal.Width >= 1.65 & Petal.Length >= 2.6 ~ "versicolor")
      
      [[13]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Length < 4.95 & 
          Petal.Width < 1.55 & Petal.Width >= 0.8 ~ "versicolor", Petal.Length >= 
          4.95 & Petal.Width < 1.55 & Petal.Width >= 0.8 ~ "virginica", 
          Sepal.Length >= 5.95 & Petal.Width >= 1.55 & Petal.Width >= 
              0.8 ~ "virginica", Petal.Length < 4.85 & Sepal.Length < 
              5.95 & Petal.Width >= 1.55 & Petal.Width >= 0.8 ~ "versicolor", 
          Petal.Length >= 4.85 & Sepal.Length < 5.95 & Petal.Width >= 
              1.55 & Petal.Width >= 0.8 ~ "virginica")
      
      [[14]]
      case_when(Petal.Length < 2.45 ~ "setosa", Petal.Width < 1.65 & 
          Petal.Length < 4.85 & Petal.Length >= 2.45 ~ "versicolor", 
          Petal.Width >= 1.75 & Petal.Length >= 4.85 & Petal.Length >= 
              2.45 ~ "virginica", Petal.Width < 1.75 & Petal.Width >= 
              1.65 & Petal.Length < 4.85 & Petal.Length >= 2.45 ~ "virginica", 
          Sepal.Width < 2.75 & Petal.Width < 1.75 & Petal.Length >= 
              4.85 & Petal.Length >= 2.45 ~ "versicolor", Sepal.Length < 
              6.05 & Petal.Width >= 1.75 & Petal.Width >= 1.65 & Petal.Length < 
              4.85 & Petal.Length >= 2.45 ~ "versicolor", Sepal.Length >= 
              6.05 & Petal.Width >= 1.75 & Petal.Width >= 1.65 & Petal.Length < 
              4.85 & Petal.Length >= 2.45 ~ "virginica", Petal.Width < 
              1.65 & Sepal.Width >= 2.75 & Petal.Width < 1.75 & Petal.Length >= 
              4.85 & Petal.Length >= 2.45 ~ "virginica", Petal.Width >= 
              1.65 & Sepal.Width >= 2.75 & Petal.Width < 1.75 & Petal.Length >= 
              4.85 & Petal.Length >= 2.45 ~ "versicolor")
      
      [[15]]
      case_when(Petal.Length < 2.45 & Sepal.Length < 5.45 ~ "setosa", 
          Petal.Length < 4 & Petal.Length >= 2.45 & Sepal.Length < 
              5.45 ~ "versicolor", Petal.Length < 2.7 & Petal.Width < 
              1.75 & Sepal.Length >= 5.45 ~ "setosa", Petal.Length >= 
              4.85 & Petal.Width >= 1.75 & Sepal.Length >= 5.45 ~ "virginica", 
          Sepal.Length < 5.15 & Petal.Length >= 4 & Petal.Length >= 
              2.45 & Sepal.Length < 5.45 ~ "virginica", Sepal.Length >= 
              5.15 & Petal.Length >= 4 & Petal.Length >= 2.45 & Sepal.Length < 
              5.45 ~ "versicolor", Sepal.Width < 2.25 & Petal.Length >= 
              2.7 & Petal.Width < 1.75 & Sepal.Length >= 5.45 ~ "virginica", 
          Sepal.Length < 6.05 & Petal.Length < 4.85 & Petal.Width >= 
              1.75 & Sepal.Length >= 5.45 ~ "versicolor", Sepal.Length >= 
              6.05 & Petal.Length < 4.85 & Petal.Width >= 1.75 & Sepal.Length >= 
              5.45 ~ "virginica", Petal.Width < 1.35 & Sepal.Width >= 
              2.25 & Petal.Length >= 2.7 & Petal.Width < 1.75 & Sepal.Length >= 
              5.45 ~ "versicolor", Petal.Length < 5.05 & Petal.Width >= 
              1.35 & Sepal.Width >= 2.25 & Petal.Length >= 2.7 & Petal.Width < 
              1.75 & Sepal.Length >= 5.45 ~ "versicolor", Petal.Length >= 
              5.05 & Petal.Width >= 1.35 & Sepal.Width >= 2.25 & Petal.Length >= 
              2.7 & Petal.Width < 1.75 & Sepal.Length >= 5.45 ~ "virginica")
      
      [[16]]
      case_when(Petal.Length < 2.7 ~ "setosa", Petal.Width >= 1.75 & 
          Petal.Length >= 2.7 ~ "virginica", Petal.Width < 1.45 & Petal.Width < 
          1.55 & Petal.Width < 1.75 & Petal.Length >= 2.7 ~ "versicolor", 
          Sepal.Length >= 6.95 & Petal.Width >= 1.55 & Petal.Width < 
              1.75 & Petal.Length >= 2.7 ~ "virginica", Sepal.Length < 
              6.25 & Petal.Width >= 1.45 & Petal.Width < 1.55 & Petal.Width < 
              1.75 & Petal.Length >= 2.7 ~ "versicolor", Sepal.Width < 
              2.75 & Sepal.Length < 6.95 & Petal.Width >= 1.55 & Petal.Width < 
              1.75 & Petal.Length >= 2.7 ~ "virginica", Sepal.Width >= 
              2.75 & Sepal.Length < 6.95 & Petal.Width >= 1.55 & Petal.Width < 
              1.75 & Petal.Length >= 2.7 ~ "versicolor", Petal.Length < 
              5 & Sepal.Length >= 6.25 & Petal.Width >= 1.45 & Petal.Width < 
              1.55 & Petal.Width < 1.75 & Petal.Length >= 2.7 ~ "versicolor", 
          Petal.Length >= 5 & Sepal.Length >= 6.25 & Petal.Width >= 
              1.45 & Petal.Width < 1.55 & Petal.Width < 1.75 & Petal.Length >= 
              2.7 ~ "virginica")
      
      [[17]]
      case_when(Petal.Length < 2.6 ~ "setosa", Petal.Length < 4.85 & 
          Petal.Length < 4.95 & Petal.Length >= 2.6 ~ "versicolor", 
          Petal.Width >= 1.75 & Petal.Length >= 4.95 & Petal.Length >= 
              2.6 ~ "virginica", Sepal.Length < 6.2 & Petal.Length >= 
              4.85 & Petal.Length < 4.95 & Petal.Length >= 2.6 ~ "virginica", 
          Sepal.Length >= 6.2 & Petal.Length >= 4.85 & Petal.Length < 
              4.95 & Petal.Length >= 2.6 ~ "versicolor", Petal.Width < 
              1.5 & Petal.Width < 1.75 & Petal.Length >= 4.95 & Petal.Length >= 
              2.6 ~ "virginica", Petal.Width >= 1.5 & Petal.Width < 
              1.75 & Petal.Length >= 4.95 & Petal.Length >= 2.6 ~ "versicolor")
      
      [[18]]
      case_when(Petal.Width < 0.7 ~ "setosa", Petal.Length >= 5.05 & 
          Sepal.Length >= 6.15 & Petal.Width >= 0.7 ~ "virginica", 
          Petal.Length < 4.85 & Petal.Width < 1.65 & Sepal.Length < 
              6.15 & Petal.Width >= 0.7 ~ "versicolor", Petal.Length >= 
              4.85 & Petal.Width < 1.65 & Sepal.Length < 6.15 & Petal.Width >= 
              0.7 ~ "virginica", Petal.Length >= 4.85 & Petal.Width >= 
              1.65 & Sepal.Length < 6.15 & Petal.Width >= 0.7 ~ "virginica", 
          Sepal.Length < 6.25 & Petal.Length < 5.05 & Sepal.Length >= 
              6.15 & Petal.Width >= 0.7 ~ "virginica", Sepal.Width < 
              2.85 & Petal.Length < 4.85 & Petal.Width >= 1.65 & Sepal.Length < 
              6.15 & Petal.Width >= 0.7 ~ "virginica", Sepal.Width >= 
              2.85 & Petal.Length < 4.85 & Petal.Width >= 1.65 & Sepal.Length < 
              6.15 & Petal.Width >= 0.7 ~ "versicolor", Petal.Width < 
              1.8 & Sepal.Length >= 6.25 & Petal.Length < 5.05 & Sepal.Length >= 
              6.15 & Petal.Width >= 0.7 ~ "versicolor", Petal.Width >= 
              1.8 & Sepal.Length >= 6.25 & Petal.Length < 5.05 & Sepal.Length >= 
              6.15 & Petal.Width >= 0.7 ~ "virginica")
      
      [[19]]
      case_when(Petal.Length < 2.45 ~ "setosa", Petal.Width >= 1.75 & 
          Petal.Length >= 2.45 ~ "virginica", Sepal.Length >= 7.05 & 
          Petal.Width < 1.75 & Petal.Length >= 2.45 ~ "virginica", 
          Sepal.Length < 6.25 & Sepal.Length < 7.05 & Petal.Width < 
              1.75 & Petal.Length >= 2.45 ~ "versicolor", Petal.Length < 
              5.05 & Sepal.Length >= 6.25 & Sepal.Length < 7.05 & Petal.Width < 
              1.75 & Petal.Length >= 2.45 ~ "versicolor", Petal.Length >= 
              5.05 & Sepal.Length >= 6.25 & Sepal.Length < 7.05 & Petal.Width < 
              1.75 & Petal.Length >= 2.45 ~ "virginica")
      
      [[20]]
      case_when(Petal.Length < 2.7 ~ "setosa", Petal.Width < 1.55 & 
          Petal.Length >= 2.7 ~ "versicolor", Petal.Width >= 1.75 & 
          Petal.Width >= 1.55 & Petal.Length >= 2.7 ~ "virginica", 
          Sepal.Width < 2.6 & Petal.Width < 1.75 & Petal.Width >= 1.55 & 
              Petal.Length >= 2.7 ~ "virginica", Sepal.Length < 6.95 & 
              Sepal.Width >= 2.6 & Petal.Width < 1.75 & Petal.Width >= 
              1.55 & Petal.Length >= 2.7 ~ "versicolor", Sepal.Length >= 
              6.95 & Sepal.Width >= 2.6 & Petal.Width < 1.75 & Petal.Width >= 
              1.55 & Petal.Length >= 2.7 ~ "virginica")
      
      [[21]]
      case_when(Petal.Width < 0.7 ~ "setosa", Petal.Length < 4.95 & 
          Petal.Width < 1.65 & Petal.Width >= 0.7 ~ "versicolor", Sepal.Length >= 
          6.05 & Petal.Width >= 1.65 & Petal.Width >= 0.7 ~ "virginica", 
          Petal.Width < 1.55 & Petal.Length >= 4.95 & Petal.Width < 
              1.65 & Petal.Width >= 0.7 ~ "virginica", Sepal.Width < 
              3.1 & Sepal.Length < 6.05 & Petal.Width >= 1.65 & Petal.Width >= 
              0.7 ~ "virginica", Sepal.Width >= 3.1 & Sepal.Length < 
              6.05 & Petal.Width >= 1.65 & Petal.Width >= 0.7 ~ "versicolor", 
          Sepal.Width < 2.85 & Petal.Width >= 1.55 & Petal.Length >= 
              4.95 & Petal.Width < 1.65 & Petal.Width >= 0.7 ~ "versicolor", 
          Sepal.Width >= 2.85 & Petal.Width >= 1.55 & Petal.Length >= 
              4.95 & Petal.Width < 1.65 & Petal.Width >= 0.7 ~ "virginica")
      
      [[22]]
      case_when(Petal.Length < 2.45 ~ "setosa", Sepal.Length >= 5 & 
          Petal.Length < 4.75 & Petal.Length >= 2.45 ~ "versicolor", 
          Petal.Width >= 1.7 & Petal.Length >= 4.75 & Petal.Length >= 
              2.45 ~ "virginica", Sepal.Width < 2.45 & Sepal.Length < 
              5 & Petal.Length < 4.75 & Petal.Length >= 2.45 ~ "versicolor", 
          Sepal.Width >= 2.45 & Sepal.Length < 5 & Petal.Length < 4.75 & 
              Petal.Length >= 2.45 ~ "virginica", Petal.Length >= 5.35 & 
              Petal.Width < 1.7 & Petal.Length >= 4.75 & Petal.Length >= 
              2.45 ~ "virginica", Petal.Length >= 5.05 & Petal.Length < 
              5.35 & Petal.Width < 1.7 & Petal.Length >= 4.75 & Petal.Length >= 
              2.45 ~ "versicolor", Petal.Length < 4.95 & Petal.Length < 
              5.05 & Petal.Length < 5.35 & Petal.Width < 1.7 & Petal.Length >= 
              4.75 & Petal.Length >= 2.45 ~ "versicolor", Petal.Length >= 
              4.95 & Petal.Length < 5.05 & Petal.Length < 5.35 & Petal.Width < 
              1.7 & Petal.Length >= 4.75 & Petal.Length >= 2.45 ~ "virginica")
      
      [[23]]
      case_when(Petal.Length < 2.35 ~ "setosa", Petal.Length < 4.95 & 
          Petal.Width < 1.65 & Petal.Length >= 2.35 ~ "versicolor", 
          Petal.Length >= 4.95 & Petal.Width < 1.65 & Petal.Length >= 
              2.35 ~ "virginica", Petal.Length >= 5.05 & Petal.Width >= 
              1.65 & Petal.Length >= 2.35 ~ "virginica", Petal.Length >= 
              4.95 & Petal.Length < 5.05 & Petal.Width >= 1.65 & Petal.Length >= 
              2.35 ~ "versicolor", Sepal.Width < 3.1 & Petal.Length < 
              4.95 & Petal.Length < 5.05 & Petal.Width >= 1.65 & Petal.Length >= 
              2.35 ~ "virginica", Sepal.Width >= 3.1 & Petal.Length < 
              4.95 & Petal.Length < 5.05 & Petal.Width >= 1.65 & Petal.Length >= 
              2.35 ~ "versicolor")
      
      [[24]]
      case_when(Petal.Width < 0.7 ~ "setosa", Petal.Width < 1.6 & Sepal.Length < 
          5.65 & Petal.Width >= 0.7 ~ "versicolor", Petal.Width >= 
          1.6 & Sepal.Length < 5.65 & Petal.Width >= 0.7 ~ "virginica", 
          Petal.Width >= 1.7 & Sepal.Length >= 5.65 & Petal.Width >= 
              0.7 ~ "virginica", Petal.Length < 4.95 & Petal.Width < 
              1.7 & Sepal.Length >= 5.65 & Petal.Width >= 0.7 ~ "versicolor", 
          Petal.Length >= 4.95 & Petal.Width < 1.7 & Sepal.Length >= 
              5.65 & Petal.Width >= 0.7 ~ "virginica")
      
      [[25]]
      case_when(Petal.Length < 2.5 & Petal.Width < 1.7 ~ "setosa", 
          Petal.Length >= 4.85 & Petal.Width >= 1.7 ~ "virginica", 
          Petal.Length < 5 & Petal.Length >= 2.5 & Petal.Width < 1.7 ~ 
              "versicolor", Petal.Length >= 5 & Petal.Length >= 2.5 & 
              Petal.Width < 1.7 ~ "virginica", Sepal.Width < 3 & Petal.Length < 
              4.85 & Petal.Width >= 1.7 ~ "virginica", Sepal.Width >= 
              3 & Petal.Length < 4.85 & Petal.Width >= 1.7 ~ "versicolor")
      
      [[26]]
      case_when(Petal.Length < 2.45 ~ "setosa", Sepal.Length < 4.95 & 
          Petal.Length < 4.75 & Petal.Length >= 2.45 ~ "virginica", 
          Sepal.Length >= 4.95 & Petal.Length < 4.75 & Petal.Length >= 
              2.45 ~ "versicolor", Sepal.Width < 2.35 & Petal.Width < 
              1.75 & Petal.Length >= 4.75 & Petal.Length >= 2.45 ~ 
              "virginica", Sepal.Width >= 2.35 & Petal.Width < 1.75 & 
              Petal.Length >= 4.75 & Petal.Length >= 2.45 ~ "versicolor", 
          Sepal.Length >= 5.95 & Petal.Width >= 1.75 & Petal.Length >= 
              4.75 & Petal.Length >= 2.45 ~ "virginica", Petal.Length < 
              4.85 & Sepal.Length < 5.95 & Petal.Width >= 1.75 & Petal.Length >= 
              4.75 & Petal.Length >= 2.45 ~ "versicolor", Petal.Length >= 
              4.85 & Sepal.Length < 5.95 & Petal.Width >= 1.75 & Petal.Length >= 
              4.75 & Petal.Length >= 2.45 ~ "virginica")
      
      [[27]]
      case_when(Petal.Width < 0.8 & Sepal.Length < 5.45 ~ "setosa", 
          Petal.Width >= 0.8 & Sepal.Length < 5.45 ~ "versicolor", 
          Sepal.Length >= 7.05 & Sepal.Length >= 5.45 ~ "virginica", 
          Sepal.Width >= 3.6 & Sepal.Length < 7.05 & Sepal.Length >= 
              5.45 ~ "setosa", Petal.Length < 4.95 & Petal.Width < 
              1.7 & Sepal.Width < 3.6 & Sepal.Length < 7.05 & Sepal.Length >= 
              5.45 ~ "versicolor", Petal.Length >= 4.95 & Petal.Width < 
              1.7 & Sepal.Width < 3.6 & Sepal.Length < 7.05 & Sepal.Length >= 
              5.45 ~ "virginica", Petal.Length >= 4.85 & Petal.Width >= 
              1.7 & Sepal.Width < 3.6 & Sepal.Length < 7.05 & Sepal.Length >= 
              5.45 ~ "virginica", Sepal.Length < 5.95 & Petal.Length < 
              4.85 & Petal.Width >= 1.7 & Sepal.Width < 3.6 & Sepal.Length < 
              7.05 & Sepal.Length >= 5.45 ~ "versicolor", Sepal.Length >= 
              5.95 & Petal.Length < 4.85 & Petal.Width >= 1.7 & Sepal.Width < 
              3.6 & Sepal.Length < 7.05 & Sepal.Length >= 5.45 ~ "virginica")
      
      [[28]]
      case_when(Petal.Length < 2.7 ~ "setosa", Sepal.Width >= 2.65 & 
          Petal.Width < 1.65 & Petal.Length >= 2.7 ~ "versicolor", 
          Petal.Length < 5.25 & Sepal.Width < 2.65 & Petal.Width < 
              1.65 & Petal.Length >= 2.7 ~ "versicolor", Petal.Length >= 
              5.25 & Sepal.Width < 2.65 & Petal.Width < 1.65 & Petal.Length >= 
              2.7 ~ "virginica", Petal.Length < 4.75 & Petal.Width < 
              1.75 & Petal.Width >= 1.65 & Petal.Length >= 2.7 ~ "virginica", 
          Petal.Length >= 4.75 & Petal.Width < 1.75 & Petal.Width >= 
              1.65 & Petal.Length >= 2.7 ~ "versicolor", Sepal.Width < 
              3.15 & Petal.Width >= 1.75 & Petal.Width >= 1.65 & Petal.Length >= 
              2.7 ~ "virginica", Sepal.Length < 6.1 & Sepal.Width >= 
              3.15 & Petal.Width >= 1.75 & Petal.Width >= 1.65 & Petal.Length >= 
              2.7 ~ "versicolor", Sepal.Length >= 6.1 & Sepal.Width >= 
              3.15 & Petal.Width >= 1.75 & Petal.Width >= 1.65 & Petal.Length >= 
              2.7 ~ "virginica")
      
      [[29]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Length < 4.95 & 
          Petal.Width < 1.75 & Petal.Width >= 0.8 ~ "versicolor", Sepal.Length >= 
          5.95 & Petal.Width >= 1.75 & Petal.Width >= 0.8 ~ "virginica", 
          Sepal.Width < 2.9 & Petal.Length >= 4.95 & Petal.Width < 
              1.75 & Petal.Width >= 0.8 ~ "virginica", Sepal.Width >= 
              2.9 & Petal.Length >= 4.95 & Petal.Width < 1.75 & Petal.Width >= 
              0.8 ~ "versicolor", Sepal.Length < 5.75 & Sepal.Length < 
              5.95 & Petal.Width >= 1.75 & Petal.Width >= 0.8 ~ "virginica", 
          Sepal.Length >= 5.75 & Sepal.Length < 5.95 & Petal.Width >= 
              1.75 & Petal.Width >= 0.8 ~ "versicolor")
      
      [[30]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Length < 4.75 & 
          Sepal.Length < 6.35 & Petal.Width >= 0.8 ~ "versicolor", 
          Sepal.Length >= 7.1 & Sepal.Length >= 6.35 & Petal.Width >= 
              0.8 ~ "virginica", Sepal.Width < 3.1 & Petal.Length >= 
              4.75 & Sepal.Length < 6.35 & Petal.Width >= 0.8 ~ "virginica", 
          Petal.Length < 5.1 & Sepal.Length < 7.1 & Sepal.Length >= 
              6.35 & Petal.Width >= 0.8 ~ "versicolor", Petal.Length >= 
              5.1 & Sepal.Length < 7.1 & Sepal.Length >= 6.35 & Petal.Width >= 
              0.8 ~ "virginica", Petal.Length < 5.1 & Sepal.Width >= 
              3.1 & Petal.Length >= 4.75 & Sepal.Length < 6.35 & Petal.Width >= 
              0.8 ~ "versicolor", Petal.Length >= 5.1 & Sepal.Width >= 
              3.1 & Petal.Length >= 4.75 & Sepal.Length < 6.35 & Petal.Width >= 
              0.8 ~ "virginica")
      
      [[31]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Length >= 4.85 & 
          Sepal.Length < 6.15 & Petal.Width >= 0.8 ~ "virginica", Petal.Width >= 
          1.7 & Sepal.Length >= 6.15 & Petal.Width >= 0.8 ~ "virginica", 
          Petal.Width < 1.6 & Petal.Length < 4.85 & Sepal.Length < 
              6.15 & Petal.Width >= 0.8 ~ "versicolor", Sepal.Length >= 
              6.35 & Petal.Width < 1.7 & Sepal.Length >= 6.15 & Petal.Width >= 
              0.8 ~ "versicolor", Sepal.Width < 2.85 & Petal.Width >= 
              1.6 & Petal.Length < 4.85 & Sepal.Length < 6.15 & Petal.Width >= 
              0.8 ~ "virginica", Sepal.Width >= 2.85 & Petal.Width >= 
              1.6 & Petal.Length < 4.85 & Sepal.Length < 6.15 & Petal.Width >= 
              0.8 ~ "versicolor", Petal.Length < 5 & Sepal.Length < 
              6.35 & Petal.Width < 1.7 & Sepal.Length >= 6.15 & Petal.Width >= 
              0.8 ~ "versicolor", Petal.Length >= 5 & Sepal.Length < 
              6.35 & Petal.Width < 1.7 & Sepal.Length >= 6.15 & Petal.Width >= 
              0.8 ~ "virginica")
      
      [[32]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Length >= 4.9 & 
          Petal.Width >= 1.75 & Petal.Width >= 0.8 ~ "virginica", Petal.Width < 
          1.35 & Sepal.Length < 4.95 & Petal.Width < 1.75 & Petal.Width >= 
          0.8 ~ "versicolor", Petal.Width >= 1.35 & Sepal.Length < 
          4.95 & Petal.Width < 1.75 & Petal.Width >= 0.8 ~ "virginica", 
          Sepal.Length >= 7 & Sepal.Length >= 4.95 & Petal.Width < 
              1.75 & Petal.Width >= 0.8 ~ "virginica", Sepal.Width < 
              3.1 & Petal.Length < 4.9 & Petal.Width >= 1.75 & Petal.Width >= 
              0.8 ~ "virginica", Sepal.Width >= 3.1 & Petal.Length < 
              4.9 & Petal.Width >= 1.75 & Petal.Width >= 0.8 ~ "versicolor", 
          Petal.Length >= 5.35 & Sepal.Length < 7 & Sepal.Length >= 
              4.95 & Petal.Width < 1.75 & Petal.Width >= 0.8 ~ "virginica", 
          Petal.Width < 1.45 & Petal.Length < 5.35 & Sepal.Length < 
              7 & Sepal.Length >= 4.95 & Petal.Width < 1.75 & Petal.Width >= 
              0.8 ~ "versicolor", Sepal.Width < 2.35 & Petal.Width >= 
              1.45 & Petal.Length < 5.35 & Sepal.Length < 7 & Sepal.Length >= 
              4.95 & Petal.Width < 1.75 & Petal.Width >= 0.8 ~ "versicolor", 
          Sepal.Width >= 2.35 & Petal.Width >= 1.45 & Petal.Length < 
              5.35 & Sepal.Length < 7 & Sepal.Length >= 4.95 & Petal.Width < 
              1.75 & Petal.Width >= 0.8 ~ "versicolor")
      
      [[33]]
      case_when(Petal.Width < 0.7 ~ "setosa", Sepal.Width < 2.3 & Petal.Width < 
          1.75 & Petal.Width >= 0.7 ~ "virginica", Sepal.Width < 3.15 & 
          Petal.Width >= 1.75 & Petal.Width >= 0.7 ~ "virginica", Petal.Width < 
          1.45 & Sepal.Width >= 2.3 & Petal.Width < 1.75 & Petal.Width >= 
          0.7 ~ "versicolor", Sepal.Length < 6.05 & Sepal.Width >= 
          3.15 & Petal.Width >= 1.75 & Petal.Width >= 0.7 ~ "versicolor", 
          Sepal.Length >= 6.05 & Sepal.Width >= 3.15 & Petal.Width >= 
              1.75 & Petal.Width >= 0.7 ~ "virginica", Petal.Length < 
              5.05 & Petal.Width >= 1.45 & Sepal.Width >= 2.3 & Petal.Width < 
              1.75 & Petal.Width >= 0.7 ~ "versicolor", Petal.Length >= 
              5.05 & Petal.Width >= 1.45 & Sepal.Width >= 2.3 & Petal.Width < 
              1.75 & Petal.Width >= 0.7 ~ "virginica")
      
      [[34]]
      case_when(Petal.Length < 2.6 ~ "setosa", Petal.Width >= 1.65 & 
          Petal.Length >= 2.6 ~ "virginica", Petal.Width < 1.25 & Sepal.Width < 
          2.3 & Petal.Width < 1.65 & Petal.Length >= 2.6 ~ "versicolor", 
          Petal.Width >= 1.25 & Sepal.Width < 2.3 & Petal.Width < 1.65 & 
              Petal.Length >= 2.6 ~ "virginica", Petal.Length < 5 & 
              Sepal.Width >= 2.3 & Petal.Width < 1.65 & Petal.Length >= 
              2.6 ~ "versicolor", Petal.Width < 1.55 & Petal.Length >= 
              5 & Sepal.Width >= 2.3 & Petal.Width < 1.65 & Petal.Length >= 
              2.6 ~ "virginica", Petal.Width >= 1.55 & Petal.Length >= 
              5 & Sepal.Width >= 2.3 & Petal.Width < 1.65 & Petal.Length >= 
              2.6 ~ "versicolor")
      
      [[35]]
      case_when(Petal.Length < 2.45 ~ "setosa", Petal.Length < 4.95 & 
          Petal.Width < 1.7 & Petal.Length >= 2.45 ~ "versicolor", 
          Petal.Length >= 4.95 & Petal.Width < 1.7 & Petal.Length >= 
              2.45 ~ "virginica", Sepal.Length >= 5.95 & Petal.Width >= 
              1.7 & Petal.Length >= 2.45 ~ "virginica", Sepal.Length < 
              5.85 & Sepal.Length < 5.95 & Petal.Width >= 1.7 & Petal.Length >= 
              2.45 ~ "virginica", Sepal.Length >= 5.85 & Sepal.Length < 
              5.95 & Petal.Width >= 1.7 & Petal.Length >= 2.45 ~ "versicolor")
      
      [[36]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Width >= 1.75 & 
          Petal.Width >= 0.8 ~ "virginica", Petal.Length >= 5.05 & 
          Petal.Width < 1.75 & Petal.Width >= 0.8 ~ "virginica", Sepal.Width >= 
          2.25 & Petal.Length < 5.05 & Petal.Width < 1.75 & Petal.Width >= 
          0.8 ~ "versicolor", Petal.Length < 4.75 & Sepal.Width < 2.25 & 
          Petal.Length < 5.05 & Petal.Width < 1.75 & Petal.Width >= 
          0.8 ~ "versicolor", Petal.Length >= 4.75 & Sepal.Width < 
          2.25 & Petal.Length < 5.05 & Petal.Width < 1.75 & Petal.Width >= 
          0.8 ~ "virginica")
      
      [[37]]
      case_when(Petal.Length < 2.45 ~ "setosa", Petal.Length >= 5.35 & 
          Petal.Width < 1.75 & Petal.Length >= 2.45 ~ "virginica", 
          Petal.Length < 4.85 & Petal.Width >= 1.75 & Petal.Length >= 
              2.45 ~ "versicolor", Petal.Length >= 4.85 & Petal.Width >= 
              1.75 & Petal.Length >= 2.45 ~ "virginica", Petal.Width < 
              1.65 & Petal.Length < 5.05 & Petal.Length < 5.35 & Petal.Width < 
              1.75 & Petal.Length >= 2.45 ~ "versicolor", Sepal.Length < 
              6.15 & Petal.Length >= 5.05 & Petal.Length < 5.35 & Petal.Width < 
              1.75 & Petal.Length >= 2.45 ~ "versicolor", Sepal.Length >= 
              6.15 & Petal.Length >= 5.05 & Petal.Length < 5.35 & Petal.Width < 
              1.75 & Petal.Length >= 2.45 ~ "virginica", Petal.Length < 
              4.75 & Petal.Width >= 1.65 & Petal.Length < 5.05 & Petal.Length < 
              5.35 & Petal.Width < 1.75 & Petal.Length >= 2.45 ~ "virginica", 
          Petal.Length >= 4.75 & Petal.Width >= 1.65 & Petal.Length < 
              5.05 & Petal.Length < 5.35 & Petal.Width < 1.75 & Petal.Length >= 
              2.45 ~ "versicolor")
      
      [[38]]
      case_when(Petal.Width >= 1.75 ~ "virginica", Petal.Length < 2.45 & 
          Petal.Width < 1.75 ~ "setosa", Sepal.Length >= 7.05 & Petal.Length >= 
          2.45 & Petal.Width < 1.75 ~ "virginica", Sepal.Length < 6.25 & 
          Sepal.Length < 7.05 & Petal.Length >= 2.45 & Petal.Width < 
          1.75 ~ "versicolor", Sepal.Length >= 6.35 & Sepal.Length >= 
          6.25 & Sepal.Length < 7.05 & Petal.Length >= 2.45 & Petal.Width < 
          1.75 ~ "versicolor", Petal.Width < 1.4 & Sepal.Length < 6.35 & 
          Sepal.Length >= 6.25 & Sepal.Length < 7.05 & Petal.Length >= 
          2.45 & Petal.Width < 1.75 ~ "versicolor", Petal.Width >= 
          1.55 & Petal.Width >= 1.4 & Sepal.Length < 6.35 & Sepal.Length >= 
          6.25 & Sepal.Length < 7.05 & Petal.Length >= 2.45 & Petal.Width < 
          1.75 ~ "versicolor", Petal.Length < 5 & Petal.Width < 1.55 & 
          Petal.Width >= 1.4 & Sepal.Length < 6.35 & Sepal.Length >= 
          6.25 & Sepal.Length < 7.05 & Petal.Length >= 2.45 & Petal.Width < 
          1.75 ~ "versicolor", Petal.Length >= 5 & Petal.Width < 1.55 & 
          Petal.Width >= 1.4 & Sepal.Length < 6.35 & Sepal.Length >= 
          6.25 & Sepal.Length < 7.05 & Petal.Length >= 2.45 & Petal.Width < 
          1.75 ~ "virginica")
      
      [[39]]
      case_when(Petal.Length < 2.45 ~ "setosa", Petal.Width < 1.35 & 
          Petal.Width < 1.75 & Petal.Length >= 2.45 ~ "versicolor", 
          Sepal.Length >= 5.95 & Petal.Width >= 1.75 & Petal.Length >= 
              2.45 ~ "virginica", Sepal.Width < 3 & Sepal.Length < 
              5.95 & Petal.Width >= 1.75 & Petal.Length >= 2.45 ~ "virginica", 
          Sepal.Width >= 3 & Sepal.Length < 5.95 & Petal.Width >= 1.75 & 
              Petal.Length >= 2.45 ~ "versicolor", Petal.Length < 4.95 & 
              Petal.Length < 5.05 & Petal.Width >= 1.35 & Petal.Width < 
              1.75 & Petal.Length >= 2.45 ~ "versicolor", Petal.Width < 
              1.55 & Petal.Length >= 5.05 & Petal.Width >= 1.35 & Petal.Width < 
              1.75 & Petal.Length >= 2.45 ~ "virginica", Petal.Width >= 
              1.55 & Petal.Length >= 5.05 & Petal.Width >= 1.35 & Petal.Width < 
              1.75 & Petal.Length >= 2.45 ~ "versicolor", Sepal.Width < 
              2.6 & Petal.Length >= 4.95 & Petal.Length < 5.05 & Petal.Width >= 
              1.35 & Petal.Width < 1.75 & Petal.Length >= 2.45 ~ "virginica", 
          Sepal.Width >= 2.6 & Petal.Length >= 4.95 & Petal.Length < 
              5.05 & Petal.Width >= 1.35 & Petal.Width < 1.75 & Petal.Length >= 
              2.45 ~ "versicolor")
      
      [[40]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Width >= 1.75 & 
          Petal.Width >= 0.8 ~ "virginica", Sepal.Width >= 2.85 & Petal.Width < 
          1.75 & Petal.Width >= 0.8 ~ "versicolor", Petal.Length < 
          4.9 & Sepal.Width < 2.85 & Petal.Width < 1.75 & Petal.Width >= 
          0.8 ~ "versicolor", Petal.Width < 1.55 & Petal.Length >= 
          4.9 & Sepal.Width < 2.85 & Petal.Width < 1.75 & Petal.Width >= 
          0.8 ~ "virginica", Petal.Width >= 1.55 & Petal.Length >= 
          4.9 & Sepal.Width < 2.85 & Petal.Width < 1.75 & Petal.Width >= 
          0.8 ~ "versicolor")
      
      [[41]]
      case_when(Petal.Length < 2.45 ~ "setosa", Sepal.Length >= 6.1 & 
          Petal.Length >= 5.05 & Petal.Length >= 2.45 ~ "virginica", 
          Petal.Width < 1.35 & Sepal.Length < 4.95 & Petal.Length < 
              5.05 & Petal.Length >= 2.45 ~ "versicolor", Petal.Width >= 
              1.35 & Sepal.Length < 4.95 & Petal.Length < 5.05 & Petal.Length >= 
              2.45 ~ "virginica", Petal.Width >= 1.75 & Sepal.Length >= 
              4.95 & Petal.Length < 5.05 & Petal.Length >= 2.45 ~ "virginica", 
          Sepal.Length < 5.95 & Sepal.Length < 6.1 & Petal.Length >= 
              5.05 & Petal.Length >= 2.45 ~ "virginica", Sepal.Length >= 
              5.95 & Sepal.Length < 6.1 & Petal.Length >= 5.05 & Petal.Length >= 
              2.45 ~ "versicolor", Petal.Length < 4.95 & Petal.Width < 
              1.75 & Sepal.Length >= 4.95 & Petal.Length < 5.05 & Petal.Length >= 
              2.45 ~ "versicolor", Sepal.Width < 2.6 & Petal.Length >= 
              4.95 & Petal.Width < 1.75 & Sepal.Length >= 4.95 & Petal.Length < 
              5.05 & Petal.Length >= 2.45 ~ "virginica", Sepal.Width >= 
              2.6 & Petal.Length >= 4.95 & Petal.Width < 1.75 & Sepal.Length >= 
              4.95 & Petal.Length < 5.05 & Petal.Length >= 2.45 ~ "versicolor")
      
      [[42]]
      case_when(Petal.Length < 2.6 & Sepal.Length < 5.45 ~ "setosa", 
          Petal.Length >= 2.6 & Sepal.Length < 5.45 ~ "versicolor", 
          Petal.Width < 0.65 & Petal.Width < 1.75 & Sepal.Length >= 
              5.45 ~ "setosa", Sepal.Length >= 6 & Petal.Width >= 1.75 & 
              Sepal.Length >= 5.45 ~ "virginica", Petal.Length >= 5.3 & 
              Petal.Width >= 0.65 & Petal.Width < 1.75 & Sepal.Length >= 
              5.45 ~ "virginica", Sepal.Width < 3.1 & Sepal.Length < 
              6 & Petal.Width >= 1.75 & Sepal.Length >= 5.45 ~ "virginica", 
          Sepal.Width >= 3.1 & Sepal.Length < 6 & Petal.Width >= 1.75 & 
              Sepal.Length >= 5.45 ~ "versicolor", Petal.Length < 4.95 & 
              Petal.Length < 5.3 & Petal.Width >= 0.65 & Petal.Width < 
              1.75 & Sepal.Length >= 5.45 ~ "versicolor", Sepal.Length < 
              6.35 & Petal.Length >= 4.95 & Petal.Length < 5.3 & Petal.Width >= 
              0.65 & Petal.Width < 1.75 & Sepal.Length >= 5.45 ~ "virginica", 
          Sepal.Length >= 6.35 & Petal.Length >= 4.95 & Petal.Length < 
              5.3 & Petal.Width >= 0.65 & Petal.Width < 1.75 & Sepal.Length >= 
              5.45 ~ "versicolor")
      
      [[43]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Length < 4.75 & 
          Petal.Length < 4.85 & Petal.Width >= 0.8 ~ "versicolor", 
          Petal.Width >= 1.75 & Petal.Length >= 4.85 & Petal.Width >= 
              0.8 ~ "virginica", Sepal.Width < 3.1 & Petal.Length >= 
              4.75 & Petal.Length < 4.85 & Petal.Width >= 0.8 ~ "virginica", 
          Sepal.Width >= 3.1 & Petal.Length >= 4.75 & Petal.Length < 
              4.85 & Petal.Width >= 0.8 ~ "versicolor", Petal.Width >= 
              1.65 & Petal.Width < 1.75 & Petal.Length >= 4.85 & Petal.Width >= 
              0.8 ~ "versicolor", Petal.Width < 1.55 & Petal.Width < 
              1.65 & Petal.Width < 1.75 & Petal.Length >= 4.85 & Petal.Width >= 
              0.8 ~ "virginica", Petal.Width >= 1.55 & Petal.Width < 
              1.65 & Petal.Width < 1.75 & Petal.Length >= 4.85 & Petal.Width >= 
              0.8 ~ "virginica")
      
      [[44]]
      case_when(Petal.Length < 2.5 ~ "setosa", Petal.Length >= 4.95 & 
          Petal.Length >= 2.5 ~ "virginica", Sepal.Length >= 5.15 & 
          Petal.Length < 4.75 & Petal.Length < 4.95 & Petal.Length >= 
          2.5 ~ "versicolor", Sepal.Length >= 6.6 & Petal.Length >= 
          4.75 & Petal.Length < 4.95 & Petal.Length >= 2.5 ~ "versicolor", 
          Sepal.Width < 2.45 & Sepal.Length < 5.15 & Petal.Length < 
              4.75 & Petal.Length < 4.95 & Petal.Length >= 2.5 ~ "versicolor", 
          Sepal.Width >= 2.45 & Sepal.Length < 5.15 & Petal.Length < 
              4.75 & Petal.Length < 4.95 & Petal.Length >= 2.5 ~ "virginica", 
          Sepal.Width >= 3.1 & Sepal.Length < 6.6 & Petal.Length >= 
              4.75 & Petal.Length < 4.95 & Petal.Length >= 2.5 ~ "versicolor", 
          Petal.Width < 1.65 & Sepal.Width < 3.1 & Sepal.Length < 6.6 & 
              Petal.Length >= 4.75 & Petal.Length < 4.95 & Petal.Length >= 
              2.5 ~ "versicolor", Petal.Width >= 1.65 & Sepal.Width < 
              3.1 & Sepal.Length < 6.6 & Petal.Length >= 4.75 & Petal.Length < 
              4.95 & Petal.Length >= 2.5 ~ "virginica")
      
      [[45]]
      case_when(Petal.Length < 2.6 & Sepal.Length < 5.55 ~ "setosa", 
          Petal.Width < 1.6 & Petal.Length >= 2.6 & Sepal.Length < 
              5.55 ~ "versicolor", Petal.Width >= 1.6 & Petal.Length >= 
              2.6 & Sepal.Length < 5.55 ~ "virginica", Petal.Length < 
              4.85 & Petal.Width >= 1.75 & Sepal.Length >= 5.55 ~ "versicolor", 
          Petal.Length >= 4.85 & Petal.Width >= 1.75 & Sepal.Length >= 
              5.55 ~ "virginica", Petal.Width < 0.65 & Petal.Length < 
              5.05 & Petal.Width < 1.75 & Sepal.Length >= 5.55 ~ "setosa", 
          Petal.Width >= 0.65 & Petal.Length < 5.05 & Petal.Width < 
              1.75 & Sepal.Length >= 5.55 ~ "versicolor", Sepal.Length < 
              6.05 & Petal.Length >= 5.05 & Petal.Width < 1.75 & Sepal.Length >= 
              5.55 ~ "versicolor", Sepal.Length >= 6.05 & Petal.Length >= 
              5.05 & Petal.Width < 1.75 & Sepal.Length >= 5.55 ~ "virginica")
      
      [[46]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Length >= 5 & Petal.Width >= 
          0.8 ~ "virginica", Petal.Length < 4.75 & Petal.Length < 5 & 
          Petal.Width >= 0.8 ~ "versicolor", Sepal.Length >= 6.2 & 
          Petal.Length >= 4.75 & Petal.Length < 5 & Petal.Width >= 
          0.8 ~ "versicolor", Sepal.Width < 3.1 & Sepal.Length < 6.2 & 
          Petal.Length >= 4.75 & Petal.Length < 5 & Petal.Width >= 
          0.8 ~ "virginica", Sepal.Width >= 3.1 & Sepal.Length < 6.2 & 
          Petal.Length >= 4.75 & Petal.Length < 5 & Petal.Width >= 
          0.8 ~ "versicolor")
      
      [[47]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Length < 4.75 & 
          Petal.Width >= 0.8 ~ "versicolor", Petal.Length < 5 & Petal.Width < 
          1.55 & Petal.Length >= 4.75 & Petal.Width >= 0.8 ~ "versicolor", 
          Petal.Length >= 5 & Petal.Width < 1.55 & Petal.Length >= 
              4.75 & Petal.Width >= 0.8 ~ "virginica", Sepal.Length >= 
              6.05 & Petal.Width >= 1.55 & Petal.Length >= 4.75 & Petal.Width >= 
              0.8 ~ "virginica", Petal.Width < 1.7 & Sepal.Length < 
              6.05 & Petal.Width >= 1.55 & Petal.Length >= 4.75 & Petal.Width >= 
              0.8 ~ "versicolor", Sepal.Width < 3.1 & Petal.Width >= 
              1.7 & Sepal.Length < 6.05 & Petal.Width >= 1.55 & Petal.Length >= 
              4.75 & Petal.Width >= 0.8 ~ "virginica", Sepal.Width >= 
              3.1 & Petal.Width >= 1.7 & Sepal.Length < 6.05 & Petal.Width >= 
              1.55 & Petal.Length >= 4.75 & Petal.Width >= 0.8 ~ "versicolor")
      
      [[48]]
      case_when(Petal.Width < 0.7 ~ "setosa", Petal.Width < 1.65 & 
          Petal.Length < 4.95 & Petal.Width >= 0.7 ~ "versicolor", 
          Petal.Width >= 1.75 & Petal.Length >= 4.95 & Petal.Width >= 
              0.7 ~ "virginica", Sepal.Width < 3.1 & Petal.Width >= 
              1.65 & Petal.Length < 4.95 & Petal.Width >= 0.7 ~ "virginica", 
          Sepal.Width >= 3.1 & Petal.Width >= 1.65 & Petal.Length < 
              4.95 & Petal.Width >= 0.7 ~ "versicolor", Petal.Width < 
              1.65 & Petal.Width < 1.75 & Petal.Length >= 4.95 & Petal.Width >= 
              0.7 ~ "virginica", Petal.Width >= 1.65 & Petal.Width < 
              1.75 & Petal.Length >= 4.95 & Petal.Width >= 0.7 ~ "versicolor")
      
      [[49]]
      case_when(Petal.Width < 0.7 ~ "setosa", Sepal.Length < 5 & Petal.Length < 
          4.85 & Petal.Width >= 0.7 ~ "virginica", Petal.Width >= 1.75 & 
          Petal.Length >= 4.85 & Petal.Width >= 0.7 ~ "virginica", 
          Sepal.Width < 2.95 & Sepal.Length >= 5 & Petal.Length < 4.85 & 
              Petal.Width >= 0.7 ~ "versicolor", Sepal.Width >= 2.65 & 
              Petal.Width < 1.75 & Petal.Length >= 4.85 & Petal.Width >= 
              0.7 ~ "versicolor", Petal.Width < 1.7 & Sepal.Width >= 
              2.95 & Sepal.Length >= 5 & Petal.Length < 4.85 & Petal.Width >= 
              0.7 ~ "versicolor", Petal.Length < 4.95 & Sepal.Width < 
              2.65 & Petal.Width < 1.75 & Petal.Length >= 4.85 & Petal.Width >= 
              0.7 ~ "versicolor", Petal.Length >= 4.95 & Sepal.Width < 
              2.65 & Petal.Width < 1.75 & Petal.Length >= 4.85 & Petal.Width >= 
              0.7 ~ "virginica", Sepal.Width < 3.1 & Petal.Width >= 
              1.7 & Sepal.Width >= 2.95 & Sepal.Length >= 5 & Petal.Length < 
              4.85 & Petal.Width >= 0.7 ~ "virginica", Sepal.Width >= 
              3.1 & Petal.Width >= 1.7 & Sepal.Width >= 2.95 & Sepal.Length >= 
              5 & Petal.Length < 4.85 & Petal.Width >= 0.7 ~ "versicolor")
      
      [[50]]
      case_when(Petal.Width < 0.75 ~ "setosa", Petal.Length >= 5.05 & 
          Petal.Length >= 4.95 & Petal.Width >= 0.75 ~ "virginica", 
          Petal.Width < 1.65 & Petal.Length < 4.75 & Petal.Length < 
              4.95 & Petal.Width >= 0.75 ~ "versicolor", Petal.Width >= 
              1.65 & Petal.Length < 4.75 & Petal.Length < 4.95 & Petal.Width >= 
              0.75 ~ "virginica", Petal.Width < 1.65 & Petal.Length >= 
              4.75 & Petal.Length < 4.95 & Petal.Width >= 0.75 ~ "versicolor", 
          Sepal.Length < 6.5 & Petal.Length < 5.05 & Petal.Length >= 
              4.95 & Petal.Width >= 0.75 ~ "virginica", Sepal.Length >= 
              6.5 & Petal.Length < 5.05 & Petal.Length >= 4.95 & Petal.Width >= 
              0.75 ~ "versicolor", Sepal.Length < 6.05 & Petal.Width >= 
              1.65 & Petal.Length >= 4.75 & Petal.Length < 4.95 & Petal.Width >= 
              0.75 ~ "versicolor", Sepal.Length >= 6.05 & Petal.Width >= 
              1.65 & Petal.Length >= 4.75 & Petal.Length < 4.95 & Petal.Width >= 
              0.75 ~ "virginica")
      
      [[51]]
      case_when(Petal.Length < 2.45 ~ "setosa", Sepal.Width >= 2.55 & 
          Petal.Length < 4.75 & Petal.Length >= 2.45 ~ "versicolor", 
          Petal.Length >= 4.95 & Petal.Length >= 4.75 & Petal.Length >= 
              2.45 ~ "virginica", Petal.Width < 1.5 & Sepal.Width < 
              2.55 & Petal.Length < 4.75 & Petal.Length >= 2.45 ~ "versicolor", 
          Petal.Width >= 1.5 & Sepal.Width < 2.55 & Petal.Length < 
              4.75 & Petal.Length >= 2.45 ~ "virginica", Sepal.Length < 
              6.5 & Petal.Length < 4.95 & Petal.Length >= 4.75 & Petal.Length >= 
              2.45 ~ "virginica", Sepal.Length >= 6.5 & Petal.Length < 
              4.95 & Petal.Length >= 4.75 & Petal.Length >= 2.45 ~ 
              "versicolor")
      
      [[52]]
      case_when(Petal.Length < 2.6 & Sepal.Length < 5.45 ~ "setosa", 
          Petal.Length >= 4.85 & Sepal.Length >= 5.45 ~ "virginica", 
          Petal.Length < 4 & Petal.Length >= 2.6 & Sepal.Length < 5.45 ~ 
              "versicolor", Petal.Length >= 4 & Petal.Length >= 2.6 & 
              Sepal.Length < 5.45 ~ "virginica", Sepal.Width >= 3.45 & 
              Petal.Length < 4.85 & Sepal.Length >= 5.45 ~ "setosa", 
          Petal.Width < 1.7 & Sepal.Width < 3.45 & Petal.Length < 4.85 & 
              Sepal.Length >= 5.45 ~ "versicolor", Petal.Width >= 1.7 & 
              Sepal.Width < 3.45 & Petal.Length < 4.85 & Sepal.Length >= 
              5.45 ~ "versicolor")
      
      [[53]]
      case_when(Petal.Length < 2.7 ~ "setosa", Petal.Length < 4.95 & 
          Petal.Width < 1.75 & Petal.Length >= 2.7 ~ "versicolor", 
          Sepal.Width < 3.15 & Petal.Width >= 1.75 & Petal.Length >= 
              2.7 ~ "virginica", Petal.Width < 1.55 & Petal.Length >= 
              4.95 & Petal.Width < 1.75 & Petal.Length >= 2.7 ~ "virginica", 
          Petal.Width >= 1.55 & Petal.Length >= 4.95 & Petal.Width < 
              1.75 & Petal.Length >= 2.7 ~ "versicolor", Petal.Length < 
              5.2 & Sepal.Width >= 3.15 & Petal.Width >= 1.75 & Petal.Length >= 
              2.7 ~ "versicolor", Petal.Length >= 5.2 & Sepal.Width >= 
              3.15 & Petal.Width >= 1.75 & Petal.Length >= 2.7 ~ "virginica")
      
      [[54]]
      case_when(Petal.Length < 2.35 ~ "setosa", Petal.Length >= 4.95 & 
          Petal.Length >= 2.35 ~ "virginica", Petal.Width < 1.65 & 
          Petal.Length < 4.95 & Petal.Length >= 2.35 ~ "versicolor", 
          Sepal.Width < 3.1 & Petal.Width >= 1.65 & Petal.Length < 
              4.95 & Petal.Length >= 2.35 ~ "virginica", Sepal.Width >= 
              3.1 & Petal.Width >= 1.65 & Petal.Length < 4.95 & Petal.Length >= 
              2.35 ~ "versicolor")
      
      [[55]]
      case_when(Petal.Width < 0.8 & Sepal.Length < 5.45 ~ "setosa", 
          Petal.Width >= 0.8 & Sepal.Length < 5.45 ~ "versicolor", 
          Petal.Width >= 1.75 & Sepal.Length >= 5.45 ~ "virginica", 
          Petal.Width < 0.7 & Petal.Width < 1.75 & Sepal.Length >= 
              5.45 ~ "setosa", Petal.Length >= 5.05 & Petal.Width >= 
              0.7 & Petal.Width < 1.75 & Sepal.Length >= 5.45 ~ "virginica", 
          Sepal.Width < 2.25 & Petal.Length < 5.05 & Petal.Width >= 
              0.7 & Petal.Width < 1.75 & Sepal.Length >= 5.45 ~ "versicolor", 
          Sepal.Width >= 2.25 & Petal.Length < 5.05 & Petal.Width >= 
              0.7 & Petal.Width < 1.75 & Sepal.Length >= 5.45 ~ "versicolor")
      
      [[56]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Length >= 5.05 & 
          Sepal.Length >= 6.25 & Petal.Width >= 0.8 ~ "virginica", 
          Sepal.Width >= 2.25 & Petal.Width < 1.6 & Sepal.Length < 
              6.25 & Petal.Width >= 0.8 ~ "versicolor", Petal.Length >= 
              4.85 & Petal.Width >= 1.6 & Sepal.Length < 6.25 & Petal.Width >= 
              0.8 ~ "virginica", Sepal.Width >= 2.75 & Petal.Length < 
              5.05 & Sepal.Length >= 6.25 & Petal.Width >= 0.8 ~ "versicolor", 
          Petal.Length < 4.75 & Sepal.Width < 2.25 & Petal.Width < 
              1.6 & Sepal.Length < 6.25 & Petal.Width >= 0.8 ~ "versicolor", 
          Petal.Length >= 4.75 & Sepal.Width < 2.25 & Petal.Width < 
              1.6 & Sepal.Length < 6.25 & Petal.Width >= 0.8 ~ "virginica", 
          Petal.Length < 4.65 & Petal.Length < 4.85 & Petal.Width >= 
              1.6 & Sepal.Length < 6.25 & Petal.Width >= 0.8 ~ "virginica", 
          Petal.Length >= 4.95 & Sepal.Width < 2.75 & Petal.Length < 
              5.05 & Sepal.Length >= 6.25 & Petal.Width >= 0.8 ~ "virginica", 
          Sepal.Length < 5.95 & Petal.Length >= 4.65 & Petal.Length < 
              4.85 & Petal.Width >= 1.6 & Sepal.Length < 6.25 & Petal.Width >= 
              0.8 ~ "versicolor", Sepal.Length >= 5.95 & Petal.Length >= 
              4.65 & Petal.Length < 4.85 & Petal.Width >= 1.6 & Sepal.Length < 
              6.25 & Petal.Width >= 0.8 ~ "virginica", Sepal.Width < 
              2.6 & Petal.Length < 4.95 & Sepal.Width < 2.75 & Petal.Length < 
              5.05 & Sepal.Length >= 6.25 & Petal.Width >= 0.8 ~ "versicolor", 
          Sepal.Width >= 2.6 & Petal.Length < 4.95 & Sepal.Width < 
              2.75 & Petal.Length < 5.05 & Sepal.Length >= 6.25 & Petal.Width >= 
              0.8 ~ "virginica")
      
      [[57]]
      case_when(Petal.Length < 2.6 ~ "setosa", Petal.Width >= 1.75 & 
          Petal.Length >= 4.95 & Petal.Length >= 2.6 ~ "virginica", 
          Petal.Width < 1.6 & Petal.Length < 4.85 & Petal.Length < 
              4.95 & Petal.Length >= 2.6 ~ "versicolor", Petal.Width >= 
              1.6 & Petal.Length < 4.85 & Petal.Length < 4.95 & Petal.Length >= 
              2.6 ~ "virginica", Petal.Width < 1.65 & Petal.Length >= 
              4.85 & Petal.Length < 4.95 & Petal.Length >= 2.6 ~ "versicolor", 
          Petal.Width >= 1.65 & Petal.Length >= 4.85 & Petal.Length < 
              4.95 & Petal.Length >= 2.6 ~ "virginica", Sepal.Width < 
              2.45 & Petal.Width < 1.75 & Petal.Length >= 4.95 & Petal.Length >= 
              2.6 ~ "virginica", Sepal.Length < 6.15 & Sepal.Width >= 
              2.45 & Petal.Width < 1.75 & Petal.Length >= 4.95 & Petal.Length >= 
              2.6 ~ "versicolor", Petal.Width < 1.6 & Sepal.Length >= 
              6.15 & Sepal.Width >= 2.45 & Petal.Width < 1.75 & Petal.Length >= 
              4.95 & Petal.Length >= 2.6 ~ "virginica", Petal.Width >= 
              1.6 & Sepal.Length >= 6.15 & Sepal.Width >= 2.45 & Petal.Width < 
              1.75 & Petal.Length >= 4.95 & Petal.Length >= 2.6 ~ "versicolor")
      
      [[58]]
      case_when(Petal.Length < 2.6 ~ "setosa", Sepal.Length >= 7.1 & 
          Sepal.Length >= 6.05 & Petal.Length >= 2.6 ~ "virginica", 
          Petal.Width < 1.65 & Petal.Length < 4.85 & Sepal.Length < 
              6.05 & Petal.Length >= 2.6 ~ "versicolor", Petal.Length < 
              5.05 & Petal.Length >= 4.85 & Sepal.Length < 6.05 & Petal.Length >= 
              2.6 ~ "virginica", Petal.Length >= 5.05 & Petal.Length >= 
              4.85 & Sepal.Length < 6.05 & Petal.Length >= 2.6 ~ "versicolor", 
          Petal.Width >= 1.65 & Sepal.Length < 7.1 & Sepal.Length >= 
              6.05 & Petal.Length >= 2.6 ~ "virginica", Sepal.Width < 
              3.1 & Petal.Width >= 1.65 & Petal.Length < 4.85 & Sepal.Length < 
              6.05 & Petal.Length >= 2.6 ~ "virginica", Sepal.Width >= 
              3.1 & Petal.Width >= 1.65 & Petal.Length < 4.85 & Sepal.Length < 
              6.05 & Petal.Length >= 2.6 ~ "versicolor", Petal.Length < 
              4.95 & Petal.Width < 1.65 & Sepal.Length < 7.1 & Sepal.Length >= 
              6.05 & Petal.Length >= 2.6 ~ "versicolor", Petal.Length >= 
              4.95 & Petal.Width < 1.65 & Sepal.Length < 7.1 & Sepal.Length >= 
              6.05 & Petal.Length >= 2.6 ~ "virginica")
      
      [[59]]
      case_when(Petal.Width < 0.75 ~ "setosa", Petal.Length < 4.45 & 
          Petal.Length < 4.75 & Petal.Width >= 0.75 ~ "versicolor", 
          Petal.Width >= 1.75 & Petal.Length >= 4.75 & Petal.Width >= 
              0.75 ~ "virginica", Sepal.Length < 5.5 & Petal.Length >= 
              4.45 & Petal.Length < 4.75 & Petal.Width >= 0.75 ~ "virginica", 
          Sepal.Length >= 5.5 & Petal.Length >= 4.45 & Petal.Length < 
              4.75 & Petal.Width >= 0.75 ~ "versicolor", Petal.Length < 
              4.95 & Petal.Width < 1.75 & Petal.Length >= 4.75 & Petal.Width >= 
              0.75 ~ "versicolor", Sepal.Length < 6.5 & Petal.Length >= 
              4.95 & Petal.Width < 1.75 & Petal.Length >= 4.75 & Petal.Width >= 
              0.75 ~ "virginica", Sepal.Length >= 6.5 & Petal.Length >= 
              4.95 & Petal.Width < 1.75 & Petal.Length >= 4.75 & Petal.Width >= 
              0.75 ~ "versicolor")
      
      [[60]]
      case_when(Petal.Width < 0.7 ~ "setosa", Petal.Width >= 1.7 & 
          Petal.Width >= 0.7 ~ "virginica", Petal.Length < 4.9 & Petal.Width < 
          1.7 & Petal.Width >= 0.7 ~ "versicolor", Petal.Width < 1.55 & 
          Petal.Length >= 4.9 & Petal.Width < 1.7 & Petal.Width >= 
          0.7 ~ "virginica", Petal.Width >= 1.55 & Petal.Length >= 
          4.9 & Petal.Width < 1.7 & Petal.Width >= 0.7 ~ "versicolor")
      
      [[61]]
      case_when(Petal.Length < 2.35 ~ "setosa", Sepal.Width < 2.25 & 
          Petal.Width < 1.7 & Petal.Length >= 2.35 ~ "virginica", Petal.Length < 
          4.85 & Petal.Width >= 1.7 & Petal.Length >= 2.35 ~ "versicolor", 
          Petal.Length >= 4.85 & Petal.Width >= 1.7 & Petal.Length >= 
              2.35 ~ "virginica", Sepal.Length < 6.05 & Sepal.Width >= 
              2.25 & Petal.Width < 1.7 & Petal.Length >= 2.35 ~ "versicolor", 
          Petal.Width < 1.35 & Sepal.Width < 2.7 & Sepal.Length >= 
              6.05 & Sepal.Width >= 2.25 & Petal.Width < 1.7 & Petal.Length >= 
              2.35 ~ "versicolor", Petal.Width >= 1.35 & Sepal.Width < 
              2.7 & Sepal.Length >= 6.05 & Sepal.Width >= 2.25 & Petal.Width < 
              1.7 & Petal.Length >= 2.35 ~ "virginica", Petal.Length < 
              4.95 & Sepal.Width >= 2.7 & Sepal.Length >= 6.05 & Sepal.Width >= 
              2.25 & Petal.Width < 1.7 & Petal.Length >= 2.35 ~ "versicolor", 
          Petal.Length >= 4.95 & Sepal.Width >= 2.7 & Sepal.Length >= 
              6.05 & Sepal.Width >= 2.25 & Petal.Width < 1.7 & Petal.Length >= 
              2.35 ~ "virginica")
      
      [[62]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Width >= 1.7 & 
          Petal.Width >= 0.8 ~ "virginica", Petal.Length < 5.2 & Petal.Width < 
          1.45 & Petal.Width < 1.7 & Petal.Width >= 0.8 ~ "versicolor", 
          Petal.Length >= 5.2 & Petal.Width < 1.45 & Petal.Width < 
              1.7 & Petal.Width >= 0.8 ~ "virginica", Petal.Length < 
              4.75 & Petal.Width >= 1.45 & Petal.Width < 1.7 & Petal.Width >= 
              0.8 ~ "versicolor", Sepal.Length >= 6.15 & Petal.Length >= 
              4.75 & Petal.Width >= 1.45 & Petal.Width < 1.7 & Petal.Width >= 
              0.8 ~ "virginica", Sepal.Width < 2.45 & Sepal.Length < 
              6.15 & Petal.Length >= 4.75 & Petal.Width >= 1.45 & Petal.Width < 
              1.7 & Petal.Width >= 0.8 ~ "virginica", Sepal.Width >= 
              2.45 & Sepal.Length < 6.15 & Petal.Length >= 4.75 & Petal.Width >= 
              1.45 & Petal.Width < 1.7 & Petal.Width >= 0.8 ~ "versicolor")
      
      [[63]]
      case_when(Petal.Width < 0.7 ~ "setosa", Petal.Length < 4.75 & 
          Petal.Width >= 0.7 ~ "versicolor", Petal.Width >= 1.75 & 
          Petal.Length >= 4.75 & Petal.Width >= 0.7 ~ "virginica", 
          Sepal.Length < 6.15 & Petal.Length < 5.05 & Petal.Width < 
              1.75 & Petal.Length >= 4.75 & Petal.Width >= 0.7 ~ "virginica", 
          Sepal.Length >= 6.15 & Petal.Length < 5.05 & Petal.Width < 
              1.75 & Petal.Length >= 4.75 & Petal.Width >= 0.7 ~ "versicolor", 
          Sepal.Length < 6.05 & Petal.Length >= 5.05 & Petal.Width < 
              1.75 & Petal.Length >= 4.75 & Petal.Width >= 0.7 ~ "versicolor", 
          Sepal.Length >= 6.05 & Petal.Length >= 5.05 & Petal.Width < 
              1.75 & Petal.Length >= 4.75 & Petal.Width >= 0.7 ~ "virginica")
      
      [[64]]
      case_when(Petal.Length < 2.35 ~ "setosa", Sepal.Length < 6.25 & 
          Petal.Width < 1.75 & Petal.Length >= 2.35 ~ "versicolor", 
          Petal.Length >= 4.85 & Petal.Width >= 1.75 & Petal.Length >= 
              2.35 ~ "virginica", Sepal.Width >= 2.85 & Sepal.Length >= 
              6.25 & Petal.Width < 1.75 & Petal.Length >= 2.35 ~ "versicolor", 
          Sepal.Length < 6.05 & Petal.Length < 4.85 & Petal.Width >= 
              1.75 & Petal.Length >= 2.35 ~ "versicolor", Sepal.Length >= 
              6.05 & Petal.Length < 4.85 & Petal.Width >= 1.75 & Petal.Length >= 
              2.35 ~ "virginica", Petal.Length < 5 & Sepal.Width < 
              2.85 & Sepal.Length >= 6.25 & Petal.Width < 1.75 & Petal.Length >= 
              2.35 ~ "versicolor", Petal.Length >= 5 & Sepal.Width < 
              2.85 & Sepal.Length >= 6.25 & Petal.Width < 1.75 & Petal.Length >= 
              2.35 ~ "virginica")
      
      [[65]]
      case_when(Petal.Width < 0.8 ~ "setosa", Sepal.Length >= 6.95 & 
          Petal.Width < 1.75 & Petal.Width >= 0.8 ~ "virginica", Sepal.Length >= 
          5.95 & Petal.Width >= 1.75 & Petal.Width >= 0.8 ~ "virginica", 
          Sepal.Width >= 2.25 & Sepal.Length < 6.95 & Petal.Width < 
              1.75 & Petal.Width >= 0.8 ~ "versicolor", Petal.Length < 
              4.85 & Sepal.Length < 5.95 & Petal.Width >= 1.75 & Petal.Width >= 
              0.8 ~ "versicolor", Petal.Length >= 4.85 & Sepal.Length < 
              5.95 & Petal.Width >= 1.75 & Petal.Width >= 0.8 ~ "virginica", 
          Petal.Width < 1.25 & Sepal.Width < 2.25 & Sepal.Length < 
              6.95 & Petal.Width < 1.75 & Petal.Width >= 0.8 ~ "versicolor", 
          Petal.Length < 4.75 & Petal.Width >= 1.25 & Sepal.Width < 
              2.25 & Sepal.Length < 6.95 & Petal.Width < 1.75 & Petal.Width >= 
              0.8 ~ "versicolor", Petal.Length >= 4.75 & Petal.Width >= 
              1.25 & Sepal.Width < 2.25 & Sepal.Length < 6.95 & Petal.Width < 
              1.75 & Petal.Width >= 0.8 ~ "virginica")
      
      [[66]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Width >= 1.75 & 
          Petal.Width >= 0.8 ~ "virginica", Petal.Length < 5.05 & Petal.Width < 
          1.75 & Petal.Width >= 0.8 ~ "versicolor", Petal.Width < 1.55 & 
          Petal.Length >= 5.05 & Petal.Width < 1.75 & Petal.Width >= 
          0.8 ~ "virginica", Petal.Width >= 1.55 & Petal.Length >= 
          5.05 & Petal.Width < 1.75 & Petal.Width >= 0.8 ~ "versicolor")
      
      [[67]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Length >= 4.95 & 
          Petal.Width >= 0.8 ~ "virginica", Petal.Width < 1.65 & Petal.Length < 
          4.95 & Petal.Width >= 0.8 ~ "versicolor", Sepal.Width < 3.1 & 
          Petal.Width >= 1.65 & Petal.Length < 4.95 & Petal.Width >= 
          0.8 ~ "virginica", Sepal.Width >= 3.1 & Petal.Width >= 1.65 & 
          Petal.Length < 4.95 & Petal.Width >= 0.8 ~ "versicolor")
      
      [[68]]
      case_when(Petal.Width < 0.75 ~ "setosa", Sepal.Length >= 4.95 & 
          Petal.Length < 4.95 & Petal.Width >= 0.75 ~ "versicolor", 
          Petal.Width >= 1.75 & Petal.Length >= 4.95 & Petal.Width >= 
              0.75 ~ "virginica", Petal.Width < 1.35 & Sepal.Length < 
              4.95 & Petal.Length < 4.95 & Petal.Width >= 0.75 ~ "versicolor", 
          Petal.Width >= 1.35 & Sepal.Length < 4.95 & Petal.Length < 
              4.95 & Petal.Width >= 0.75 ~ "virginica", Petal.Width >= 
              1.65 & Petal.Width < 1.75 & Petal.Length >= 4.95 & Petal.Width >= 
              0.75 ~ "versicolor", Petal.Length >= 5.35 & Petal.Width < 
              1.65 & Petal.Width < 1.75 & Petal.Length >= 4.95 & Petal.Width >= 
              0.75 ~ "virginica", Sepal.Length >= 6.15 & Petal.Length < 
              5.35 & Petal.Width < 1.65 & Petal.Width < 1.75 & Petal.Length >= 
              4.95 & Petal.Width >= 0.75 ~ "virginica", Petal.Width < 
              1.55 & Sepal.Length < 6.15 & Petal.Length < 5.35 & Petal.Width < 
              1.65 & Petal.Width < 1.75 & Petal.Length >= 4.95 & Petal.Width >= 
              0.75 ~ "virginica", Petal.Width >= 1.55 & Sepal.Length < 
              6.15 & Petal.Length < 5.35 & Petal.Width < 1.65 & Petal.Width < 
              1.75 & Petal.Length >= 4.95 & Petal.Width >= 0.75 ~ "versicolor")
      
      [[69]]
      case_when(Petal.Length < 2.45 ~ "setosa", Petal.Length < 4.85 & 
          Petal.Length < 4.95 & Petal.Length >= 2.45 ~ "versicolor", 
          Petal.Width >= 1.75 & Petal.Length >= 4.95 & Petal.Length >= 
              2.45 ~ "virginica", Sepal.Length < 6.2 & Petal.Length >= 
              4.85 & Petal.Length < 4.95 & Petal.Length >= 2.45 ~ "virginica", 
          Sepal.Length >= 6.2 & Petal.Length >= 4.85 & Petal.Length < 
              4.95 & Petal.Length >= 2.45 ~ "versicolor", Sepal.Length >= 
              6.95 & Petal.Width < 1.75 & Petal.Length >= 4.95 & Petal.Length >= 
              2.45 ~ "virginica", Sepal.Width < 2.65 & Sepal.Length < 
              6.95 & Petal.Width < 1.75 & Petal.Length >= 4.95 & Petal.Length >= 
              2.45 ~ "virginica", Sepal.Width >= 2.65 & Sepal.Length < 
              6.95 & Petal.Width < 1.75 & Petal.Length >= 4.95 & Petal.Length >= 
              2.45 ~ "versicolor")
      
      [[70]]
      case_when(Petal.Length < 2.45 ~ "setosa", Petal.Length < 4.75 & 
          Petal.Length >= 2.45 ~ "versicolor", Petal.Width >= 1.75 & 
          Petal.Length >= 4.75 & Petal.Length >= 2.45 ~ "virginica", 
          Petal.Length < 4.95 & Petal.Width < 1.75 & Petal.Length >= 
              4.75 & Petal.Length >= 2.45 ~ "versicolor", Sepal.Length < 
              6.4 & Petal.Length >= 4.95 & Petal.Width < 1.75 & Petal.Length >= 
              4.75 & Petal.Length >= 2.45 ~ "virginica", Sepal.Length < 
              6.95 & Sepal.Length >= 6.4 & Petal.Length >= 4.95 & Petal.Width < 
              1.75 & Petal.Length >= 4.75 & Petal.Length >= 2.45 ~ 
              "versicolor", Sepal.Length >= 6.95 & Sepal.Length >= 
              6.4 & Petal.Length >= 4.95 & Petal.Width < 1.75 & Petal.Length >= 
              4.75 & Petal.Length >= 2.45 ~ "virginica")
      
      [[71]]
      case_when(Petal.Width < 0.75 ~ "setosa", Sepal.Length >= 6.85 & 
          Petal.Width >= 0.75 ~ "virginica", Petal.Length < 4.95 & 
          Petal.Width < 1.65 & Sepal.Length < 6.85 & Petal.Width >= 
          0.75 ~ "versicolor", Petal.Length >= 5.05 & Petal.Width >= 
          1.65 & Sepal.Length < 6.85 & Petal.Width >= 0.75 ~ "virginica", 
          Sepal.Width < 2.45 & Petal.Length >= 4.95 & Petal.Width < 
              1.65 & Sepal.Length < 6.85 & Petal.Width >= 0.75 ~ "virginica", 
          Sepal.Width >= 2.45 & Petal.Length >= 4.95 & Petal.Width < 
              1.65 & Sepal.Length < 6.85 & Petal.Width >= 0.75 ~ "versicolor", 
          Sepal.Length >= 6.45 & Petal.Length < 5.05 & Petal.Width >= 
              1.65 & Sepal.Length < 6.85 & Petal.Width >= 0.75 ~ "versicolor", 
          Sepal.Width < 3.1 & Sepal.Length < 6.45 & Petal.Length < 
              5.05 & Petal.Width >= 1.65 & Sepal.Length < 6.85 & Petal.Width >= 
              0.75 ~ "virginica", Sepal.Width >= 3.1 & Sepal.Length < 
              6.45 & Petal.Length < 5.05 & Petal.Width >= 1.65 & Sepal.Length < 
              6.85 & Petal.Width >= 0.75 ~ "versicolor")
      
      [[72]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Width < 1.55 & 
          Petal.Width >= 0.8 ~ "versicolor", Sepal.Width >= 3.35 & 
          Petal.Width >= 1.55 & Petal.Width >= 0.8 ~ "versicolor", 
          Petal.Length >= 5.05 & Sepal.Width < 3.35 & Petal.Width >= 
              1.55 & Petal.Width >= 0.8 ~ "virginica", Sepal.Width < 
              2.9 & Petal.Length < 5.05 & Sepal.Width < 3.35 & Petal.Width >= 
              1.55 & Petal.Width >= 0.8 ~ "virginica", Sepal.Length < 
              5.95 & Sepal.Width >= 2.9 & Petal.Length < 5.05 & Sepal.Width < 
              3.35 & Petal.Width >= 1.55 & Petal.Width >= 0.8 ~ "versicolor", 
          Sepal.Length < 6.35 & Sepal.Length >= 5.95 & Sepal.Width >= 
              2.9 & Petal.Length < 5.05 & Sepal.Width < 3.35 & Petal.Width >= 
              1.55 & Petal.Width >= 0.8 ~ "virginica", Sepal.Length >= 
              6.35 & Sepal.Length >= 5.95 & Sepal.Width >= 2.9 & Petal.Length < 
              5.05 & Sepal.Width < 3.35 & Petal.Width >= 1.55 & Petal.Width >= 
              0.8 ~ "versicolor")
      
      [[73]]
      case_when(Petal.Width < 0.7 ~ "setosa", Petal.Width < 1.65 & 
          Petal.Length < 4.85 & Petal.Width >= 0.7 ~ "versicolor", 
          Petal.Width >= 1.75 & Petal.Length >= 4.85 & Petal.Width >= 
              0.7 ~ "virginica", Sepal.Length >= 6.05 & Petal.Width >= 
              1.65 & Petal.Length < 4.85 & Petal.Width >= 0.7 ~ "virginica", 
          Sepal.Width < 2.35 & Petal.Width < 1.75 & Petal.Length >= 
              4.85 & Petal.Width >= 0.7 ~ "virginica", Petal.Length < 
              4.65 & Sepal.Length < 6.05 & Petal.Width >= 1.65 & Petal.Length < 
              4.85 & Petal.Width >= 0.7 ~ "virginica", Petal.Length >= 
              4.65 & Sepal.Length < 6.05 & Petal.Width >= 1.65 & Petal.Length < 
              4.85 & Petal.Width >= 0.7 ~ "versicolor", Petal.Width < 
              1.45 & Sepal.Width >= 2.35 & Petal.Width < 1.75 & Petal.Length >= 
              4.85 & Petal.Width >= 0.7 ~ "virginica", Sepal.Width < 
              2.85 & Petal.Width >= 1.45 & Sepal.Width >= 2.35 & Petal.Width < 
              1.75 & Petal.Length >= 4.85 & Petal.Width >= 0.7 ~ "versicolor", 
          Petal.Length < 5.4 & Sepal.Width >= 2.85 & Petal.Width >= 
              1.45 & Sepal.Width >= 2.35 & Petal.Width < 1.75 & Petal.Length >= 
              4.85 & Petal.Width >= 0.7 ~ "versicolor", Petal.Length >= 
              5.4 & Sepal.Width >= 2.85 & Petal.Width >= 1.45 & Sepal.Width >= 
              2.35 & Petal.Width < 1.75 & Petal.Length >= 4.85 & Petal.Width >= 
              0.7 ~ "virginica")
      
      [[74]]
      case_when(Sepal.Length < 6.85 & Petal.Length >= 4.75 ~ "virginica", 
          Petal.Length < 2.45 & Sepal.Length < 5.45 & Petal.Length < 
              4.75 ~ "setosa", Petal.Length >= 2.45 & Sepal.Length < 
              5.45 & Petal.Length < 4.75 ~ "versicolor", Petal.Length < 
              2.6 & Sepal.Length >= 5.45 & Petal.Length < 4.75 ~ "setosa", 
          Petal.Length >= 2.6 & Sepal.Length >= 5.45 & Petal.Length < 
              4.75 ~ "versicolor", Petal.Width < 1.55 & Sepal.Length >= 
              6.85 & Petal.Length >= 4.75 ~ "versicolor", Petal.Width >= 
              1.55 & Sepal.Length >= 6.85 & Petal.Length >= 4.75 ~ 
              "virginica")
      
      [[75]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Width >= 1.65 & 
          Sepal.Length >= 6.15 & Petal.Width >= 0.8 ~ "virginica", 
          Petal.Length < 4.85 & Petal.Width < 1.6 & Sepal.Length < 
              6.15 & Petal.Width >= 0.8 ~ "versicolor", Petal.Length >= 
              4.85 & Petal.Width < 1.6 & Sepal.Length < 6.15 & Petal.Width >= 
              0.8 ~ "virginica", Sepal.Length < 5.85 & Petal.Width >= 
              1.6 & Sepal.Length < 6.15 & Petal.Width >= 0.8 ~ "virginica", 
          Petal.Length < 5 & Petal.Width < 1.65 & Sepal.Length >= 6.15 & 
              Petal.Width >= 0.8 ~ "versicolor", Petal.Length >= 5 & 
              Petal.Width < 1.65 & Sepal.Length >= 6.15 & Petal.Width >= 
              0.8 ~ "virginica", Petal.Length < 4.95 & Sepal.Length >= 
              5.85 & Petal.Width >= 1.6 & Sepal.Length < 6.15 & Petal.Width >= 
              0.8 ~ "versicolor", Petal.Length >= 4.95 & Sepal.Length >= 
              5.85 & Petal.Width >= 1.6 & Sepal.Length < 6.15 & Petal.Width >= 
              0.8 ~ "virginica")
      
      [[76]]
      case_when(Petal.Length < 2.6 ~ "setosa", Petal.Length >= 4.85 & 
          Petal.Width >= 1.75 & Petal.Length >= 2.6 ~ "virginica", 
          Sepal.Length >= 4.95 & Petal.Length < 4.95 & Petal.Width < 
              1.75 & Petal.Length >= 2.6 ~ "versicolor", Petal.Width < 
              1.55 & Petal.Length >= 4.95 & Petal.Width < 1.75 & Petal.Length >= 
              2.6 ~ "virginica", Petal.Width >= 1.55 & Petal.Length >= 
              4.95 & Petal.Width < 1.75 & Petal.Length >= 2.6 ~ "versicolor", 
          Sepal.Width < 3.1 & Petal.Length < 4.85 & Petal.Width >= 
              1.75 & Petal.Length >= 2.6 ~ "virginica", Sepal.Width >= 
              3.1 & Petal.Length < 4.85 & Petal.Width >= 1.75 & Petal.Length >= 
              2.6 ~ "versicolor", Sepal.Width < 2.45 & Sepal.Length < 
              4.95 & Petal.Length < 4.95 & Petal.Width < 1.75 & Petal.Length >= 
              2.6 ~ "versicolor", Sepal.Width >= 2.45 & Sepal.Length < 
              4.95 & Petal.Length < 4.95 & Petal.Width < 1.75 & Petal.Length >= 
              2.6 ~ "virginica")
      
      [[77]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Length < 4.75 & 
          Petal.Width >= 0.8 ~ "versicolor", Petal.Width < 1.65 & Petal.Length < 
          4.95 & Petal.Length >= 4.75 & Petal.Width >= 0.8 ~ "versicolor", 
          Petal.Width >= 1.7 & Petal.Length >= 4.95 & Petal.Length >= 
              4.75 & Petal.Width >= 0.8 ~ "virginica", Sepal.Length >= 
              6.05 & Petal.Width >= 1.65 & Petal.Length < 4.95 & Petal.Length >= 
              4.75 & Petal.Width >= 0.8 ~ "virginica", Sepal.Width < 
              2.65 & Petal.Width < 1.7 & Petal.Length >= 4.95 & Petal.Length >= 
              4.75 & Petal.Width >= 0.8 ~ "virginica", Sepal.Length < 
              5.75 & Sepal.Length < 6.05 & Petal.Width >= 1.65 & Petal.Length < 
              4.95 & Petal.Length >= 4.75 & Petal.Width >= 0.8 ~ "virginica", 
          Sepal.Length >= 5.75 & Sepal.Length < 6.05 & Petal.Width >= 
              1.65 & Petal.Length < 4.95 & Petal.Length >= 4.75 & Petal.Width >= 
              0.8 ~ "versicolor", Sepal.Width < 2.75 & Sepal.Width >= 
              2.65 & Petal.Width < 1.7 & Petal.Length >= 4.95 & Petal.Length >= 
              4.75 & Petal.Width >= 0.8 ~ "versicolor", Sepal.Width >= 
              2.75 & Sepal.Width >= 2.65 & Petal.Width < 1.7 & Petal.Length >= 
              4.95 & Petal.Length >= 4.75 & Petal.Width >= 0.8 ~ "virginica")
      
      [[78]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Width >= 1.75 & 
          Petal.Width >= 0.8 ~ "virginica", Sepal.Length >= 6.95 & 
          Petal.Width < 1.75 & Petal.Width >= 0.8 ~ "virginica", Petal.Length >= 
          5.35 & Sepal.Length < 6.95 & Petal.Width < 1.75 & Petal.Width >= 
          0.8 ~ "virginica", Petal.Length < 4.75 & Sepal.Width < 2.25 & 
          Petal.Length < 5.35 & Sepal.Length < 6.95 & Petal.Width < 
          1.75 & Petal.Width >= 0.8 ~ "versicolor", Petal.Length >= 
          4.75 & Sepal.Width < 2.25 & Petal.Length < 5.35 & Sepal.Length < 
          6.95 & Petal.Width < 1.75 & Petal.Width >= 0.8 ~ "virginica", 
          Petal.Width < 1.65 & Sepal.Width >= 2.25 & Petal.Length < 
              5.35 & Sepal.Length < 6.95 & Petal.Width < 1.75 & Petal.Width >= 
              0.8 ~ "versicolor", Petal.Length < 4.75 & Petal.Width >= 
              1.65 & Sepal.Width >= 2.25 & Petal.Length < 5.35 & Sepal.Length < 
              6.95 & Petal.Width < 1.75 & Petal.Width >= 0.8 ~ "virginica", 
          Petal.Length >= 4.75 & Petal.Width >= 1.65 & Sepal.Width >= 
              2.25 & Petal.Length < 5.35 & Sepal.Length < 6.95 & Petal.Width < 
              1.75 & Petal.Width >= 0.8 ~ "versicolor")
      
      [[79]]
      case_when(Petal.Length < 2.45 ~ "setosa", Petal.Width < 1.65 & 
          Petal.Length < 4.75 & Petal.Length >= 2.45 ~ "versicolor", 
          Petal.Width >= 1.65 & Petal.Length < 4.75 & Petal.Length >= 
              2.45 ~ "virginica", Petal.Width >= 1.7 & Petal.Length >= 
              4.75 & Petal.Length >= 2.45 ~ "virginica", Petal.Length >= 
              5.35 & Petal.Width < 1.7 & Petal.Length >= 4.75 & Petal.Length >= 
              2.45 ~ "virginica", Petal.Length >= 5.05 & Petal.Length < 
              5.35 & Petal.Width < 1.7 & Petal.Length >= 4.75 & Petal.Length >= 
              2.45 ~ "versicolor", Sepal.Length < 6.45 & Petal.Length < 
              5.05 & Petal.Length < 5.35 & Petal.Width < 1.7 & Petal.Length >= 
              4.75 & Petal.Length >= 2.45 ~ "virginica", Sepal.Length >= 
              6.45 & Petal.Length < 5.05 & Petal.Length < 5.35 & Petal.Width < 
              1.7 & Petal.Length >= 4.75 & Petal.Length >= 2.45 ~ "versicolor")
      
      [[80]]
      case_when(Sepal.Width < 2.75 & Sepal.Length < 5.55 ~ "versicolor", 
          Sepal.Width >= 2.75 & Sepal.Length < 5.55 ~ "setosa", Petal.Width >= 
              1.75 & Sepal.Length >= 5.55 ~ "virginica", Petal.Length < 
              2.6 & Petal.Width < 1.75 & Sepal.Length >= 5.55 ~ "setosa", 
          Petal.Length < 4.95 & Petal.Length >= 2.6 & Petal.Width < 
              1.75 & Sepal.Length >= 5.55 ~ "versicolor", Petal.Width < 
              1.65 & Petal.Length >= 4.95 & Petal.Length >= 2.6 & Petal.Width < 
              1.75 & Sepal.Length >= 5.55 ~ "virginica", Petal.Width >= 
              1.65 & Petal.Length >= 4.95 & Petal.Length >= 2.6 & Petal.Width < 
              1.75 & Sepal.Length >= 5.55 ~ "versicolor")
      
      [[81]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Width < 1.65 & 
          Petal.Length < 4.95 & Petal.Width >= 0.8 ~ "versicolor", 
          Petal.Width >= 1.75 & Petal.Length >= 4.95 & Petal.Width >= 
              0.8 ~ "virginica", Sepal.Width < 3.1 & Petal.Width >= 
              1.65 & Petal.Length < 4.95 & Petal.Width >= 0.8 ~ "virginica", 
          Sepal.Width >= 3.1 & Petal.Width >= 1.65 & Petal.Length < 
              4.95 & Petal.Width >= 0.8 ~ "versicolor", Sepal.Width < 
              2.9 & Petal.Width < 1.75 & Petal.Length >= 4.95 & Petal.Width >= 
              0.8 ~ "virginica", Petal.Width < 1.65 & Sepal.Width >= 
              2.9 & Petal.Width < 1.75 & Petal.Length >= 4.95 & Petal.Width >= 
              0.8 ~ "virginica", Petal.Width >= 1.65 & Sepal.Width >= 
              2.9 & Petal.Width < 1.75 & Petal.Length >= 4.95 & Petal.Width >= 
              0.8 ~ "versicolor")
      
      [[82]]
      case_when(Petal.Width < 0.7 ~ "setosa", Sepal.Length < 6.05 & 
          Petal.Width < 1.7 & Petal.Width >= 0.7 ~ "versicolor", Sepal.Width < 
          3.15 & Petal.Width >= 1.7 & Petal.Width >= 0.7 ~ "virginica", 
          Sepal.Width >= 2.85 & Sepal.Length >= 6.05 & Petal.Width < 
              1.7 & Petal.Width >= 0.7 ~ "versicolor", Petal.Width < 
              1.9 & Sepal.Width >= 3.15 & Petal.Width >= 1.7 & Petal.Width >= 
              0.7 ~ "versicolor", Petal.Width >= 1.9 & Sepal.Width >= 
              3.15 & Petal.Width >= 1.7 & Petal.Width >= 0.7 ~ "virginica", 
          Sepal.Width < 2.55 & Sepal.Width < 2.85 & Sepal.Length >= 
              6.05 & Petal.Width < 1.7 & Petal.Width >= 0.7 ~ "versicolor", 
          Petal.Length < 4.95 & Sepal.Width >= 2.55 & Sepal.Width < 
              2.85 & Sepal.Length >= 6.05 & Petal.Width < 1.7 & Petal.Width >= 
              0.7 ~ "versicolor", Petal.Length >= 4.95 & Sepal.Width >= 
              2.55 & Sepal.Width < 2.85 & Sepal.Length >= 6.05 & Petal.Width < 
              1.7 & Petal.Width >= 0.7 ~ "virginica")
      
      [[83]]
      case_when(Petal.Width < 0.75 ~ "setosa", Sepal.Length >= 7.05 & 
          Petal.Width < 1.75 & Petal.Width >= 0.75 ~ "virginica", Petal.Length >= 
          4.85 & Petal.Width >= 1.75 & Petal.Width >= 0.75 ~ "virginica", 
          Petal.Length >= 5.35 & Sepal.Length < 7.05 & Petal.Width < 
              1.75 & Petal.Width >= 0.75 ~ "virginica", Sepal.Length < 
              5.95 & Petal.Length < 4.85 & Petal.Width >= 1.75 & Petal.Width >= 
              0.75 ~ "versicolor", Sepal.Length >= 5.95 & Petal.Length < 
              4.85 & Petal.Width >= 1.75 & Petal.Width >= 0.75 ~ "virginica", 
          Sepal.Length < 4.95 & Petal.Length < 5.35 & Sepal.Length < 
              7.05 & Petal.Width < 1.75 & Petal.Width >= 0.75 ~ "virginica", 
          Petal.Width < 1.45 & Sepal.Length >= 4.95 & Petal.Length < 
              5.35 & Sepal.Length < 7.05 & Petal.Width < 1.75 & Petal.Width >= 
              0.75 ~ "versicolor", Sepal.Length < 6.1 & Sepal.Width < 
              2.35 & Petal.Width >= 1.45 & Sepal.Length >= 4.95 & Petal.Length < 
              5.35 & Sepal.Length < 7.05 & Petal.Width < 1.75 & Petal.Width >= 
              0.75 ~ "virginica", Sepal.Length >= 6.1 & Sepal.Width < 
              2.35 & Petal.Width >= 1.45 & Sepal.Length >= 4.95 & Petal.Length < 
              5.35 & Sepal.Length < 7.05 & Petal.Width < 1.75 & Petal.Width >= 
              0.75 ~ "versicolor", Sepal.Length < 6.15 & Sepal.Width >= 
              2.35 & Petal.Width >= 1.45 & Sepal.Length >= 4.95 & Petal.Length < 
              5.35 & Sepal.Length < 7.05 & Petal.Width < 1.75 & Petal.Width >= 
              0.75 ~ "versicolor", Petal.Length < 5.05 & Sepal.Length >= 
              6.15 & Sepal.Width >= 2.35 & Petal.Width >= 1.45 & Sepal.Length >= 
              4.95 & Petal.Length < 5.35 & Sepal.Length < 7.05 & Petal.Width < 
              1.75 & Petal.Width >= 0.75 ~ "versicolor", Petal.Length >= 
              5.05 & Sepal.Length >= 6.15 & Sepal.Width >= 2.35 & Petal.Width >= 
              1.45 & Sepal.Length >= 4.95 & Petal.Length < 5.35 & Sepal.Length < 
              7.05 & Petal.Width < 1.75 & Petal.Width >= 0.75 ~ "virginica")
      
      [[84]]
      case_when(Petal.Length < 2.45 & Sepal.Length < 5.45 ~ "setosa", 
          Petal.Length < 3.9 & Petal.Length >= 2.45 & Sepal.Length < 
              5.45 ~ "versicolor", Petal.Width < 0.7 & Petal.Length < 
              4.85 & Sepal.Length >= 5.45 ~ "setosa", Petal.Width < 
              1.6 & Petal.Length >= 3.9 & Petal.Length >= 2.45 & Sepal.Length < 
              5.45 ~ "versicolor", Petal.Width >= 1.6 & Petal.Length >= 
              3.9 & Petal.Length >= 2.45 & Sepal.Length < 5.45 ~ "virginica", 
          Petal.Length < 4.75 & Petal.Width >= 0.7 & Petal.Length < 
              4.85 & Sepal.Length >= 5.45 ~ "versicolor", Sepal.Width >= 
              3.05 & Petal.Length < 4.95 & Petal.Length >= 4.85 & Sepal.Length >= 
              5.45 ~ "versicolor", Sepal.Length >= 6.05 & Petal.Length >= 
              4.95 & Petal.Length >= 4.85 & Sepal.Length >= 5.45 ~ 
              "virginica", Sepal.Length < 6.05 & Petal.Length >= 4.75 & 
              Petal.Width >= 0.7 & Petal.Length < 4.85 & Sepal.Length >= 
              5.45 ~ "versicolor", Sepal.Width < 2.6 & Sepal.Width < 
              3.05 & Petal.Length < 4.95 & Petal.Length >= 4.85 & Sepal.Length >= 
              5.45 ~ "versicolor", Sepal.Width >= 2.6 & Sepal.Width < 
              3.05 & Petal.Length < 4.95 & Petal.Length >= 4.85 & Sepal.Length >= 
              5.45 ~ "virginica", Sepal.Width >= 2.75 & Sepal.Length < 
              6.05 & Petal.Length >= 4.95 & Petal.Length >= 4.85 & 
              Sepal.Length >= 5.45 ~ "virginica", Sepal.Length < 6.5 & 
              Sepal.Length >= 6.05 & Petal.Length >= 4.75 & Petal.Width >= 
              0.7 & Petal.Length < 4.85 & Sepal.Length >= 5.45 ~ "virginica", 
          Sepal.Length >= 6.5 & Sepal.Length >= 6.05 & Petal.Length >= 
              4.75 & Petal.Width >= 0.7 & Petal.Length < 4.85 & Sepal.Length >= 
              5.45 ~ "versicolor", Sepal.Width < 2.6 & Sepal.Width < 
              2.75 & Sepal.Length < 6.05 & Petal.Length >= 4.95 & Petal.Length >= 
              4.85 & Sepal.Length >= 5.45 ~ "virginica", Petal.Width < 
              1.75 & Sepal.Width >= 2.6 & Sepal.Width < 2.75 & Sepal.Length < 
              6.05 & Petal.Length >= 4.95 & Petal.Length >= 4.85 & 
              Sepal.Length >= 5.45 ~ "versicolor", Petal.Width >= 1.75 & 
              Sepal.Width >= 2.6 & Sepal.Width < 2.75 & Sepal.Length < 
              6.05 & Petal.Length >= 4.95 & Petal.Length >= 4.85 & 
              Sepal.Length >= 5.45 ~ "virginica")
      
      [[85]]
      case_when(Petal.Length < 2.6 ~ "setosa", Petal.Length >= 4.85 & 
          Petal.Width >= 1.75 & Petal.Length >= 2.6 ~ "virginica", 
          Petal.Length < 5 & Petal.Width < 1.65 & Petal.Width < 1.75 & 
              Petal.Length >= 2.6 ~ "versicolor", Petal.Length < 4.75 & 
              Petal.Width >= 1.65 & Petal.Width < 1.75 & Petal.Length >= 
              2.6 ~ "virginica", Petal.Length >= 4.75 & Petal.Width >= 
              1.65 & Petal.Width < 1.75 & Petal.Length >= 2.6 ~ "versicolor", 
          Sepal.Length < 5.95 & Petal.Length < 4.85 & Petal.Width >= 
              1.75 & Petal.Length >= 2.6 ~ "versicolor", Sepal.Length >= 
              5.95 & Petal.Length < 4.85 & Petal.Width >= 1.75 & Petal.Length >= 
              2.6 ~ "virginica", Sepal.Width >= 2.75 & Petal.Length >= 
              5 & Petal.Width < 1.65 & Petal.Width < 1.75 & Petal.Length >= 
              2.6 ~ "virginica", Petal.Width < 1.5 & Sepal.Width < 
              2.75 & Petal.Length >= 5 & Petal.Width < 1.65 & Petal.Width < 
              1.75 & Petal.Length >= 2.6 ~ "virginica", Petal.Width >= 
              1.5 & Sepal.Width < 2.75 & Petal.Length >= 5 & Petal.Width < 
              1.65 & Petal.Width < 1.75 & Petal.Length >= 2.6 ~ "versicolor")
      
      [[86]]
      case_when(Petal.Width < 0.8 & Petal.Width < 1.65 ~ "setosa", 
          Petal.Length >= 5.05 & Petal.Width >= 1.65 ~ "virginica", 
          Petal.Length >= 5.35 & Petal.Width >= 0.8 & Petal.Width < 
              1.65 ~ "virginica", Sepal.Width < 2.9 & Petal.Length < 
              5.05 & Petal.Width >= 1.65 ~ "virginica", Sepal.Width >= 
              2.9 & Petal.Length < 5.05 & Petal.Width >= 1.65 ~ "versicolor", 
          Petal.Length < 5 & Petal.Length < 5.35 & Petal.Width >= 0.8 & 
              Petal.Width < 1.65 ~ "versicolor", Petal.Width < 1.55 & 
              Petal.Length >= 5 & Petal.Length < 5.35 & Petal.Width >= 
              0.8 & Petal.Width < 1.65 ~ "virginica", Petal.Width >= 
              1.55 & Petal.Length >= 5 & Petal.Length < 5.35 & Petal.Width >= 
              0.8 & Petal.Width < 1.65 ~ "versicolor")
      
      [[87]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Width >= 1.85 & 
          Sepal.Length < 6.15 & Petal.Width >= 0.8 ~ "virginica", Petal.Width >= 
          1.75 & Sepal.Length >= 6.15 & Petal.Width >= 0.8 ~ "virginica", 
          Petal.Width < 1.35 & Petal.Width < 1.85 & Sepal.Length < 
              6.15 & Petal.Width >= 0.8 ~ "versicolor", Sepal.Length >= 
              7.1 & Petal.Width < 1.75 & Sepal.Length >= 6.15 & Petal.Width >= 
              0.8 ~ "virginica", Petal.Length >= 4.9 & Petal.Width >= 
              1.35 & Petal.Width < 1.85 & Sepal.Length < 6.15 & Petal.Width >= 
              0.8 ~ "virginica", Petal.Length < 5.05 & Sepal.Length < 
              7.1 & Petal.Width < 1.75 & Sepal.Length >= 6.15 & Petal.Width >= 
              0.8 ~ "versicolor", Petal.Length >= 5.05 & Sepal.Length < 
              7.1 & Petal.Width < 1.75 & Sepal.Length >= 6.15 & Petal.Width >= 
              0.8 ~ "virginica", Sepal.Length < 5.05 & Petal.Length < 
              4.9 & Petal.Width >= 1.35 & Petal.Width < 1.85 & Sepal.Length < 
              6.15 & Petal.Width >= 0.8 ~ "virginica", Sepal.Length >= 
              5.05 & Petal.Length < 4.9 & Petal.Width >= 1.35 & Petal.Width < 
              1.85 & Sepal.Length < 6.15 & Petal.Width >= 0.8 ~ "versicolor")
      
      [[88]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Width >= 1.7 & 
          Petal.Width >= 0.8 ~ "virginica", Petal.Length < 5 & Petal.Width < 
          1.7 & Petal.Width >= 0.8 ~ "versicolor", Petal.Length >= 
          5.35 & Petal.Length >= 5 & Petal.Width < 1.7 & Petal.Width >= 
          0.8 ~ "virginica", Sepal.Width < 2.75 & Petal.Length < 5.35 & 
          Petal.Length >= 5 & Petal.Width < 1.7 & Petal.Width >= 0.8 ~ 
          "versicolor", Sepal.Width >= 2.75 & Petal.Length < 5.35 & 
          Petal.Length >= 5 & Petal.Width < 1.7 & Petal.Width >= 0.8 ~ 
          "virginica")
      
      [[89]]
      case_when(Petal.Length < 2.45 ~ "setosa", Petal.Width < 1.7 & 
          Petal.Length < 4.95 & Petal.Length >= 2.45 ~ "versicolor", 
          Petal.Width >= 1.7 & Petal.Length < 4.95 & Petal.Length >= 
              2.45 ~ "virginica", Sepal.Length < 5.95 & Sepal.Length < 
              6.05 & Petal.Length >= 4.95 & Petal.Length >= 2.45 ~ 
              "virginica", Sepal.Width >= 3.05 & Sepal.Length >= 6.05 & 
              Petal.Length >= 4.95 & Petal.Length >= 2.45 ~ "virginica", 
          Sepal.Width < 2.45 & Sepal.Length >= 5.95 & Sepal.Length < 
              6.05 & Petal.Length >= 4.95 & Petal.Length >= 2.45 ~ 
              "virginica", Sepal.Width >= 2.45 & Sepal.Length >= 5.95 & 
              Sepal.Length < 6.05 & Petal.Length >= 4.95 & Petal.Length >= 
              2.45 ~ "versicolor", Sepal.Width < 2.95 & Sepal.Width < 
              3.05 & Sepal.Length >= 6.05 & Petal.Length >= 4.95 & 
              Petal.Length >= 2.45 ~ "virginica", Petal.Length < 5.1 & 
              Sepal.Width >= 2.95 & Sepal.Width < 3.05 & Sepal.Length >= 
              6.05 & Petal.Length >= 4.95 & Petal.Length >= 2.45 ~ 
              "versicolor", Petal.Length >= 5.1 & Sepal.Width >= 2.95 & 
              Sepal.Width < 3.05 & Sepal.Length >= 6.05 & Petal.Length >= 
              4.95 & Petal.Length >= 2.45 ~ "virginica")
      
      [[90]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Width < 1.65 & 
          Petal.Length < 4.85 & Petal.Width >= 0.8 ~ "versicolor", 
          Petal.Width >= 1.55 & Petal.Length >= 4.85 & Petal.Width >= 
              0.8 ~ "virginica", Sepal.Width < 3 & Petal.Width >= 1.65 & 
              Petal.Length < 4.85 & Petal.Width >= 0.8 ~ "virginica", 
          Sepal.Width >= 3 & Petal.Width >= 1.65 & Petal.Length < 4.85 & 
              Petal.Width >= 0.8 ~ "versicolor", Petal.Length < 4.95 & 
              Petal.Width < 1.55 & Petal.Length >= 4.85 & Petal.Width >= 
              0.8 ~ "versicolor", Petal.Length >= 4.95 & Petal.Width < 
              1.55 & Petal.Length >= 4.85 & Petal.Width >= 0.8 ~ "virginica")
      
      [[91]]
      case_when(Petal.Width < 0.75 & Sepal.Length < 5.45 ~ "setosa", 
          Sepal.Length >= 5 & Petal.Width >= 0.75 & Sepal.Length < 
              5.45 ~ "versicolor", Sepal.Width >= 3.65 & Petal.Width < 
              1.7 & Sepal.Length >= 5.45 ~ "setosa", Sepal.Length >= 
              5.95 & Petal.Width >= 1.7 & Sepal.Length >= 5.45 ~ "virginica", 
          Petal.Length < 3.9 & Sepal.Length < 5 & Petal.Width >= 0.75 & 
              Sepal.Length < 5.45 ~ "versicolor", Petal.Length >= 3.9 & 
              Sepal.Length < 5 & Petal.Width >= 0.75 & Sepal.Length < 
              5.45 ~ "virginica", Petal.Width < 1.35 & Sepal.Width < 
              3.65 & Petal.Width < 1.7 & Sepal.Length >= 5.45 ~ "versicolor", 
          Sepal.Width < 3.1 & Sepal.Length < 5.95 & Petal.Width >= 
              1.7 & Sepal.Length >= 5.45 ~ "virginica", Sepal.Width >= 
              3.1 & Sepal.Length < 5.95 & Petal.Width >= 1.7 & Sepal.Length >= 
              5.45 ~ "versicolor", Sepal.Length >= 7.1 & Petal.Width >= 
              1.35 & Sepal.Width < 3.65 & Petal.Width < 1.7 & Sepal.Length >= 
              5.45 ~ "virginica", Sepal.Length >= 6.2 & Sepal.Length < 
              7.1 & Petal.Width >= 1.35 & Sepal.Width < 3.65 & Petal.Width < 
              1.7 & Sepal.Length >= 5.45 ~ "versicolor", Sepal.Width < 
              2.65 & Sepal.Length < 6.2 & Sepal.Length < 7.1 & Petal.Width >= 
              1.35 & Sepal.Width < 3.65 & Petal.Width < 1.7 & Sepal.Length >= 
              5.45 ~ "virginica", Sepal.Width >= 2.65 & Sepal.Length < 
              6.2 & Sepal.Length < 7.1 & Petal.Width >= 1.35 & Sepal.Width < 
              3.65 & Petal.Width < 1.7 & Sepal.Length >= 5.45 ~ "versicolor")
      
      [[92]]
      case_when(Sepal.Width >= 2.8 & Sepal.Length < 5.4 ~ "setosa", 
          Sepal.Length < 4.7 & Sepal.Width < 2.8 & Sepal.Length < 5.4 ~ 
              "setosa", Sepal.Length >= 4.7 & Sepal.Width < 2.8 & Sepal.Length < 
              5.4 ~ "versicolor", Sepal.Width < 3.5 & Petal.Length < 
              4.75 & Sepal.Length >= 5.4 ~ "versicolor", Sepal.Width >= 
              3.5 & Petal.Length < 4.75 & Sepal.Length >= 5.4 ~ "setosa", 
          Petal.Width >= 1.75 & Petal.Length >= 4.75 & Sepal.Length >= 
              5.4 ~ "virginica", Sepal.Width >= 2.9 & Petal.Width < 
              1.75 & Petal.Length >= 4.75 & Sepal.Length >= 5.4 ~ "versicolor", 
          Petal.Length < 5 & Sepal.Width < 2.9 & Petal.Width < 1.75 & 
              Petal.Length >= 4.75 & Sepal.Length >= 5.4 ~ "versicolor", 
          Petal.Length >= 5 & Sepal.Width < 2.9 & Petal.Width < 1.75 & 
              Petal.Length >= 4.75 & Sepal.Length >= 5.4 ~ "virginica")
      
      [[93]]
      case_when(Petal.Width < 0.8 ~ "setosa", Sepal.Width >= 2.85 & 
          Petal.Width < 1.75 & Petal.Width >= 0.8 ~ "versicolor", Petal.Length >= 
          4.85 & Petal.Width >= 1.75 & Petal.Width >= 0.8 ~ "virginica", 
          Petal.Length < 5 & Sepal.Width < 2.85 & Petal.Width < 1.75 & 
              Petal.Width >= 0.8 ~ "versicolor", Sepal.Width < 3 & 
              Petal.Length < 4.85 & Petal.Width >= 1.75 & Petal.Width >= 
              0.8 ~ "virginica", Sepal.Width >= 3 & Petal.Length < 
              4.85 & Petal.Width >= 1.75 & Petal.Width >= 0.8 ~ "versicolor", 
          Sepal.Length < 6.05 & Petal.Length >= 5 & Sepal.Width < 2.85 & 
              Petal.Width < 1.75 & Petal.Width >= 0.8 ~ "versicolor", 
          Sepal.Length >= 6.05 & Petal.Length >= 5 & Sepal.Width < 
              2.85 & Petal.Width < 1.75 & Petal.Width >= 0.8 ~ "virginica")
      
      [[94]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Length >= 4.95 & 
          Petal.Width >= 0.8 ~ "virginica", Sepal.Length < 5.95 & Petal.Length < 
          4.95 & Petal.Width >= 0.8 ~ "versicolor", Petal.Length < 
          4.75 & Sepal.Length >= 5.95 & Petal.Length < 4.95 & Petal.Width >= 
          0.8 ~ "versicolor", Sepal.Width >= 2.9 & Petal.Length >= 
          4.75 & Sepal.Length >= 5.95 & Petal.Length < 4.95 & Petal.Width >= 
          0.8 ~ "virginica", Sepal.Width >= 2.75 & Sepal.Width < 2.9 & 
          Petal.Length >= 4.75 & Sepal.Length >= 5.95 & Petal.Length < 
          4.95 & Petal.Width >= 0.8 ~ "versicolor", Petal.Width < 1.65 & 
          Sepal.Width < 2.75 & Sepal.Width < 2.9 & Petal.Length >= 
          4.75 & Sepal.Length >= 5.95 & Petal.Length < 4.95 & Petal.Width >= 
          0.8 ~ "versicolor", Petal.Width >= 1.65 & Sepal.Width < 2.75 & 
          Sepal.Width < 2.9 & Petal.Length >= 4.75 & Sepal.Length >= 
          5.95 & Petal.Length < 4.95 & Petal.Width >= 0.8 ~ "virginica")
      
      [[95]]
      case_when(Petal.Length < 2.6 ~ "setosa", Petal.Width < 1.65 & 
          Petal.Length < 4.85 & Petal.Length >= 2.6 ~ "versicolor", 
          Petal.Width >= 1.65 & Petal.Length < 4.85 & Petal.Length >= 
              2.6 ~ "virginica", Petal.Length >= 5.05 & Petal.Length >= 
              4.85 & Petal.Length >= 2.6 ~ "virginica", Petal.Width < 
              1.75 & Petal.Length < 5.05 & Petal.Length >= 4.85 & Petal.Length >= 
              2.6 ~ "versicolor", Petal.Width >= 1.75 & Petal.Length < 
              5.05 & Petal.Length >= 4.85 & Petal.Length >= 2.6 ~ "virginica")
      
      [[96]]
      case_when(Petal.Width < 0.7 ~ "setosa", Petal.Length >= 5.45 & 
          Petal.Width < 1.75 & Petal.Width >= 0.7 ~ "virginica", Sepal.Length >= 
          6 & Petal.Width >= 1.75 & Petal.Width >= 0.7 ~ "virginica", 
          Petal.Width < 1.85 & Sepal.Length < 6 & Petal.Width >= 1.75 & 
              Petal.Width >= 0.7 ~ "versicolor", Petal.Width >= 1.85 & 
              Sepal.Length < 6 & Petal.Width >= 1.75 & Petal.Width >= 
              0.7 ~ "virginica", Sepal.Width < 2.45 & Sepal.Length < 
              4.95 & Petal.Length < 5.45 & Petal.Width < 1.75 & Petal.Width >= 
              0.7 ~ "versicolor", Sepal.Width >= 2.45 & Sepal.Length < 
              4.95 & Petal.Length < 5.45 & Petal.Width < 1.75 & Petal.Width >= 
              0.7 ~ "virginica", Petal.Length < 4.95 & Sepal.Length >= 
              4.95 & Petal.Length < 5.45 & Petal.Width < 1.75 & Petal.Width >= 
              0.7 ~ "versicolor", Petal.Width < 1.55 & Petal.Length >= 
              4.95 & Sepal.Length >= 4.95 & Petal.Length < 5.45 & Petal.Width < 
              1.75 & Petal.Width >= 0.7 ~ "virginica", Petal.Width >= 
              1.55 & Petal.Length >= 4.95 & Sepal.Length >= 4.95 & 
              Petal.Length < 5.45 & Petal.Width < 1.75 & Petal.Width >= 
              0.7 ~ "versicolor")
      
      [[97]]
      case_when(Petal.Width < 0.75 ~ "setosa", Petal.Length < 4.85 & 
          Petal.Width < 1.7 & Petal.Width >= 0.75 ~ "versicolor", Petal.Width >= 
          1.85 & Petal.Width >= 1.7 & Petal.Width >= 0.75 ~ "virginica", 
          Sepal.Width >= 2.75 & Petal.Length >= 4.85 & Petal.Width < 
              1.7 & Petal.Width >= 0.75 ~ "virginica", Petal.Length >= 
              4.95 & Petal.Width < 1.85 & Petal.Width >= 1.7 & Petal.Width >= 
              0.75 ~ "virginica", Sepal.Width < 2.45 & Sepal.Width < 
              2.75 & Petal.Length >= 4.85 & Petal.Width < 1.7 & Petal.Width >= 
              0.75 ~ "virginica", Sepal.Width >= 2.45 & Sepal.Width < 
              2.75 & Petal.Length >= 4.85 & Petal.Width < 1.7 & Petal.Width >= 
              0.75 ~ "versicolor", Sepal.Width < 3.1 & Petal.Length < 
              4.95 & Petal.Width < 1.85 & Petal.Width >= 1.7 & Petal.Width >= 
              0.75 ~ "virginica", Sepal.Width >= 3.1 & Petal.Length < 
              4.95 & Petal.Width < 1.85 & Petal.Width >= 1.7 & Petal.Width >= 
              0.75 ~ "versicolor")
      
      [[98]]
      case_when(Petal.Width < 0.8 & Sepal.Length < 5.45 ~ "setosa", 
          Petal.Width >= 0.8 & Sepal.Length < 5.45 ~ "versicolor", 
          Petal.Width >= 1.7 & Sepal.Length >= 5.45 ~ "virginica", 
          Sepal.Width >= 3.55 & Petal.Width < 1.7 & Sepal.Length >= 
              5.45 ~ "setosa", Petal.Width < 1.35 & Sepal.Width < 3.55 & 
              Petal.Width < 1.7 & Sepal.Length >= 5.45 ~ "versicolor", 
          Petal.Length < 5 & Petal.Width >= 1.35 & Sepal.Width < 3.55 & 
              Petal.Width < 1.7 & Sepal.Length >= 5.45 ~ "versicolor", 
          Petal.Width < 1.55 & Petal.Length >= 5 & Petal.Width >= 1.35 & 
              Sepal.Width < 3.55 & Petal.Width < 1.7 & Sepal.Length >= 
              5.45 ~ "virginica", Petal.Width >= 1.55 & Petal.Length >= 
              5 & Petal.Width >= 1.35 & Sepal.Width < 3.55 & Petal.Width < 
              1.7 & Sepal.Length >= 5.45 ~ "versicolor")
      
      [[99]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Width >= 1.7 & 
          Sepal.Length < 6.25 & Petal.Width >= 0.8 ~ "virginica", Petal.Length >= 
          5.05 & Sepal.Length >= 6.25 & Petal.Width >= 0.8 ~ "virginica", 
          Petal.Width < 1.45 & Petal.Width < 1.7 & Sepal.Length < 6.25 & 
              Petal.Width >= 0.8 ~ "versicolor", Sepal.Width >= 2.75 & 
              Petal.Length < 5.05 & Sepal.Length >= 6.25 & Petal.Width >= 
              0.8 ~ "versicolor", Petal.Length < 4.75 & Petal.Width >= 
              1.45 & Petal.Width < 1.7 & Sepal.Length < 6.25 & Petal.Width >= 
              0.8 ~ "versicolor", Petal.Length >= 4.75 & Petal.Width >= 
              1.45 & Petal.Width < 1.7 & Sepal.Length < 6.25 & Petal.Width >= 
              0.8 ~ "virginica", Petal.Width < 1.65 & Sepal.Width < 
              2.75 & Petal.Length < 5.05 & Sepal.Length >= 6.25 & Petal.Width >= 
              0.8 ~ "versicolor", Petal.Width >= 1.65 & Sepal.Width < 
              2.75 & Petal.Length < 5.05 & Sepal.Length >= 6.25 & Petal.Width >= 
              0.8 ~ "virginica")
      
      [[100]]
      case_when(Petal.Width < 0.7 ~ "setosa", Petal.Width < 1.7 & Petal.Length < 
          4.85 & Petal.Width >= 0.7 ~ "versicolor", Petal.Width >= 
          1.75 & Petal.Length >= 4.85 & Petal.Width >= 0.7 ~ "virginica", 
          Sepal.Width < 3.1 & Petal.Width >= 1.7 & Petal.Length < 4.85 & 
              Petal.Width >= 0.7 ~ "virginica", Sepal.Width >= 3.1 & 
              Petal.Width >= 1.7 & Petal.Length < 4.85 & Petal.Width >= 
              0.7 ~ "versicolor", Petal.Length < 4.95 & Petal.Width < 
              1.75 & Petal.Length >= 4.85 & Petal.Width >= 0.7 ~ "versicolor", 
          Petal.Width < 1.6 & Petal.Length >= 4.95 & Petal.Width < 
              1.75 & Petal.Length >= 4.85 & Petal.Width >= 0.7 ~ "virginica", 
          Petal.Width >= 1.6 & Petal.Length >= 4.95 & Petal.Width < 
              1.75 & Petal.Length >= 4.85 & Petal.Width >= 0.7 ~ "versicolor")
      

