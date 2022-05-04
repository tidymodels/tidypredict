# Model can be saved and re-loaded

    Code
      tidypredict_fit(pm)
    Output
      [[1]]
      case_when(Petal.Length < 2.6 ~ "setosa", Petal.Width >= 1.7 & 
          Sepal.Length < 6.25 & Petal.Length >= 2.6 ~ "virginica", 
          Petal.Length >= 5.05 & Sepal.Length >= 6.25 & Petal.Length >= 
              2.6 ~ "virginica", Petal.Length < 5.35 & Petal.Width < 
              1.7 & Sepal.Length < 6.25 & Petal.Length >= 2.6 ~ "versicolor", 
          Petal.Length >= 5.35 & Petal.Width < 1.7 & Sepal.Length < 
              6.25 & Petal.Length >= 2.6 ~ "virginica", Petal.Length < 
              4.85 & Petal.Length < 5.05 & Sepal.Length >= 6.25 & Petal.Length >= 
              2.6 ~ "versicolor", Sepal.Length < 6.5 & Petal.Length >= 
              4.85 & Petal.Length < 5.05 & Sepal.Length >= 6.25 & Petal.Length >= 
              2.6 ~ "versicolor", Sepal.Length >= 6.5 & Petal.Length >= 
              4.85 & Petal.Length < 5.05 & Sepal.Length >= 6.25 & Petal.Length >= 
              2.6 ~ "versicolor")
      
      [[2]]
      case_when(Petal.Width < 0.75 ~ "setosa", Petal.Width >= 1.75 & 
          Petal.Width >= 0.75 ~ "virginica", Petal.Width < 1.25 & Sepal.Width < 
          2.25 & Petal.Width < 1.75 & Petal.Width >= 0.75 ~ "versicolor", 
          Petal.Width >= 1.25 & Sepal.Width < 2.25 & Petal.Width < 
              1.75 & Petal.Width >= 0.75 ~ "virginica", Sepal.Length >= 
              7.05 & Sepal.Width >= 2.25 & Petal.Width < 1.75 & Petal.Width >= 
              0.75 ~ "virginica", Petal.Length >= 5.05 & Sepal.Length < 
              7.05 & Sepal.Width >= 2.25 & Petal.Width < 1.75 & Petal.Width >= 
              0.75 ~ "virginica", Sepal.Width >= 2.55 & Petal.Length < 
              5.05 & Sepal.Length < 7.05 & Sepal.Width >= 2.25 & Petal.Width < 
              1.75 & Petal.Width >= 0.75 ~ "versicolor", Sepal.Length >= 
              4.95 & Sepal.Width < 2.55 & Petal.Length < 5.05 & Sepal.Length < 
              7.05 & Sepal.Width >= 2.25 & Petal.Width < 1.75 & Petal.Width >= 
              0.75 ~ "versicolor", Petal.Length < 3.9 & Sepal.Length < 
              4.95 & Sepal.Width < 2.55 & Petal.Length < 5.05 & Sepal.Length < 
              7.05 & Sepal.Width >= 2.25 & Petal.Width < 1.75 & Petal.Width >= 
              0.75 ~ "versicolor", Petal.Length >= 3.9 & Sepal.Length < 
              4.95 & Sepal.Width < 2.55 & Petal.Length < 5.05 & Sepal.Length < 
              7.05 & Sepal.Width >= 2.25 & Petal.Width < 1.75 & Petal.Width >= 
              0.75 ~ "virginica")
      
      [[3]]
      case_when(Petal.Length < 2.35 ~ "setosa", Petal.Length < 4.75 & 
          Petal.Length >= 2.35 ~ "versicolor", Petal.Length < 4.95 & 
          Petal.Width < 1.7 & Petal.Length >= 4.75 & Petal.Length >= 
          2.35 ~ "versicolor", Sepal.Length >= 6 & Petal.Width >= 1.7 & 
          Petal.Length >= 4.75 & Petal.Length >= 2.35 ~ "virginica", 
          Sepal.Width < 2.65 & Petal.Length >= 4.95 & Petal.Width < 
              1.7 & Petal.Length >= 4.75 & Petal.Length >= 2.35 ~ "virginica", 
          Sepal.Width >= 2.65 & Petal.Length >= 4.95 & Petal.Width < 
              1.7 & Petal.Length >= 4.75 & Petal.Length >= 2.35 ~ "versicolor", 
          Petal.Width < 1.85 & Sepal.Length < 6 & Petal.Width >= 1.7 & 
              Petal.Length >= 4.75 & Petal.Length >= 2.35 ~ "versicolor", 
          Petal.Width >= 1.85 & Sepal.Length < 6 & Petal.Width >= 1.7 & 
              Petal.Length >= 4.75 & Petal.Length >= 2.35 ~ "virginica")
      
      [[4]]
      case_when(Petal.Width < 0.8 ~ "setosa", Petal.Width < 1.6 & Petal.Length < 
          4.75 & Petal.Width >= 0.8 ~ "versicolor", Petal.Width >= 
          1.6 & Petal.Length < 4.75 & Petal.Width >= 0.8 ~ "virginica", 
          Petal.Width >= 1.75 & Petal.Length >= 4.75 & Petal.Width >= 
              0.8 ~ "virginica", Petal.Length < 5.05 & Petal.Width < 
              1.75 & Petal.Length >= 4.75 & Petal.Width >= 0.8 ~ "versicolor", 
          Petal.Length >= 5.05 & Petal.Width < 1.75 & Petal.Length >= 
              4.75 & Petal.Width >= 0.8 ~ "virginica")
      
      [[5]]
      case_when(Petal.Length < 2.45 ~ "setosa", Petal.Length < 4.9 & 
          Petal.Width < 1.75 & Petal.Length >= 2.45 ~ "versicolor", 
          Petal.Length >= 4.85 & Petal.Width >= 1.75 & Petal.Length >= 
              2.45 ~ "virginica", Petal.Width < 1.55 & Petal.Length >= 
              4.9 & Petal.Width < 1.75 & Petal.Length >= 2.45 ~ "virginica", 
          Petal.Width >= 1.55 & Petal.Length >= 4.9 & Petal.Width < 
              1.75 & Petal.Length >= 2.45 ~ "versicolor", Sepal.Width < 
              3.1 & Petal.Length < 4.85 & Petal.Width >= 1.75 & Petal.Length >= 
              2.45 ~ "virginica", Sepal.Width >= 3.1 & Petal.Length < 
              4.85 & Petal.Width >= 1.75 & Petal.Length >= 2.45 ~ "versicolor")
      
      [[6]]
      case_when(Petal.Width < 0.8 & Sepal.Length < 5.75 ~ "setosa", 
          Sepal.Width >= 2.55 & Petal.Width >= 0.8 & Sepal.Length < 
              5.75 ~ "versicolor", Petal.Length < 4.75 & Sepal.Length < 
              6.35 & Sepal.Length >= 5.75 ~ "versicolor", Petal.Width < 
              1.55 & Sepal.Length >= 6.35 & Sepal.Length >= 5.75 ~ 
              "versicolor", Petal.Width >= 1.55 & Sepal.Length >= 6.35 & 
              Sepal.Length >= 5.75 ~ "virginica", Petal.Width < 1.35 & 
              Sepal.Width < 2.55 & Petal.Width >= 0.8 & Sepal.Length < 
              5.75 ~ "versicolor", Petal.Width >= 1.35 & Sepal.Width < 
              2.55 & Petal.Width >= 0.8 & Sepal.Length < 5.75 ~ "virginica", 
          Sepal.Length >= 6.05 & Petal.Length >= 4.75 & Sepal.Length < 
              6.35 & Sepal.Length >= 5.75 ~ "virginica", Petal.Width >= 
              1.85 & Sepal.Length < 6.05 & Petal.Length >= 4.75 & Sepal.Length < 
              6.35 & Sepal.Length >= 5.75 ~ "virginica", Petal.Width < 
              1.55 & Petal.Width < 1.85 & Sepal.Length < 6.05 & Petal.Length >= 
              4.75 & Sepal.Length < 6.35 & Sepal.Length >= 5.75 ~ "virginica", 
          Sepal.Length < 5.95 & Petal.Width >= 1.55 & Petal.Width < 
              1.85 & Sepal.Length < 6.05 & Petal.Length >= 4.75 & Sepal.Length < 
              6.35 & Sepal.Length >= 5.75 ~ "versicolor", Petal.Width < 
              1.7 & Sepal.Length >= 5.95 & Petal.Width >= 1.55 & Petal.Width < 
              1.85 & Sepal.Length < 6.05 & Petal.Length >= 4.75 & Sepal.Length < 
              6.35 & Sepal.Length >= 5.75 ~ "versicolor", Petal.Width >= 
              1.7 & Sepal.Length >= 5.95 & Petal.Width >= 1.55 & Petal.Width < 
              1.85 & Sepal.Length < 6.05 & Petal.Length >= 4.75 & Sepal.Length < 
              6.35 & Sepal.Length >= 5.75 ~ "virginica")
      
      [[7]]
      case_when(Petal.Width < 1.55 & Sepal.Width < 2.8 & Sepal.Length < 
          5.55 ~ "versicolor", Petal.Width >= 1.55 & Sepal.Width < 
          2.8 & Sepal.Length < 5.55 ~ "virginica", Petal.Width < 1 & 
          Sepal.Width >= 2.8 & Sepal.Length < 5.55 ~ "setosa", Petal.Width >= 
          1 & Sepal.Width >= 2.8 & Sepal.Length < 5.55 ~ "versicolor", 
          Sepal.Width >= 3.55 & Petal.Length < 4.85 & Sepal.Length >= 
              5.55 ~ "setosa", Petal.Length >= 5.15 & Petal.Length >= 
              4.85 & Sepal.Length >= 5.55 ~ "virginica", Sepal.Length >= 
              6.05 & Sepal.Width < 3.55 & Petal.Length < 4.85 & Sepal.Length >= 
              5.55 ~ "versicolor", Petal.Width >= 1.75 & Petal.Length < 
              5.15 & Petal.Length >= 4.85 & Sepal.Length >= 5.55 ~ 
              "virginica", Petal.Width < 1.65 & Sepal.Length < 6.05 & 
              Sepal.Width < 3.55 & Petal.Length < 4.85 & Sepal.Length >= 
              5.55 ~ "versicolor", Sepal.Length >= 6.5 & Petal.Width < 
              1.75 & Petal.Length < 5.15 & Petal.Length >= 4.85 & Sepal.Length >= 
              5.55 ~ "versicolor", Sepal.Length < 5.95 & Petal.Width >= 
              1.65 & Sepal.Length < 6.05 & Sepal.Width < 3.55 & Petal.Length < 
              4.85 & Sepal.Length >= 5.55 ~ "versicolor", Sepal.Length >= 
              5.95 & Petal.Width >= 1.65 & Sepal.Length < 6.05 & Sepal.Width < 
              3.55 & Petal.Length < 4.85 & Sepal.Length >= 5.55 ~ "virginica", 
          Sepal.Width < 2.35 & Sepal.Length < 6.5 & Petal.Width < 1.75 & 
              Petal.Length < 5.15 & Petal.Length >= 4.85 & Sepal.Length >= 
              5.55 ~ "virginica", Sepal.Width < 2.75 & Sepal.Width >= 
              2.35 & Sepal.Length < 6.5 & Petal.Width < 1.75 & Petal.Length < 
              5.15 & Petal.Length >= 4.85 & Sepal.Length >= 5.55 ~ 
              "versicolor", Sepal.Width >= 2.75 & Sepal.Width >= 2.35 & 
              Sepal.Length < 6.5 & Petal.Width < 1.75 & Petal.Length < 
              5.15 & Petal.Length >= 4.85 & Sepal.Length >= 5.55 ~ 
              "virginica")
      
      [[8]]
      case_when(Petal.Width < 0.7 ~ "setosa", Petal.Width < 1.65 & 
          Petal.Length < 4.75 & Petal.Width >= 0.7 ~ "versicolor", 
          Petal.Width >= 1.65 & Petal.Length < 4.75 & Petal.Width >= 
              0.7 ~ "virginica", Petal.Width >= 1.75 & Petal.Length >= 
              4.75 & Petal.Width >= 0.7 ~ "virginica", Sepal.Length >= 
              7.05 & Petal.Width < 1.75 & Petal.Length >= 4.75 & Petal.Width >= 
              0.7 ~ "virginica", Petal.Length < 4.95 & Sepal.Length < 
              7.05 & Petal.Width < 1.75 & Petal.Length >= 4.75 & Petal.Width >= 
              0.7 ~ "versicolor", Sepal.Length >= 6.5 & Petal.Length >= 
              4.95 & Sepal.Length < 7.05 & Petal.Width < 1.75 & Petal.Length >= 
              4.75 & Petal.Width >= 0.7 ~ "versicolor", Sepal.Length >= 
              6.05 & Sepal.Length < 6.5 & Petal.Length >= 4.95 & Sepal.Length < 
              7.05 & Petal.Width < 1.75 & Petal.Length >= 4.75 & Petal.Width >= 
              0.7 ~ "virginica", Sepal.Width < 2.45 & Sepal.Length < 
              6.05 & Sepal.Length < 6.5 & Petal.Length >= 4.95 & Sepal.Length < 
              7.05 & Petal.Width < 1.75 & Petal.Length >= 4.75 & Petal.Width >= 
              0.7 ~ "virginica", Sepal.Width >= 2.45 & Sepal.Length < 
              6.05 & Sepal.Length < 6.5 & Petal.Length >= 4.95 & Sepal.Length < 
              7.05 & Petal.Width < 1.75 & Petal.Length >= 4.75 & Petal.Width >= 
              0.7 ~ "versicolor")
      
      [[9]]
      case_when(Petal.Width >= 1.7 ~ "virginica", Petal.Length < 2.6 & 
          Petal.Width < 1.7 ~ "setosa", Sepal.Length < 6.25 & Petal.Length >= 
          2.6 & Petal.Width < 1.7 ~ "versicolor", Petal.Length < 5 & 
          Sepal.Length >= 6.25 & Petal.Length >= 2.6 & Petal.Width < 
          1.7 ~ "versicolor", Petal.Length >= 5 & Sepal.Length >= 6.25 & 
          Petal.Length >= 2.6 & Petal.Width < 1.7 ~ "virginica")
      
      [[10]]
      case_when(Petal.Length < 2.35 ~ "setosa", Petal.Length >= 4.95 & 
          Petal.Length >= 2.35 ~ "virginica", Petal.Width < 1.65 & 
          Petal.Length < 4.95 & Petal.Length >= 2.35 ~ "versicolor", 
          Petal.Width >= 1.65 & Petal.Length < 4.95 & Petal.Length >= 
              2.35 ~ "virginica")
      

