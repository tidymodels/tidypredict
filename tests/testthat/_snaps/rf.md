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

