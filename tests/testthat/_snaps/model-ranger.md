# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "(case_when(cyl <= 5 & hp <= 118 ~ 26.7, cyl > 5 & hp <= 118 ~ \n    21.1333333333333, hp <= 205 & hp > 118 ~ 17.48, .default = 14.5416666666667) + \n    case_when(hp <= 80.5 ~ 32.2333333333333, hp <= 118 & hp > \n        80.5 ~ 21.9545454545455, .default = 16.5722222222222) + \n    case_when(disp <= 101.55 ~ 31.9, cyl <= 7 & disp > 101.55 ~ \n        20.8384615384615, .default = 14.8785714285714))/3L"

# formulas produces correct predictions

    Code
      tidypredict_test(ranger::ranger(mpg ~ ., data = mtcars, num.trees = 3,
      max.depth = 2, seed = 100, num.threads = 2), mtcars)
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

# classification models error with clear message (#191)

    Code
      tidypredict_fit(model)
    Condition
      Error in `tidypredict_fit_ranger()`:
      ! Classification models are not supported for ranger.
      i Only regression models can be converted to tidy formulas.
      i Classification requires a voting mechanism that cannot be expressed as a single formula.

# .extract_ranger_classprob errors on non-ranger model

    Code
      .extract_ranger_classprob(model)
    Condition
      Error in `.extract_ranger_classprob()`:
      ! `model` must be <ranger>, not a <lm> object.

# .extract_ranger_classprob errors without probability = TRUE

    Code
      .extract_ranger_classprob(model)
    Condition
      Error in `.extract_ranger_classprob()`:
      ! Model does not contain probability information.
      i Fit the ranger model with `probability = TRUE`.

