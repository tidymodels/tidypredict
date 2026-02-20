# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "(case_when(wt <= 2.3325 ~ case_when(disp <= 78.85 ~ 33.525, .default = 27.425), \n    .default = case_when(disp <= 281 ~ case_when(cyl <= 5 ~ case_when(drat <= \n        3.695 ~ 24.4, .default = 22.3666666666667), .default = case_when(wt <= \n        3.3275 ~ case_when(disp <= 152.5 ~ 19.7, .default = 21.1), \n        .default = case_when(disp <= 196.3 ~ 18.92, .default = 18.1))), \n        .default = 14.1)) + case_when(disp <= 266.9 ~ case_when(hp <= \n    80.5 ~ 28.5333333333333, .default = case_when(hp <= 118 ~ \n    case_when(wt <= 1.989 ~ 30.4, .default = case_when(hp <= \n        96 ~ 22.8, .default = case_when(qsec <= 19.725 ~ 21.4, \n        .default = 21.5))), .default = case_when(qsec <= 18.6 ~ \n    19.6, .default = 17.8))), .default = case_when(drat <= 3.035 ~ \n    10.4, .default = case_when(carb <= 2.5 ~ 18.7, .default = case_when(drat <= \n    3.635 ~ case_when(wt <= 4.5625 ~ 15.04, .default = 14.7), \n    .default = 13.3)))) + case_when(drat <= 3.75 ~ case_when(wt <= \n    3.4725 ~ 20.8333333333333, .default = case_when(qsec <= 16.23 ~ \n    13.3, .default = case_when(disp <= 456 ~ case_when(qsec <= \n    17.225 ~ 19.2, .default = case_when(wt <= 4.7075 ~ 16.34, \n    .default = 14.7)), .default = 10.4))), .default = case_when(disp <= \n    130.55 ~ case_when(disp <= 78.85 ~ 32.2333333333333, .default = 27.75), \n    .default = case_when(cyl <= 5 ~ 22.8, .default = case_when(qsec <= \n        17.66 ~ 21, .default = 19.2)))))/3"

# formulas produces correct predictions

    Code
      tidypredict_test(randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 3),
      mtcars, )
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

# classification models error with clear message (#193)

    Code
      tidypredict_fit(model)
    Condition
      Error in `tidypredict_fit_rf_nested()`:
      ! Classification models are not supported for randomForest.
      i Only regression models can be converted to tidy formulas.
      i Classification requires a voting mechanism that cannot be expressed as a single formula.

# .extract_rf_classprob errors on non-randomForest model

    Code
      .extract_rf_classprob(model)
    Condition
      Error in `.extract_rf_classprob()`:
      ! `model` must be <randomForest>, not a <lm> object.

# .extract_rf_classprob errors on regression model

    Code
      .extract_rf_classprob(model)
    Condition
      Error in `.extract_rf_classprob()`:
      ! Model is not a classification model.
      i Use `tidypredict_fit()` for regression models.

