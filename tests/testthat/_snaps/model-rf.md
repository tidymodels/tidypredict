# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "(case_when(disp <= 78.85 & wt <= 2.3325 ~ 33.525, disp > 78.85 & \n    wt <= 2.3325 ~ 27.425, disp > 281 & wt > 2.3325 ~ 14.1, drat <= \n    3.695 & cyl <= 5 & disp <= 281 & wt > 2.3325 ~ 24.4, drat > \n    3.695 & cyl <= 5 & disp <= 281 & wt > 2.3325 ~ 22.3666666666667, \n    disp <= 152.5 & wt <= 3.3275 & cyl > 5 & disp <= 281 & wt > \n        2.3325 ~ 19.7, disp > 152.5 & wt <= 3.3275 & cyl > 5 & \n        disp <= 281 & wt > 2.3325 ~ 21.1, disp <= 196.3 & wt > \n        3.3275 & cyl > 5 & disp <= 281 & wt > 2.3325 ~ 18.92, \n    .default = 18.1) + case_when(hp <= 80.5 & disp <= 266.9 ~ \n    28.5333333333333, drat <= 3.035 & disp > 266.9 ~ 10.4, carb <= \n    2.5 & drat > 3.035 & disp > 266.9 ~ 18.7, wt <= 1.989 & hp <= \n    118 & hp > 80.5 & disp <= 266.9 ~ 30.4, qsec <= 18.6 & hp > \n    118 & hp > 80.5 & disp <= 266.9 ~ 19.6, qsec > 18.6 & hp > \n    118 & hp > 80.5 & disp <= 266.9 ~ 17.8, drat > 3.635 & carb > \n    2.5 & drat > 3.035 & disp > 266.9 ~ 13.3, hp <= 96 & wt > \n    1.989 & hp <= 118 & hp > 80.5 & disp <= 266.9 ~ 22.8, wt <= \n    4.5625 & drat <= 3.635 & carb > 2.5 & drat > 3.035 & disp > \n    266.9 ~ 15.04, wt > 4.5625 & drat <= 3.635 & carb > 2.5 & \n    drat > 3.035 & disp > 266.9 ~ 14.7, qsec <= 19.725 & hp > \n    96 & wt > 1.989 & hp <= 118 & hp > 80.5 & disp <= 266.9 ~ \n    21.4, .default = 21.5) + case_when(wt <= 3.4725 & drat <= \n    3.75 ~ 20.8333333333333, qsec <= 16.23 & wt > 3.4725 & drat <= \n    3.75 ~ 13.3, disp <= 78.85 & disp <= 130.55 & drat > 3.75 ~ \n    32.2333333333333, disp > 78.85 & disp <= 130.55 & drat > \n    3.75 ~ 27.75, cyl <= 5 & disp > 130.55 & drat > 3.75 ~ 22.8, \n    disp > 456 & qsec > 16.23 & wt > 3.4725 & drat <= 3.75 ~ \n        10.4, qsec <= 17.66 & cyl > 5 & disp > 130.55 & drat > \n        3.75 ~ 21, qsec > 17.66 & cyl > 5 & disp > 130.55 & drat > \n        3.75 ~ 19.2, qsec <= 17.225 & disp <= 456 & qsec > 16.23 & \n        wt > 3.4725 & drat <= 3.75 ~ 19.2, wt <= 4.7075 & qsec > \n        17.225 & disp <= 456 & qsec > 16.23 & wt > 3.4725 & drat <= \n        3.75 ~ 16.34, .default = 14.7))/3L"

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
      Error in `tidypredict_fit_randomForest()`:
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

