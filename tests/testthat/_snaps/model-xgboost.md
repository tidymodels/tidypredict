# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "0 + case_when((qsec < 19.9549999 | is.na(qsec)) & (wt < 3.18000007 | \n    is.na(wt)) ~ 0.138461545, qsec >= 19.9549999 & (wt < 3.18000007 | \n    is.na(wt)) ~ -0.100000009, (hp < 290 | is.na(hp)) & wt >= \n    3.18000007 ~ -0.141666666, hp >= 290 & wt >= 3.18000007 ~ \n    0.075000003) + case_when((qsec < 19.9549999 | is.na(qsec)) & \n    (wt < 3.01250005 | is.na(wt)) ~ 0.0994230807, qsec >= 19.9549999 & \n    (wt < 3.01250005 | is.na(wt)) ~ -0.0599999987, (hp < 254.5 | \n    is.na(hp)) & wt >= 3.01250005 ~ -0.102500007, hp >= 254.5 & \n    wt >= 3.01250005 ~ 0.0786538497) + case_when((gear < 3.5 | \n    is.na(gear)) ~ -0.0735312551, (wt < 3.01250005 | is.na(wt)) & \n    gear >= 3.5 ~ 0.0720817298, wt >= 3.01250005 & gear >= 3.5 ~ \n    -0.0186758228) + case_when((gear < 3.5 | is.na(gear)) ~ -0.0528505854, \n    (qsec < 19.9500008 | is.na(qsec)) & gear >= 3.5 ~ 0.0427994467, \n    qsec >= 19.9500008 & gear >= 3.5 ~ -0.0515981652) + 0.5"

# Model can be saved and re-loaded

    Code
      tidypredict_fit(pm)
    Output
      0 + case_when((qsec < 19.9549999 | is.na(qsec)) & (wt < 3.1800001 | 
          is.na(wt)) ~ 0.1384615, qsec >= 19.9549999 & (wt < 3.1800001 | 
          is.na(wt)) ~ -0.1, (hp < 290 | is.na(hp)) & wt >= 3.1800001 ~ 
          -0.1416667, hp >= 290 & wt >= 3.1800001 ~ 0.075) + case_when((qsec < 
          19.9549999 | is.na(qsec)) & (wt < 3.0125 | is.na(wt)) ~ 0.0994231, 
          qsec >= 19.9549999 & (wt < 3.0125 | is.na(wt)) ~ -0.06, (hp < 
              254.5 | is.na(hp)) & wt >= 3.0125 ~ -0.1025, hp >= 254.5 & 
              wt >= 3.0125 ~ 0.0786538) + case_when((gear < 3.5 | is.na(gear)) ~ 
          -0.0735313, (wt < 3.0125 | is.na(wt)) & gear >= 3.5 ~ 0.0720817, 
          wt >= 3.0125 & gear >= 3.5 ~ -0.0186758) + case_when((gear < 
          3.5 | is.na(gear)) ~ -0.0528506, (qsec < 19.9500008 | is.na(qsec)) & 
          gear >= 3.5 ~ 0.0427994, qsec >= 19.9500008 & gear >= 3.5 ~ 
          -0.0515982) + 0.5

# formulas produces correct predictions

    Code
      tidypredict_test(xgboost::xgb.train(params = list(max_depth = 2, objective = "reg:squarederror",
        base_score = 0.5), data = xgb_bin_data, nrounds = 4), mtcars, xg_df = xgb_bin_data,
      threshold = 1e-07)
    Output
      tidypredict test results
      Difference threshold: 1e-07
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(xgboost::xgb.train(params = list(max_depth = 2, objective = "binary:logitraw",
        base_score = 0.5), data = xgb_bin_data, nrounds = 4), mtcars, xg_df = xgb_bin_data,
      threshold = 1e-07)
    Output
      tidypredict test results
      Difference threshold: 1e-07
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(xgboost::xgb.train(params = list(max_depth = 2, objective = "reg:logistic",
        base_score = 0.5), data = xgb_bin_data, nrounds = 4), mtcars, xg_df = xgb_bin_data,
      threshold = 1e-07)
    Output
      tidypredict test results
      Difference threshold: 1e-07
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(xgboost::xgb.train(params = list(max_depth = 2, objective = "binary:logistic",
        base_score = 0.5), data = xgb_bin_data, nrounds = 4), mtcars, xg_df = xgb_bin_data,
      threshold = 1e-07)
    Output
      tidypredict test results
      Difference threshold: 1e-07
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(xgboost::xgb.train(params = list(max_depth = 2, objective = "reg:logistic",
        base_score = mean(mtcars$am)), data = xgb_bin_data, nrounds = 4), mtcars,
      xg_df = xgb_bin_data, threshold = 1e-07)
    Output
      tidypredict test results
      Difference threshold: 1e-07
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(xgboost::xgb.train(params = list(max_depth = 2, objective = "binary:logistic",
        base_score = mean(mtcars$am)), data = xgb_bin_data, nrounds = 4), mtcars,
      xg_df = xgb_bin_data, threshold = 1e-07)
    Output
      tidypredict test results
      Difference threshold: 1e-07
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(xgboost::xgb.train(params = list(max_depth = 2, objective = "reg:logistic",
        base_score = 0.5), data = xgb_bin_data, nrounds = 50), mtcars, xg_df = xgb_bin_data,
      threshold = 1e-07)
    Output
      tidypredict test results
      Difference threshold: 1e-07
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(xgboost::xgb.train(params = list(max_depth = 2, objective = "binary:logistic",
        base_score = 0.5), data = xgb_bin_data, nrounds = 50), mtcars, xg_df = xgb_bin_data,
      threshold = 1e-07)
    Output
      tidypredict test results
      Difference threshold: 1e-07
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(xgboost::xgb.train(params = list(max_depth = 20, objective = "reg:logistic",
        base_score = 0.5), data = xgb_bin_data, nrounds = 4), mtcars, xg_df = xgb_bin_data,
      threshold = 1e-07)
    Output
      tidypredict test results
      Difference threshold: 1e-07
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(xgboost::xgb.train(params = list(max_depth = 20, objective = "binary:logistic",
        base_score = 0.5), data = xgb_bin_data, nrounds = 4), mtcars, xg_df = xgb_bin_data,
      threshold = 1e-07)
    Output
      tidypredict test results
      Difference threshold: 1e-07
      
       All results are within the difference threshold

