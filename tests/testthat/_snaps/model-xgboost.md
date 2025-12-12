# formulas produces correct predictions

    Code
      tidypredict_test(xgboost::xgb.train(params = list(max_depth = 2, objective = "reg:squarederror"),
      data = xgb_bin_data, nrounds = 4), mtcars, xg_df = xgb_bin_data, threshold = 1e-07)
    Output
      tidypredict test results
      Difference threshold: 1e-07
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(xgboost::xgb.train(params = list(max_depth = 2, objective = "binary:logitraw"),
      data = xgb_bin_data, nrounds = 4), mtcars, xg_df = xgb_bin_data, threshold = 1e-07)
    Output
      tidypredict test results
      Difference threshold: 1e-07
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(xgboost::xgb.train(params = list(max_depth = 2, objective = "reg:logistic"),
      data = xgb_bin_data, nrounds = 4), mtcars, xg_df = xgb_bin_data, threshold = 1e-07)
    Output
      tidypredict test results
      Difference threshold: 1e-07
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(xgboost::xgb.train(params = list(max_depth = 2, objective = "binary:logistic"),
      data = xgb_bin_data, nrounds = 4), mtcars, xg_df = xgb_bin_data, threshold = 1e-07)
    Output
      tidypredict test results
      Difference threshold: 1e-07
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(xgboost::xgb.train(params = list(max_depth = 2, objective = "reg:tweedie"),
      data = xgb_bin_data, nrounds = 4), mtcars, xg_df = xgb_bin_data, threshold = 1e-07)
    Output
      tidypredict test results
      Difference threshold: 1e-07
      
       All results are within the difference threshold

---

    Code
      tidypredict_test(xgboost::xgb.train(params = list(max_depth = 2, objective = "count:poisson"),
      data = xgb_bin_data, nrounds = 4), mtcars, xg_df = xgb_bin_data, threshold = 1e-07)
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

