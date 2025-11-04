# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "(37.2 + hp * -0.0318 + wt * -3.88 + (ifelse(disp > 95.099998, \n    14.89 + hp * -0.0406 + drat * 2.4, 0) + ifelse(disp <= 95.099998, \n    33.06, 0))/((disp > 95.099998) + (disp <= 95.099998)) + (37.26 + \n    wt * -5.28))/3"

# formulas produces correct predictions

    Code
      tidypredict_test(model, non_split_data, threshold = 1e-05)
    Output
      tidypredict test results
      Difference threshold: 1e-05
      
       All results are within the difference threshold

