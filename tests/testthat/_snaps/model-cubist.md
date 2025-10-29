# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "(ifelse(TRUE, 37.2 + hp * -0.0318 + wt * -3.88, 0)/TRUE + (ifelse(disp > \n    95.099998, 14.89 + hp * -0.0406 + drat * 2.4, 0) + ifelse(disp <= \n    95.099998, 33.06, 0))/((disp > 95.099998) + (disp <= 95.099998)) + \n    ifelse(TRUE, 37.26 + wt * -5.28, 0)/TRUE)/3"

# Model can be saved and re-loaded

    Code
      tidypredict_fit(pm)
    Output
      (ifelse(TRUE, 37.2 + hp * -0.0318 + wt * -3.88, 0) + ifelse(disp > 
          95.099998, 14.89 + hp * -0.0406 + drat * 2.4, 0) + ifelse(disp <= 
          95.099998, 33.06, 0) + ifelse(TRUE, 37.26 + wt * -5.28, 0))/3

