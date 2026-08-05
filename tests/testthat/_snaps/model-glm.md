# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "1.520331147866 + (wt * -0.372988616484) + (cyl * 0.013885491477)"

# tidypredict_interval errors for non-gaussian glm

    Code
      tidypredict_interval(model)
    Condition
      Error in `te_interval_glm()`:
      ! Combination of family and link are not supported for prediction intervals.

