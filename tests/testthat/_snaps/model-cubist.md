# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "(37.2 + hp * -0.0318 + wt * -3.88 + (ifelse(disp > 95.1000022888184, \n    14.89 + hp * -0.0406 + drat * 2.4, 0) + ifelse(disp <= 95.1000022888184, \n    33.06, 0))/((disp > 95.1000022888184) + (disp <= 95.1000022888184)) + \n    (37.26 + wt * -5.28))/3"

# prediction intervals are not supported

    Code
      tidypredict_interval(model)
    Condition
      Error in `tidypredict_interval()`:
      ! Prediction intervals are not supported for <cubist> models.
      i Only <lm> and <glm> models have prediction intervals.

