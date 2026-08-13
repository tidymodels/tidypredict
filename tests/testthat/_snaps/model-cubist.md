# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "(pmin(pmax(37.2 + ifelse(is.na(hp), 146.6, hp) * -0.0318 + ifelse(is.na(wt), \n    3.2172, wt) * -3.88, 0, na.rm = TRUE), 57.4, na.rm = TRUE) + \n    (ifelse(ifelse(is.na(disp), 230.72, disp) > 95.1000022888184, \n        pmin(pmax(14.89 + ifelse(is.na(hp), 146.6, hp) * -0.0406 + \n            ifelse(is.na(drat), 3.596, drat) * 2.4, 0, na.rm = TRUE), \n            41.6, na.rm = TRUE), 0) + ifelse(ifelse(is.na(disp), \n        230.72, disp) <= 95.1000022888184, 33.06, 0))/((ifelse(is.na(disp), \n        230.72, disp) > 95.1000022888184) + (ifelse(is.na(disp), \n        230.72, disp) <= 95.1000022888184)) + pmin(pmax(37.26 + \n    ifelse(is.na(wt), 3.2172, wt) * -5.28, 0, na.rm = TRUE), \n    57.4, na.rm = TRUE))/3"

# prediction intervals are not supported

    Code
      tidypredict_interval(model)
    Condition
      Error in `tidypredict_interval()`:
      ! Prediction intervals are not supported for <cubist> models.
      i Only <lm> and <glm> models have prediction intervals.

