# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "38.8714285714286 + (wt * -2.67857142857143) + (cyl * -1.74285714285714)"

# formulas produces correct predictions

    Code
      tidypredict_test(quantreg::rq(mpg ~ wt + cyl + disp, data = mtcars), mtcars)
    Condition
      Warning in `rq.fit.br()`:
      Solution may be nonunique
    Output
      tidypredict test results
      Difference threshold: 1e-12
      
       All results are within the difference threshold

# errors for multiple quantiles

    Code
      tidypredict_fit(model)
    Condition
      Error in `tidypredict_fit()`:
      ! tidypredict does not support `quantreg::rq()` models fitted with more than one quantile (`tau`).
      i Fit a separate model for each quantile level instead.

