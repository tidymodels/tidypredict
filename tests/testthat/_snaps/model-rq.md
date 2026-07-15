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

# returns one expression per quantile for multiple quantiles

    Code
      lapply(tf, rlang::expr_text)
    Output
      $quantile_0.25
      [1] "35.2495867768595 + (wt * -2.64462809917355) + (cyl * -1.37066115702479)"
      
      $quantile_0.50
      [1] "38.8714285714286 + (wt * -2.67857142857143) + (cyl * -1.74285714285714)"
      
      $quantile_0.75
      [1] "40.9812164579606 + (wt * -3.5778175313059) + (cyl * -1.2919946332737)"
      

