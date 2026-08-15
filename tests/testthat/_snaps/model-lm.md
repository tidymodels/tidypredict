# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "1.520331147866 + (wt * -0.372988616484) + (cyl * 0.013885491477)"

# prediction intervals need a QR decomposition (#308)

    Code
      tidypredict_interval(pm)
    Condition
      Error in `tidypredict_interval()`:
      x Unable to calculate the inverse of the QR decomposition.
      i Prediction intervals are not available for this model, but `tidypredict_fit()` is.

