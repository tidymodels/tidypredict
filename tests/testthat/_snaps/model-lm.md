# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "1.520331147866 + ((wt * -0.372988616484) + (cyl * 0.013885491477))"

# we get better error from QR decomposition issues (#124)

    Code
      tidypredict::tidypredict_fit(lm_fit)
    Condition
      Error in `parse_model()`:
      x Unable to calculate inverse of QR decomposition.
      i This is likely happening because the predictors contain a linear combination of predictors. Please remove and try again.

