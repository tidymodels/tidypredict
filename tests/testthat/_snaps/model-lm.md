# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "1.520331147866 + (wt * -0.372988616484) + (cyl * 0.013885491477)"

# an ordered factor is rejected

    Code
      tidypredict_fit(lm(y ~ x + g, data = df))
    Condition
      Error in `acceptable_lm()`:
      ! The treatment contrast is the only one supported at this time. Field(s) with an invalid contrast are: "g".

---

    Code
      tidypredict_fit(glm(y ~ x + g, data = df))
    Condition
      Error in `acceptable_lm()`:
      ! The treatment contrast is the only one supported at this time. Field(s) with an invalid contrast are: "g".

# prediction intervals need a QR decomposition (#308)

    Code
      tidypredict_interval(pm)
    Condition
      Error in `tidypredict_interval()`:
      x Unable to calculate the inverse of the QR decomposition.
      i Prediction intervals are not available for this model, but `tidypredict_fit()` is.

