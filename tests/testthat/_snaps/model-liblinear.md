# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "1 - 1/(1 + exp(-0.2524976 + (mpg * 0.1229203) + (cyl * -0.4313279)))"

# errors on non-logistic and multiclass models

    Code
      tidypredict_fit(svm)
    Condition
      Error in `parse_model()`:
      ! Only logistic regression LiblineaR models are supported.
      i The model `type` must be one of 0, 6, or 7, not 1.

---

    Code
      tidypredict_fit(multi)
    Condition
      Error in `parse_model()`:
      ! Only binary classification LiblineaR models are supported.
      i This model has 3 classes.

