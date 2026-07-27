# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "1 - 1/(1 + exp(-0.2524976 + (mpg * 0.1229203) + (cyl * -0.4313279)))"

# errors on unsupported and multiclass models

    Code
      tidypredict_fit(multi_lr)
    Condition
      Error in `parse_model()`:
      ! Only binary classification LiblineaR models are supported.
      i This model has 3 classes.

---

    Code
      tidypredict_fit(multi_svm)
    Condition
      Error in `parse_model()`:
      ! Only binary classification LiblineaR models are supported.
      i This model has 3 classes.

