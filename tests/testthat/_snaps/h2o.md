# multiclass H2O RuleFit models are not supported

    Code
      tidypredict_fit(model$fit)
    Condition
      Error in `tidypredict_fit_h2o_rulefit_multinomial()`:
      ! Multiclass H2O RuleFit models are not supported.
      i `h2o.rule_importance()` does not expose the per-class coefficients needed to reproduce the predictions.

# non-GBM H2O models are not supported

    Code
      tidypredict_fit(drf_reg)
    Condition
      Error in `tidypredict_fit()`:
      ! Only h2o GBM and RuleFit models are supported.
      i This model was fit with "drf".

---

    Code
      tidypredict_fit(drf_bin)
    Condition
      Error in `tidypredict_fit()`:
      ! Only h2o GBM and RuleFit models are supported.
      i This model was fit with "drf".

---

    Code
      tidypredict_fit(drf_mul)
    Condition
      Error in `tidypredict_fit()`:
      ! Only h2o GBM and RuleFit models are supported.
      i This model was fit with "drf".

---

    Code
      tidypredict_fit(h2o::h2o.glm(x, "mpg", hf))
    Condition
      Error in `tidypredict_fit()`:
      ! Only h2o GBM and RuleFit models are supported.
      i This model was fit with "glm".

