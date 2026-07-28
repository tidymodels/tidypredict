# multiclass H2O RuleFit models are not supported

    Code
      tidypredict_fit(model$fit)
    Condition
      Error in `tidypredict_fit_h2o_rulefit_multinomial()`:
      ! Multiclass H2O RuleFit models are not supported.
      i `h2o.rule_importance()` does not expose the per-class coefficients needed to reproduce the predictions.

