# classification errors with clear message

    Code
      tidypredict_fit(model)
    Condition
      Error in `aorsf_check_supported()`:
      ! Classification models are not supported for aorsf.
      i Only regression models can be converted to tidy formulas.
      i Classification requires a voting mechanism that cannot be expressed as a single formula.

---

    Code
      parse_model(model)
    Condition
      Error in `aorsf_check_supported()`:
      ! Classification models are not supported for aorsf.
      i Only regression models can be converted to tidy formulas.
      i Classification requires a voting mechanism that cannot be expressed as a single formula.

# non-numeric predictors error with clear message

    Code
      tidypredict_fit(model)
    Condition
      Error in `aorsf_check_supported()`:
      ! Only numeric predictors are supported for aorsf.
      i Oblique splits on non-numeric predictor(s) "cyl" cannot be expressed as a single formula.

