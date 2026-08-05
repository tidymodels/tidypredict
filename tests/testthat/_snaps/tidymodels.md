# works with rand_forest() and the partykit engine

    Code
      tidypredict_fit(cls)
    Condition
      Error in `cforest_check_regression()`:
      ! Classification models are not supported for cforest.
      i Only regression models can be converted to tidy formulas.
      i Classification requires a voting mechanism that cannot be expressed as a single formula.

# works with rand_forest() and the aorsf engine

    Code
      tidypredict_fit(cls)
    Condition
      Error in `aorsf_check_supported()`:
      ! Classification models are not supported for aorsf.
      i Only regression models can be converted to tidy formulas.
      i Classification requires a voting mechanism that cannot be expressed as a single formula.

# bart is handled with parsnip

    Code
      tidypredict_fit(cls)
    Condition
      Error in `check_bart_supported()`:
      ! Classification `dbarts::bart()` models are not supported.
      i Only regression models can be converted to tidy formulas.
      i Classification uses the probit link, which cannot be translated to SQL.

