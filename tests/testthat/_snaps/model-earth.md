# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "20.534817535821 + (ifelse(disp < 145, 145 - disp, 0) * 0.148589866311) + \n    (ifelse(disp > 145, disp - 145, 0) * -0.025012854678)"

# an ordered factor is rejected (#323)

    Code
      tidypredict_fit(earth::earth(y ~ x + z + f, data = d))
    Condition
      Error in `acceptable_contrasts()`:
      ! The treatment contrast is the only one supported at this time. Field(s) with an invalid contrast are: "f".

# a global non-treatment contrast is rejected (#323)

    Code
      tidypredict_fit(earth::earth(y ~ x + z + f, data = d))
    Condition
      Error in `acceptable_contrasts()`:
      ! The treatment contrast is the only one supported at this time. Field(s) with an invalid contrast are: "f".

# a factor level containing a colon is rejected

    Code
      tidypredict_fit(earth::earth(y ~ x + z + f, data = d))
    Condition
      Error in `acceptable_contrasts()`:
      ! The treatment contrast is the only one supported at this time. Field(s) with an invalid contrast are: "f".

# .extract_earth_multiclass errors on non-earth model

    Code
      .extract_earth_multiclass(model)
    Condition
      Error in `.extract_earth_multiclass()`:
      ! `model` must be <earth>, not a <lm> object.

# .extract_earth_multiclass errors on binary model

    Code
      .extract_earth_multiclass(model)
    Condition
      Error in `.extract_earth_multiclass()`:
      ! Model does not contain multiclass information.
      i Fit the earth model with `glm = TRUE` for classification.

# .extract_earth_multiclass errors on regression model

    Code
      .extract_earth_multiclass(model)
    Condition
      Error in `.extract_earth_multiclass()`:
      ! Model does not contain multiclass information.
      i Fit the earth model with `glm = TRUE` for classification.

