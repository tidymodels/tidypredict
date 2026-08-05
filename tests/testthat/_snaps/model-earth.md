# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "20.534817535821 + (ifelse(disp < 145, 145 - disp, 0) * 0.148589866311) + \n    (ifelse(disp > 145, disp - 145, 0) * -0.025012854678)"

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

