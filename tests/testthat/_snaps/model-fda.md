# returns the right output

    Code
      lapply(lps, round_print)
    Output
      [[1]]
      [1] "-15.77129 + (Sepal.Length * 6.443631) + (Sepal.Width * 12.38706) + (Petal.Length * -17.29227) + (Petal.Width * -21.19393)"
      
      [[2]]
      [1] "-2.040818 + (Sepal.Length * -1.562448) + (Sepal.Width * -4.46535) + (Petal.Length * 4.791495) + (Petal.Width * 3.125087)"
      
      [[3]]
      [1] "-34.19971 + (Sepal.Length * -4.881183) + (Sepal.Width * -7.921708) + (Petal.Length * 12.50077) + (Petal.Width * 18.06885)"
      

# unsupported fda fits are rejected

    Code
      tidypredict_fit(poly_model)
    Condition
      Error in `parse_model()`:
      ! Only `mda::polyreg()` fits with `degree = 1` are supported.
      i This model was fit with `degree = 2`.

---

    Code
      tidypredict_fit(mars_model)
    Condition
      Error in `parse_model()`:
      ! The `method` used to fit this mda model is not supported.
      i Only `mda::polyreg()` and `mda::gen.ridge()` are supported, not <mars>.

---

    Code
      tidypredict_fit(mda_model)
    Condition
      Error in `parse_model()`:
      ! `mda::mda()` models are not supported.
      i Only `mda::fda()` discriminant models are supported.

---

    Code
      parse_model(mda_model)
    Condition
      Error in `parse_model()`:
      ! `mda::mda()` models are not supported.
      i Only `mda::fda()` discriminant models are supported.

# tidypredict_test errors for fda models

    Code
      tidypredict_test(model, iris)
    Condition
      Error in `tidypredict_test()`:
      ! `tidypredict_test()` does not support `mda::fda()` models.
      i Use `tidypredict_fit()` directly for multiclass predictions.

# inline functions in the formula are rejected

    Code
      tidypredict_fit(model)
    Condition
      Error in `acceptable_lm()`:
      x Functions inside the formula are not supported.
      i Functions detected: "log".  Use `dplyr` transformations to prepare the data.

