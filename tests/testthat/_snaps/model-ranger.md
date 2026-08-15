# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "(case_when(is.na(hp) | hp <= 118 ~ case_when(is.na(cyl) | cyl <= \n    5 ~ 26.7, .default = 21.1333333333333), .default = case_when(is.na(hp) | \n    hp <= 205 ~ 17.48, .default = 14.5416666666667)) + (case_when(is.na(hp) | \n    hp <= 80.5 ~ 32.2333333333333, .default = case_when(is.na(hp) | \n    hp <= 118 ~ 21.9545454545455, .default = 16.5722222222222)) + \n    case_when(is.na(disp) | disp <= 101.55 ~ 31.9, .default = case_when(is.na(cyl) | \n        cyl <= 7 ~ 20.8384615384615, .default = 14.8785714285714))))/3"

# classification models error with clear message (#191)

    Code
      tidypredict_fit(model)
    Condition
      Error in `tidypredict_fit_ranger_nested()`:
      ! Classification models are not supported for ranger.
      i Only regression models can be converted to tidy formulas.
      i Classification requires a voting mechanism that cannot be expressed as a single formula.

# probability and survival forests error with clear message (#301)

    Code
      tidypredict_fit(model)
    Condition
      Error in `tidypredict_fit_ranger_nested()`:
      ! Probability forests are not supported for ranger.
      i A forest fit with `probability = TRUE` predicts one probability per class, which cannot be written as a single formula.
      i Only regression models can be converted to tidy formulas.

---

    Code
      parse_model(model)
    Condition
      Error in `parse_model()`:
      ! Probability forests are not supported for ranger.
      i A forest fit with `probability = TRUE` predicts one probability per class, which cannot be written as a single formula.
      i Only regression models can be converted to tidy formulas.

---

    Code
      tidypredict_fit(model)
    Condition
      Error in `tidypredict_fit_ranger_nested()`:
      ! Survival forests are not supported for ranger.
      i A survival forest predicts a curve over time rather than a single value, which cannot be written as a single formula.
      i Only regression models can be converted to tidy formulas.

---

    Code
      parse_model(model)
    Condition
      Error in `parse_model()`:
      ! Survival forests are not supported for ranger.
      i A survival forest predicts a curve over time rather than a single value, which cannot be written as a single formula.
      i Only regression models can be converted to tidy formulas.

# .extract_ranger_classprob errors on non-ranger model

    Code
      .extract_ranger_classprob(model)
    Condition
      Error in `.extract_ranger_classprob()`:
      ! `model` must be <ranger>, not a <lm> object.

# .extract_ranger_classprob errors without probability = TRUE

    Code
      .extract_ranger_classprob(model)
    Condition
      Error in `.extract_ranger_classprob()`:
      ! Model does not contain probability information.
      i Fit the ranger model with `probability = TRUE`.

# .extract_ranger_trees errors on non-ranger model

    Code
      .extract_ranger_trees(model)
    Condition
      Error in `.extract_ranger_trees()`:
      ! `model` must be <ranger>, not a <lm> object.

# .extract_ranger_trees errors on classification model

    Code
      .extract_ranger_trees(model)
    Condition
      Error in `.extract_ranger_trees()`:
      ! Classification models are not supported.
      i Use `.extract_ranger_classprob()` for classification models.

# v2 parsed classification model errors

    Code
      tidypredict_fit(pm)
    Condition
      Error in `tidypredict_fit_ranger()`:
      ! Classification models are not supported for ranger.
      i Only regression models can be converted to tidy formulas.
      i Classification requires a voting mechanism that cannot be expressed as a single formula.

# parse_model.ranger errors on classification

    Code
      parse_model(model)
    Condition
      Error in `parse_model()`:
      ! Classification models are not supported for ranger.
      i Only regression models can be converted to tidy formulas.
      i Classification requires a voting mechanism that cannot be expressed as a single formula.

