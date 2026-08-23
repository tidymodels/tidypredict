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

# tidypredict_class_trees errors on non-ranger model

    Code
      tidypredict_class_trees(model)
    Condition
      Error in `tidypredict_class_trees()`:
      ! `tidypredict_class_trees()` is not available for models of class <lm>.

# tidypredict_class_trees errors without probability = TRUE

    Code
      tidypredict_class_trees(model)
    Condition
      Error in `tidypredict_class_trees()`:
      ! Model does not contain probability information.
      i Fit the ranger model with `probability = TRUE`.

# tidypredict_trees errors on non-ranger model

    Code
      tidypredict_trees(model)
    Condition
      Error in `tidypredict_trees()`:
      ! `tidypredict_trees()` is not available for models of class <lm>.

# tidypredict_trees errors on classification model

    Code
      tidypredict_trees(model)
    Condition
      Error in `tidypredict_trees()`:
      ! Classification models are not supported.
      i Use `tidypredict_class_trees()` for classification models.

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

