# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "(case_when(case_when(!is.na(wt) ~ wt < 2.975, !is.na(disp) ~ \n    disp < 163.8, !is.na(cyl) ~ cyl < 7, .default = FALSE) ~ \n    23.0583333333333, .default = case_when(case_when(!is.na(wt) ~ \n    wt < 3.545, !is.na(disp) ~ disp < 380, !is.na(cyl) ~ cyl < \n    7, .default = FALSE) ~ 16.9125, .default = 15.125)) + (case_when(case_when(!is.na(wt) ~ \n    wt < 3.16, !is.na(disp) ~ disp < 163.8, !is.na(cyl) ~ cyl < \n    7, .default = FALSE) ~ 24.3642857142857, .default = 15.7444444444444) + \n    case_when(case_when(!is.na(wt) ~ wt < 2.26, !is.na(disp) ~ \n        disp < 101.55, !is.na(cyl) ~ cyl < 5, .default = FALSE) ~ \n        30.0857142857143, .default = case_when(case_when(!is.na(cyl) ~ \n        cyl < 7, !is.na(disp) ~ disp < 250.4, !is.na(wt) ~ wt < \n        3.3125, .default = TRUE) ~ 20.4428571428571, .default = 14.6454545454545))))/3L"

# unsupported base models error

    Code
      tidypredict_fit(model)
    Condition
      Error in `parse_model()`:
      ! Only "CART" and "C5.0" bagged models are supported, not "MARS".
      i Fit the model with `base_model = "CART"` or `base_model = "C5.0"`.

---

    Code
      parse_model(model)
    Condition
      Error in `parse_model()`:
      ! Only "CART" and "C5.0" bagged models are supported, not "MARS".
      i Fit the model with `base_model = "CART"` or `base_model = "C5.0"`.

# .extract_bagger_trees errors on bad input

    Code
      .extract_bagger_trees(list())
    Condition
      Error in `.extract_bagger_trees()`:
      ! `model` must be <bagger>, not an empty list.

---

    Code
      .extract_bagger_trees(bagger_cls())
    Condition
      Error in `.extract_bagger_trees()`:
      ! Classification models are not supported.
      i Use `.extract_bagger_classprob()` for classification models.

# .extract_bagger_classprob errors on bad input

    Code
      .extract_bagger_classprob(list())
    Condition
      Error in `.extract_bagger_classprob()`:
      ! `model` must be <bagger>, not an empty list.

---

    Code
      .extract_bagger_classprob(bagger_reg())
    Condition
      Error in `.extract_bagger_classprob()`:
      ! Model is not a classification model.
      i Use `.extract_bagger_trees()` for regression models.

# C5.0 base models with a cost matrix error

    Code
      tidypredict_fit(model)
    Condition
      Error in `parse_model()`:
      ! tidypredict does not support C5.0 models fitted with a cost matrix (`costs`).

---

    Code
      parse_model(model)
    Condition
      Error in `parse_model()`:
      ! tidypredict does not support C5.0 models fitted with a cost matrix (`costs`).

