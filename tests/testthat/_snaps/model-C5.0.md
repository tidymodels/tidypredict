# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(is.na(wt) | is.na(cyl) ~ case_when(case_when(is.na(cyl) ~ \n    0.5625, cyl <= 6.00000023841858 ~ 1, .default = 0) * (case_when(is.na(cyl) ~ \n    0.611111111111111, cyl <= 4.00000023841858 ~ 1, .default = 0) * \n    0.0909090909090909 + (1 - case_when(is.na(cyl) ~ 0.611111111111111, \n    cyl <= 4.00000023841858 ~ 1, .default = 0)) * (case_when(is.na(wt) ~ \n    0.428571428571429, wt <= 2.87500011920929 ~ 1, .default = 0) * \n    1 + (1 - case_when(is.na(wt) ~ 0.428571428571429, wt <= 2.87500011920929 ~ \n    1, .default = 0)) * 0)) + (1 - case_when(is.na(cyl) ~ 0.5625, \n    cyl <= 6.00000023841858 ~ 1, .default = 0)) * 1 >= case_when(is.na(cyl) ~ \n    0.5625, cyl <= 6.00000023841858 ~ 1, .default = 0) * (case_when(is.na(cyl) ~ \n    0.611111111111111, cyl <= 4.00000023841858 ~ 1, .default = 0) * \n    0.909090909090909 + (1 - case_when(is.na(cyl) ~ 0.611111111111111, \n    cyl <= 4.00000023841858 ~ 1, .default = 0)) * (case_when(is.na(wt) ~ \n    0.428571428571429, wt <= 2.87500011920929 ~ 1, .default = 0) * \n    0 + (1 - case_when(is.na(wt) ~ 0.428571428571429, wt <= 2.87500011920929 ~ \n    1, .default = 0)) * 1)) + (1 - case_when(is.na(cyl) ~ 0.5625, \n    cyl <= 6.00000023841858 ~ 1, .default = 0)) * 0 ~ \"0\", .default = \"1\"), \n    .default = case_when(cyl <= 6.00000023841858 ~ case_when(cyl <= \n        4.00000023841858 ~ \"1\", .default = case_when(wt <= 2.87500011920929 ~ \n        \"0\", .default = \"1\")), .default = \"0\"))"

# a model with no tree is reported clearly (#287)

    Code
      tidypredict_fit(structure(list(tree = "", levels = c("hi", "lo"), names = ""),
      class = "C5.0"))
    Condition
      Error in `parse_c50_trees()`:
      ! The model records no tree.
      i `C50::C5.0()` writes one only when fitting succeeded.
      i A predictor name or level containing "," or ":" is one cause.

# errors on unsupported configurations

    Code
      tidypredict_fit(fuzzy)
    Condition
      Error in `tidypredict_fit()`:
      ! tidypredict does not support C5.0 models with fuzzy thresholds (`fuzzyThreshold = TRUE`).

---

    Code
      tidypredict_fit(costs)
    Condition
      Error in `tidypredict_fit()`:
      ! tidypredict does not support C5.0 models fitted with a cost matrix (`costs`).

# rule-based models return the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(dplyr::if_else(cyl > 6.00000023841858, 0.9375, 0) + \n    dplyr::if_else(wt <= 2.87500011920929 & cyl > 4.00000023841858, \n        0.8, 0) >= dplyr::if_else(wt > 2.87500011920929 & cyl <= \n    6.00000023841858, 0.875, 0) + dplyr::if_else(cyl <= 4.00000023841858, \n    0.8461538, 0) ~ \"0\", .default = \"1\")"

# boosted rule-based models are not supported

    Code
      tidypredict_fit(model)
    Condition
      Error in `parse_c50_rules()`:
      ! tidypredict does not support boosted rule-based C5.0 models (`rules = TRUE` with `trials > 1`).

