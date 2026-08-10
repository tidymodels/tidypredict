# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(cyl <= 6.00000023841858 ~ case_when(cyl <= 4.00000023841858 ~ \n    \"1\", .default = case_when(wt <= 2.87500011920929 ~ \"0\", .default = \"1\")), \n    .default = \"0\")"

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

