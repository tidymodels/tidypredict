# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(cyl <= 6 ~ case_when(cyl <= 4 ~ \"1\", .default = case_when(wt <= \n    2.875 ~ \"0\", .default = \"1\")), .default = \"0\")"

# errors on unsupported configurations

    Code
      tidypredict_fit(fuzzy)
    Condition
      Error in `c50_check_supported()`:
      ! tidypredict does not support C5.0 models with fuzzy thresholds (`fuzzyThreshold = TRUE`).

---

    Code
      tidypredict_fit(costs)
    Condition
      Error in `c50_check_supported()`:
      ! tidypredict does not support C5.0 models fitted with a cost matrix (`costs`).

# rule-based models return the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(dplyr::if_else(cyl > 6, 0.9375, 0) + dplyr::if_else(wt <= \n    2.875 & cyl > 4, 0.8, 0) >= dplyr::if_else(wt > 2.875 & cyl <= \n    6, 0.875, 0) + dplyr::if_else(cyl <= 4, 0.8461538, 0) ~ \"0\", \n    .default = \"1\")"

# boosted rule-based models are not supported

    Code
      tidypredict_fit(model)
    Condition
      Error in `parse_c50_rules()`:
      ! tidypredict does not support boosted rule-based C5.0 models (`rules = TRUE` with `trials > 1`).

