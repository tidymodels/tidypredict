# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(cyl <= 6 ~ case_when(cyl <= 4 ~ \"1\", .default = case_when(wt <= \n    2.875 ~ \"0\", .default = \"1\")), .default = \"0\")"

# errors on unsupported configurations

    Code
      tidypredict_fit(boosted)
    Condition
      Error in `c50_tree_info_full()`:
      ! tidypredict does not support boosted C5.0 models (`trials > 1`).

---

    Code
      tidypredict_fit(rules)
    Condition
      Error in `c50_tree_info_full()`:
      ! tidypredict does not support rule-based C5.0 models (`rules = TRUE`).

