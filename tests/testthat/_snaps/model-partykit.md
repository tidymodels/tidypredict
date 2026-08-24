# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(is.na(cyl) ~ NA, cyl <= 4 ~ 26.6636363636364, .default = case_when(is.na(cyl) ~ \n    NA, cyl <= 6 ~ 19.7428571428571, .default = 15.1))"

# tidypredict_class_exprs errors on non-party model

    Code
      tidypredict_class_exprs(list())
    Condition
      Error in `tidypredict_class_exprs()`:
      ! `tidypredict_class_exprs()` is not available for models of class <list>.

