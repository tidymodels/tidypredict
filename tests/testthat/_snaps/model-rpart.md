# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(case_when(!is.na(cyl) ~ cyl < 5, !is.na(am) ~ !am < \n    0.5, .default = FALSE) ~ 26.6636363636364, .default = case_when(case_when(!is.na(cyl) ~ \n    cyl < 7, !is.na(am) ~ !am < 0.5, .default = FALSE) ~ 19.7428571428571, \n    .default = 15.1))"

# tidypredict_class_exprs errors on non-rpart model

    Code
      tidypredict_class_exprs(list())
    Condition
      Error in `tidypredict_class_exprs()`:
      ! `tidypredict_class_exprs()` is not available for models of class <list>.

# tidypredict_class_exprs errors on regression model

    Code
      tidypredict_class_exprs(model)
    Condition
      Error in `tidypredict_class_exprs()`:
      ! Only classification models are supported, not `method = anova`.

