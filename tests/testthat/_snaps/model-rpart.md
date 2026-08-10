# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(case_when(!is.na(cyl) ~ cyl < 5, !is.na(am) ~ !am < \n    0.5, .default = FALSE) ~ 26.6636363636364, .default = case_when(case_when(!is.na(cyl) ~ \n    cyl < 7, !is.na(am) ~ !am < 0.5, .default = FALSE) ~ 19.7428571428571, \n    .default = 15.1))"

# .extract_rpart_classprob errors on non-rpart model

    Code
      .extract_rpart_classprob(list())
    Condition
      Error in `.extract_rpart_classprob()`:
      ! `model` must be <rpart>, not an empty list.

# .extract_rpart_classprob errors on regression model

    Code
      .extract_rpart_classprob(model)
    Condition
      Error in `.extract_rpart_classprob()`:
      ! `model` must be a classification model (method = 'class').

