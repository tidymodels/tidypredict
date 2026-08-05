# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(cyl < 5 ~ 26.6636363636364, .default = case_when(cyl < \n    7 ~ 19.7428571428571, .default = 15.1))"

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

