# tidypredict_sql_interval() validates `interval` (#313)

    Code
      tidypredict_sql_interval(model, dbplyr::simulate_dbi(), interval = 1.5)
    Condition
      Error in `tidypredict_interval()`:
      ! `interval` must be a single number between 0 and 1, not 1.5.

# tidypredict_sql_interval() errors for unsupported models

    Code
      tidypredict_sql_interval(model, dbplyr::simulate_dbi())
    Condition
      Error in `tidypredict_interval()`:
      ! Prediction intervals are not supported for <rpart> models.
      i Only <lm> and <glm> models have prediction intervals.

