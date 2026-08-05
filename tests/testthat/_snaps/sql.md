# tidypredict_sql_interval() errors for unsupported models

    Code
      tidypredict_sql_interval(model, dbplyr::simulate_dbi())
    Condition
      Error in `tidypredict_interval()`:
      ! Prediction intervals are not supported for <rpart> models.
      i Only <lm> and <glm> models have prediction intervals.

