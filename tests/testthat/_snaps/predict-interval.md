# tidypredict_interval.list errors for unsupported model

    Code
      tidypredict_interval(pm)
    Condition
      Error in `tidypredict_interval()`:
      ! Prediction intervals are not supported for "unsupported" parsed models.
      i Only <lm> and <glm> models have prediction intervals.

# tidypredict_interval() validates `interval` (#313)

    Code
      tidypredict_interval(model, 1.5)
    Condition
      Error in `tidypredict_interval()`:
      ! `interval` must be a single number between 0 and 1, not 1.5.

---

    Code
      tidypredict_interval(model, 0)
    Condition
      Error in `tidypredict_interval()`:
      ! `interval` must be a single number between 0 and 1, not 0.

---

    Code
      tidypredict_interval(model, "a")
    Condition
      Error in `tidypredict_interval()`:
      ! `interval` must be a single number between 0 and 1, not a string.

---

    Code
      tidypredict_interval(model, c(0.9, 0.95))
    Condition
      Error in `tidypredict_interval()`:
      ! `interval` must be a single number between 0 and 1, not a double vector.

---

    Code
      tidypredict_interval(glm(am ~ wt, mtcars, family = "binomial"), 2)
    Condition
      Error in `tidypredict_interval()`:
      ! `interval` must be a single number between 0 and 1, not 2.

---

    Code
      tidypredict_interval(pm, 1.5)
    Condition
      Error in `tidypredict_interval()`:
      ! `interval` must be a single number between 0 and 1, not 1.5.

# tidypredict_interval.list errors on a malformed parsed model (#313)

    Code
      tidypredict_interval(list())
    Condition
      Error in `tidypredict_interval()`:
      ! `model` must be a fitted model or a parsed model.
      i A parsed model is a list with a general element, as returned by `parse_model()`.

---

    Code
      tidypredict_interval(structure(list(general = list()), class = "list"))
    Condition
      Error in `tidypredict_interval()`:
      ! Prediction intervals are not supported for "unknown" parsed models.
      i Only <lm> and <glm> models have prediction intervals.

# tidypredict_interval() errors for a parsed tree model (#313)

    Code
      tidypredict_interval(pm)
    Condition
      Error in `tidypredict_interval()`:
      ! Prediction intervals are not supported for "rpart" parsed models.
      i Only <lm> and <glm> models have prediction intervals.

