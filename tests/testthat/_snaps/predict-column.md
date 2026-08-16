# tidypredict_to_column() validates `vars` (#313)

    Code
      tidypredict_to_column(mtcars, model, add_interval = TRUE, vars = "f")
    Condition
      Error in `tidypredict_to_column()`:
      ! `vars` must name at least 3 columns, not 1.
      i The fit, upper and lower bound columns all need a name when `add_interval` is `TRUE`.

---

    Code
      tidypredict_to_column(mtcars, model, vars = 1)
    Condition
      Error in `tidypredict_to_column()`:
      ! `vars` must be a character vector, not a number.

---

    Code
      tidypredict_to_column(mtcars, model, vars = character(0))
    Condition
      Error in `tidypredict_to_column()`:
      ! `vars` must name at least 1 column, not 0.

# tidypredict_to_column() validates `add_interval` and `interval` (#313)

    Code
      tidypredict_to_column(mtcars, model, add_interval = "yes")
    Condition
      Error in `tidypredict_to_column()`:
      ! `add_interval` must be `TRUE` or `FALSE`, not a string.

---

    Code
      tidypredict_to_column(mtcars, model, add_interval = TRUE, interval = 1.5)
    Condition
      Error in `tidypredict_to_column()`:
      ! `interval` must be a single number between 0 and 1, not 1.5.

# tidypredict_to_column() errors for models returning many formulas

    Code
      tidypredict_to_column(iris, model)
    Condition
      Error in `tidypredict_to_column()`:
      ! `tidypredict_to_column()` does not support models that return more than one formula.
      i Use `tidypredict_fit()` directly for these models.

