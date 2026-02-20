# path_formula() errors with unsupported values

    Code
      path_formula(list(type = "unknown", op = "less", col = "x", val = 0))
    Condition
      Error in `path_formula()`:
      ! type has unsupported value of unknown.
      i This is an internal error that was detected in the tidypredict package.
        Please report it at <https://github.com/tidymodels/tidypredict/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

---

    Code
      path_formula(list(type = "conditional", op = "unknown", col = "x", val = 0))
    Condition
      Error in `build_comparison_expr()`:
      ! op has unsupported value of unknown.
      i This is an internal error that was detected in the tidypredict package.
        Please report it at <https://github.com/tidymodels/tidypredict/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

---

    Code
      path_formula(list(type = "set", op = "unknown", col = "x", vals = 0))
    Condition
      Error in `build_set_expr()`:
      ! op has unsupported value of unknown.
      i This is an internal error that was detected in the tidypredict package.
        Please report it at <https://github.com/tidymodels/tidypredict/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

