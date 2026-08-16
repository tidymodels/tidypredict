# tidypredict_fit.pm_tree errors for unsupported v2 models

    Code
      tidypredict_fit(pm)
    Condition
      Error in `tidypredict_fit()`:
      ! Version 2 parsed models of type "made_up" are not supported.
      i This is an internal error that was detected in the tidypredict package.
        Please report it at <https://github.com/tidymodels/tidypredict/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

# tidypredict_fit() errors for a model class it has no parser for

    Code
      tidypredict_fit(structure(list(), class = "made_up_model"))
    Condition
      Error in `tidypredict_fit()`:
      ! Models of class <made_up_model> are not supported.

# tidypredict_fit() errors for a parsed model type with no builder

    Code
      tidypredict_fit(pm)
    Condition
      Error in `tidypredict_fit()`:
      ! Parsed models of type "made_up" are not supported.
      i This is an internal error that was detected in the tidypredict package.
        Please report it at <https://github.com/tidymodels/tidypredict/issues> with a reprex (<https://tidyverse.org/help/>) and the full backtrace.

