# comparing no rows is an error, not a vacuous pass (#309)

    Code
      test_results_numeric(numeric(0), numeric(0), threshold = 1e-12)
    Condition
      Error in `test_results_numeric()`:
      ! There is nothing to compare.
      i The data passed to `tidypredict_test()` has no rows.
    Code
      test_results_class(character(0), character(0))
    Condition
      Error in `test_results_class()`:
      ! There is nothing to compare.
      i The data passed to `tidypredict_test()` has no rows.
    Code
      test_results_multiclass(matrix(numeric(0), ncol = 2), matrix(numeric(0), ncol = 2),
      0.1, classes = c("a", "b"))
    Condition
      Error in `test_results_multiclass()`:
      ! There is nothing to compare.
      i The data passed to `tidypredict_test()` has no rows.

