# parse_model() errors for unsupported objects (#313)

    Code
      parse_model(NULL)
    Condition
      Error in `parse_model()`:
      ! Models of class <NULL> are not supported.

---

    Code
      parse_model(list())
    Condition
      Error in `parse_model()`:
      ! Models of class <list> are not supported.

---

    Code
      parse_model(1:10)
    Condition
      Error in `parse_model()`:
      ! Models of class <integer> are not supported.

# tidypredict_save() errors for unsupported objects (#313)

    Code
      tidypredict_save(NULL, tempfile())
    Condition
      Error in `parse_model()`:
      ! Models of class <NULL> are not supported.

