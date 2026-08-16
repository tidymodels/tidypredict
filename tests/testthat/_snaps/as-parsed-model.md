# as_parsed_model() errors without a usable type (#313)

    Code
      as_parsed_model(list())
    Condition
      Error in `as_parsed_model()`:
      ! `x` is not a valid parsed model.
      i `x$general$type` must be a single string, not NULL.

---

    Code
      as_parsed_model(list(general = list()))
    Condition
      Error in `as_parsed_model()`:
      ! `x` is not a valid parsed model.
      i `x$general$type` must be a single string, not NULL.

---

    Code
      as_parsed_model(list(general = list(type = c("a", "b"))))
    Condition
      Error in `as_parsed_model()`:
      ! `x` is not a valid parsed model.
      i `x$general$type` must be a single string, not a character vector.

# as_parsed_model() errors for non-list input (#313)

    Code
      as_parsed_model(NULL)
    Condition
      Error in `as_parsed_model()`:
      ! `x` must be a parsed model, not NULL.

---

    Code
      as_parsed_model("regression")
    Condition
      Error in `as_parsed_model()`:
      ! `x` must be a parsed model, not a string.

