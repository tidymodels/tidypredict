# every field's contrast is checked (#291)

    Code
      acceptable_formula(model)
    Condition
      Error in `acceptable_lm()`:
      ! The treatment contrast is the only one supported at this time. Field(s) with an invalid contrast are: "gear".

---

    Code
      acceptable_formula(model)
    Condition
      Error in `acceptable_lm()`:
      ! The treatment contrast is the only one supported at this time. Field(s) with an invalid contrast are: "cyl" and "gear".

