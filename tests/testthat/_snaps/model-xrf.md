# returns the right output

    Code
      round_print(tf)
    Output
      [1] "21.43546 + (hp * -0.0048557) + ((ifelse(cyl == \"4\", 1, 0) * 0.7288112) + (ifelse(cyl == \"8\", 1, 0) * -2.529771)) + ((ifelse(wt < 2.32, 1, 0) * 5.003848) + (ifelse(wt < 2.465, 1, 0) * ifelse(wt >= 2.32, 1, 0) * 0.2987375) + ((ifelse(wt >= 2.465, 1, 0) * ifelse(cyl == \"8\", 1, 0) * ifelse(cyl != \"6\", 1, 0) * -7.154443e-5) + ((ifelse(hp >= 123, 1, 0) * ifelse(hp >= 180, 1, 0) * ifelse(wt < 3.78, 1, 0) * -1.226406) + (ifelse(hp >= 123, 1, 0) * ifelse(hp >= 180, 1, 0) * ifelse(wt >= 3.78, 1, 0) * -2.743406)))) + ((ifelse(hp < 123, 1, 0) * ifelse(wt >= 2.32, 1, 0) * ifelse(hp < 97, 1, 0) * 0.4264273) + (ifelse(hp >= 123, 1, 0) * ifelse(wt >= 5.25, 1, 0) * ifelse(hp < 230, 1, 0) * -4.491076) + ((ifelse(hp >= 123, 1, 0) * ifelse(wt >= 5.25, 1, 0) * ifelse(hp >= 230, 1, 0) * 0.08606922) + ((ifelse(hp < 123, 1, 0) * ifelse(hp < 91, 1, 0) * 2.53529) + (ifelse(hp < 123, 1, 0) * ifelse(hp >= 91, 1, 0) * ifelse(wt < 1.615, 1, 0) * 0.8113643))) + ((ifelse(hp >= 123, 1, 0) * ifelse(wt < 5.25, 1, 0) * ifelse(wt < 3.845, 1, 0) * -0.4930652) + (ifelse(wt >= 3.435, 1, 0) * ifelse(hp < 205, 1, 0) * ifelse(hp >= 175, 1, 0) * 0.9051407) + ((ifelse(wt >= 3.435, 1, 0) * ifelse(hp >= 205, 1, 0) * ifelse(hp >= 230, 1, 0) * -0.7948412) + ((ifelse(wt < 3.435, 1, 0) * ifelse(wt < 1.935, 1, 0) * 2.6084) + (ifelse(wt >= 3.435, 1, 0) * ifelse(hp < 205, 1, 0) * ifelse(hp < 175, 1, 0) * -2.085934)))))"

# multinomial models are not supported

    Code
      tidypredict_fit(model)
    Condition
      Error in `parse_model()`:
      ! Multinomial xrf models are not supported.
      i Only `family = "gaussian"` and `family = "binomial"` are supported.

---

    Code
      parse_model(model)
    Condition
      Error in `parse_model()`:
      ! Multinomial xrf models are not supported.
      i Only `family = "gaussian"` and `family = "binomial"` are supported.

# in-line functions in the formula are not supported

    Code
      tidypredict_fit(model)
    Condition
      Error in `acceptable_formula()`:
      x Functions inside the formula are not supported.
      i Functions detected: "log". Use `dplyr` transformations to prepare the data.

# unsupported model terms give an informative error

    Code
      tidypredict_fit(model)
    Condition
      Error in `parse_model()`:
      ! Unable to map the model term "cyl4" to a column.
      i Transformations and interactions in the formula are not supported.

