# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(cyl <= 4 ~ 26.6636363636364, .default = case_when(cyl <= \n    6 ~ 19.7428571428571, .default = 15.1))"

# .extract_partykit_classprob errors on non-party model

    Code
      .extract_partykit_classprob(list())
    Condition
      Error in `.extract_partykit_classprob()`:
      ! `model` must be <party>, not an empty list.

