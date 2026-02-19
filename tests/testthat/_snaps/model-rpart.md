# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(cyl > 7 & cyl > 5 ~ 15.1, cyl <= 7 & cyl > 5 ~ 19.7428571428571, \n    .default = 26.6636363636364)"

