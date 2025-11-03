test_that("get_rf_case() avoids ifelse if path is always TRUE (#143)", {
  node <- list(
    path = list(),
    prediction = list(
      list(col = "(Intercept)", val = 37.2, op = "none", is_intercept = 1),
      list(col = "hp", val = -0.0318, op = "multiply", is_intercept = 0),
      list(col = "wt", val = -3.88, op = "multiply", is_intercept = 0)
    )
  )

  expect_identical(
    expr_text(generate_tree_node(node, calc_mode = "ifelse")),
    "37.2 + hp * -0.0318 + wt * -3.88"
  )

  expect_identical(
    expr_text(generate_tree_node(node, calc_mode = "")),
    "37.2 + hp * -0.0318 + wt * -3.88"
  )
})

test_that("generate_tree_node() works", {
  node <- list(
    path = list(
      list(type = "conditional", col = "disp", val = 100, op = "more")
    ),
    prediction = list(
      list(col = "(Intercept)", val = 14, op = "none", is_intercept = 1),
      list(col = "hp", val = 4, op = "multiply", is_intercept = 0),
      list(col = "drat", val = 2, op = "multiply", is_intercept = 0)
    )
  )
  expect_identical(
    generate_tree_node(node, calc_mode = "ifelse"),
    quote(ifelse(disp > 100, 14 + hp * 4 + drat * 2, 0))
  )
  expect_identical(
    generate_tree_node(node, calc_mode = ""),
    quote(disp > 100 ~ 14 + hp * 4 + drat * 2)
  )

  node <- list(
    path = list(),
    prediction = list(
      list(col = "(Intercept)", val = 14, op = "none", is_intercept = 1),
      list(col = "hp", val = 4, op = "multiply", is_intercept = 0),
      list(col = "drat", val = 2, op = "multiply", is_intercept = 0)
    )
  )
  expect_identical(
    generate_tree_node(node, calc_mode = "ifelse"),
    quote(14 + hp * 4 + drat * 2)
  )
  expect_identical(
    generate_tree_node(node, calc_mode = ""),
    quote(14 + hp * 4 + drat * 2)
  )

  node <- list(
    path = list(
      list(type = "conditional", col = "disp", val = 100, op = "more")
    ),
    prediction = 3
  )
  expect_identical(
    generate_tree_node(node, calc_mode = "ifelse"),
    quote(ifelse(disp > 100, 3, 0))
  )
  expect_identical(
    generate_tree_node(node, calc_mode = ""),
    quote(disp > 100 ~ 3)
  )

  node <- list(
    path = list(
      list(type = "conditional", col = "disp", val = 100, op = "more")
    ),
    prediction = list(
      list(col = "(Intercept)", val = 14, op = "none", is_intercept = 1)
    )
  )
  expect_identical(
    generate_tree_node(node, calc_mode = "ifelse"),
    quote(ifelse(disp > 100, 14, 0))
  )
  expect_identical(
    generate_tree_node(node, calc_mode = ""),
    quote(disp > 100 ~ 14)
  )
})

test_that("path_formulas() works", {
  expect_identical(
    path_formulas(
      list()
    ),
    quote(TRUE)
  )

  expect_identical(
    path_formulas(
      list(
        list(type = "all", op = "more", col = "x", val = 0)
      )
    ),
    quote(TRUE)
  )

  expect_identical(
    path_formulas(
      list(
        list(type = "conditional", op = "more", col = "x", val = 0)
      )
    ),
    quote(x > 0)
  )

  expect_identical(
    path_formulas(
      list(
        list(type = "conditional", op = "more", col = "x", val = 0),
        list(type = "conditional", op = "less", col = "y", val = 0)
      )
    ),
    quote(x > 0 & y < 0)
  )

  expect_identical(
    path_formulas(
      list(
        list(type = "conditional", op = "more", col = "x", val = 0),
        list(type = "conditional", op = "less", col = "y", val = 0),
        list(type = "conditional", op = "less", col = "z", val = 0)
      )
    ),
    quote(x > 0 & y < 0 & z < 0)
  )
})

test_that("path_formula() works", {
  expect_identical(
    path_formula(list(
      type = "conditional",
      op = "more",
      col = "x",
      val = 0
    )),
    quote(x > 0)
  )
  expect_identical(
    path_formula(list(
      type = "conditional",
      op = "more-equal",
      col = "x",
      val = 0
    )),
    quote(x >= 0)
  )
  expect_identical(
    path_formula(list(
      type = "conditional",
      op = "less",
      col = "x",
      val = 0
    )),
    quote(x < 0)
  )
  expect_identical(
    path_formula(list(
      type = "conditional",
      op = "less-equal",
      col = "x",
      val = 0
    )),
    quote(x <= 0)
  )
  expect_identical(
    path_formula(list(
      type = "conditional",
      op = "more",
      col = "x",
      val = "h"
    )),
    quote(x > "h")
  )
  expect_identical(
    path_formula(list(
      type = "conditional",
      op = "more-equal",
      col = "x",
      val = "h"
    )),
    quote(x >= "h")
  )
  expect_identical(
    path_formula(list(
      type = "conditional",
      op = "less",
      col = "x",
      val = "h"
    )),
    quote(x < "h")
  )
  expect_identical(
    path_formula(list(
      type = "conditional",
      op = "less-equal",
      col = "x",
      val = "h"
    )),
    quote(x <= "h")
  )
  expect_identical(
    path_formula(list(
      type = "set",
      op = "in",
      col = "x",
      vals = 0
    )),
    quote(x %in% 0)
  )
  expect_identical(
    path_formula(list(
      type = "set",
      op = "not-in",
      col = "x",
      vals = 0
    )),
    quote((x %in% 0) == FALSE)
  )
  expect_identical(
    path_formula(list(
      type = "set",
      op = "in",
      col = "x",
      vals = "h"
    )),
    quote(x %in% "h")
  )
  expect_identical(
    path_formula(list(
      type = "set",
      op = "not-in",
      col = "x",
      vals = "h"
    )),
    quote((x %in% "h") == FALSE)
  )

  res <- path_formula(list(
    type = "set",
    op = "in",
    col = "x",
    vals = c(1, 2, 5)
  ))

  expect_type(res, "language")

  expect_identical(
    expr_text(res),
    "x %in% c(1, 2, 5)"
  )

  res <- path_formula(list(
    type = "set",
    op = "not-in",
    col = "x",
    vals = c(1, 2, 5)
  ))

  expect_type(res, "language")

  expect_identical(
    expr_text(res),
    "(x %in% c(1, 2, 5)) == FALSE"
  )

  res <- path_formula(list(
    type = "set",
    op = "in",
    col = "x",
    vals = letters[1:5]
  ))

  expect_type(res, "language")

  expect_identical(
    expr_text(res),
    'x %in% c("a", "b", "c", "d", "e")'
  )

  res <- path_formula(list(
    type = "set",
    op = "not-in",
    col = "x",
    vals = letters[1:5]
  ))

  expect_type(res, "language")

  expect_identical(
    expr_text(res),
    '(x %in% c("a", "b", "c", "d", "e")) == FALSE'
  )
})

test_that("path_formula() errors with unsupported values", {
  expect_snapshot(
    error = TRUE,
    path_formula(list(type = "unknown", op = "less", col = "x", val = 0))
  )
  expect_snapshot(
    error = TRUE,
    path_formula(list(type = "conditional", op = "unknown", col = "x", val = 0))
  )
  expect_snapshot(
    error = TRUE,
    path_formula(list(type = "set", op = "unknown", col = "x", val = 0))
  )
})
