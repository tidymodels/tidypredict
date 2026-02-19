test_that("generate_case_when_trees() works", {
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

  tree <- list(node)

  parsedmodel <- list(
    trees = list(tree),
    general = list(mode = "")
  )

  expect_identical(
    generate_case_when_trees(parsedmodel, default = FALSE),
    list(
      quote(case_when(disp > 100 ~ 14 + hp * 4 + drat * 2))
    )
  )

  parsedmodel <- list(
    trees = list(tree),
    general = list(mode = "ifelse")
  )

  expect_identical(
    generate_case_when_trees(parsedmodel, default = FALSE),
    list(
      quote(case_when(ifelse(disp > 100, 14 + hp * 4 + drat * 2, 0)))
    )
  )

  parsedmodel <- list(
    trees = list(tree, tree),
    general = list(mode = "")
  )

  expect_identical(
    generate_case_when_trees(parsedmodel, default = FALSE),
    list(
      quote(
        case_when(
          disp > 100 ~ 14 + hp * 4 + drat * 2
        )
      ),
      quote(
        case_when(
          disp > 100 ~ 14 + hp * 4 + drat * 2
        )
      )
    )
  )

  parsedmodel <- list(
    trees = list(tree, tree),
    general = list(mode = "ifelse")
  )

  expect_identical(
    generate_case_when_trees(parsedmodel, default = FALSE),
    list(
      quote(
        case_when(
          ifelse(disp > 100, 14 + hp * 4 + drat * 2, 0)
        )
      ),
      quote(
        case_when(
          ifelse(disp > 100, 14 + hp * 4 + drat * 2, 0)
        )
      )
    )
  )
})

test_that("generate_case_when_tree() works", {
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

  nodes <- list(node)

  expect_identical(
    generate_case_when_tree(nodes, mode = "", default = FALSE),
    quote(case_when(disp > 100 ~ 14 + hp * 4 + drat * 2))
  )
  expect_identical(
    generate_case_when_tree(nodes, mode = "ifelse", default = FALSE),
    quote(case_when(ifelse(disp > 100, 14 + hp * 4 + drat * 2, 0)))
  )

  nodes <- list(node, node)

  expect_identical(
    generate_case_when_tree(nodes, mode = "", default = FALSE),
    quote(
      case_when(
        disp > 100 ~ 14 + hp * 4 + drat * 2,
        disp > 100 ~ 14 + hp * 4 + drat * 2
      )
    )
  )
  expect_identical(
    generate_case_when_tree(nodes, mode = "ifelse", default = FALSE),
    quote(
      case_when(
        ifelse(disp > 100, 14 + hp * 4 + drat * 2, 0),
        ifelse(disp > 100, 14 + hp * 4 + drat * 2, 0)
      )
    )
  )

  nodes <- list(
    list(
      prediction = 25,
      path = list(list(
        type = "conditional",
        col = "cyl",
        val = 4,
        op = "less-equal"
      ))
    ),
    list(
      prediction = 20,
      path = list(
        list(type = "conditional", col = "cyl", val = 6, op = "less-equal"),
        list(type = "conditional", col = "cyl", val = 4, op = "more")
      )
    ),
    list(
      prediction = 15,
      path = list(
        list(type = "conditional", col = "cyl", val = 6, op = "more"),
        list(type = "conditional", col = "cyl", val = 4, op = "more")
      )
    )
  )

  expect_identical(
    generate_case_when_tree(nodes, mode = "", default = TRUE),
    quote(case_when(cyl <= 4 ~ 25, cyl <= 6 & cyl > 4 ~ 20, .default = 15))
  )
})

test_that("generate_tree_nodes() works", {
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

  nodes <- list(node)

  expect_identical(
    generate_tree_nodes(nodes, mode = ""),
    list(
      quote(disp > 100 ~ 14 + hp * 4 + drat * 2)
    )
  )
  expect_identical(
    generate_tree_nodes(nodes, mode = "ifelse"),
    list(
      quote(ifelse(disp > 100, 14 + hp * 4 + drat * 2, 0))
    )
  )

  nodes <- list(node, node)

  expect_identical(
    generate_tree_nodes(nodes, mode = ""),
    list(
      quote(disp > 100 ~ 14 + hp * 4 + drat * 2),
      quote(disp > 100 ~ 14 + hp * 4 + drat * 2)
    )
  )
  expect_identical(
    generate_tree_nodes(nodes, mode = "ifelse"),
    list(
      quote(ifelse(disp > 100, 14 + hp * 4 + drat * 2, 0)),
      quote(ifelse(disp > 100, 14 + hp * 4 + drat * 2, 0))
    )
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

test_that("generate_tree_node() avoids ifelse if path is always TRUE (#143)", {
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

test_that("generate_tree_node() avoids multipliying with 0 and 1 (#152)", {
  node <- list(
    path = list(
      list(type = "conditional", col = "disp", val = 100, op = "more")
    ),
    prediction = list(
      list(col = "(Intercept)", val = 0, op = "none", is_intercept = 1),
      list(col = "hp", val = 4, op = "multiply", is_intercept = 0),
      list(col = "drat", val = 2, op = "multiply", is_intercept = 0)
    )
  )

  expect_identical(
    generate_tree_node(node, calc_mode = "ifelse"),
    quote(ifelse(disp > 100, hp * 4 + drat * 2, 0))
  )
  expect_identical(
    generate_tree_node(node, calc_mode = ""),
    quote(disp > 100 ~ hp * 4 + drat * 2)
  )

  node <- list(
    path = list(
      list(type = "conditional", col = "disp", val = 100, op = "more")
    ),
    prediction = list(
      list(col = "(Intercept)", val = 14, op = "none", is_intercept = 1),
      list(col = "hp", val = 1, op = "multiply", is_intercept = 0),
      list(col = "drat", val = 0, op = "multiply", is_intercept = 0)
    )
  )

  expect_identical(
    generate_tree_node(node, calc_mode = "ifelse"),
    quote(ifelse(disp > 100, 14 + hp, 0))
  )
  expect_identical(
    generate_tree_node(node, calc_mode = ""),
    quote(disp > 100 ~ 14 + hp)
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
    path_formula(list(type = "set", op = "unknown", col = "x", vals = 0))
  )
})

# Tests for .build_case_when_tree()

test_that(".build_case_when_tree handles stump tree (no conditions)", {
  nodes <- list(list(prediction = 0.5, path = list()))

  expect_identical(.build_case_when_tree(nodes), 0.5)
})

test_that(".build_case_when_tree handles single node with condition", {
  nodes <- list(
    list(
      prediction = 1,
      path = list(list(
        type = "conditional",
        col = "x",
        val = 5,
        op = "less-equal"
      ))
    ),
    list(
      prediction = 0,
      path = list(list(type = "conditional", col = "x", val = 5, op = "more"))
    )
  )

  expect_identical(
    deparse1(.build_case_when_tree(nodes)),
    "case_when(x <= 5 ~ 1, .default = 0)"
  )
})

test_that(".build_case_when_tree handles multiple conditions", {
  nodes <- list(
    list(
      prediction = 1,
      path = list(
        list(type = "conditional", col = "x", val = 5, op = "less-equal"),
        list(type = "conditional", col = "y", val = 10, op = "more")
      )
    ),
    list(
      prediction = 0,
      path = list(list(type = "conditional", col = "x", val = 5, op = "more"))
    )
  )

  expect_identical(
    deparse1(.build_case_when_tree(nodes)),
    "case_when(x <= 5 & y > 10 ~ 1, .default = 0)"
  )
})

test_that(".build_case_when_tree handles three nodes", {
  nodes <- list(
    list(
      prediction = 1,
      path = list(list(
        type = "conditional",
        col = "x",
        val = 3,
        op = "less-equal"
      ))
    ),
    list(
      prediction = 0.5,
      path = list(
        list(type = "conditional", col = "x", val = 3, op = "more"),
        list(type = "conditional", col = "x", val = 7, op = "less-equal")
      )
    ),
    list(
      prediction = 0,
      path = list(list(type = "conditional", col = "x", val = 7, op = "more"))
    )
  )

  expect_identical(
    deparse1(.build_case_when_tree(nodes)),
    "case_when(x <= 3 ~ 1, x > 3 & x <= 7 ~ 0.5, .default = 0)"
  )
})

test_that(".build_case_when_tree handles set conditions", {
  nodes <- list(
    list(
      prediction = 1,
      path = list(list(type = "set", col = "x", vals = c("a", "b"), op = "in"))
    ),
    list(
      prediction = 0,
      path = list(list(
        type = "set",
        col = "x",
        vals = c("a", "b"),
        op = "not-in"
      ))
    )
  )

  expect_identical(
    deparse1(.build_case_when_tree(nodes)),
    'case_when(x %in% c("a", "b") ~ 1, .default = 0)'
  )
})
