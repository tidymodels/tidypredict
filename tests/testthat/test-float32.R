test_that("as_f32() rounds to the nearest 32-bit float", {
  expect_equal(as_f32(95.1), 95.09999847412109, tolerance = 1e-15)
  expect_equal(as_f32(0.5), 0.5)
  expect_equal(as_f32(0), 0)
})

test_that("f32_split_boundary() brackets the threshold's own float", {
  # A double that rounds to the threshold must fall on the side the model puts
  # it, which is what the boundary shift buys.
  lower <- f32_split_boundary(as_f32(3.19), "lower")
  expect_lt(lower, as_f32(3.19))
  expect_false(3.19 < lower)

  upper <- f32_split_boundary(as_f32(95.1), "upper")
  expect_gt(upper, as_f32(95.1))
  expect_true(95.1 <= upper)
})

test_that("f32_split_boundary() puts a value on the boundary where the model does", {
  # The midpoint of two floats is itself a double, so a value can sit exactly on
  # the boundary. Rounding it to a float is a tie, broken towards the even
  # mantissa, and the boundary has to agree with however that lands.
  set.seed(1)
  thresholds <- as_f32(c(0.1, 3.19, 95.1, -2.5, 1e-8, 1e8, rnorm(200, sd = 20)))

  for (side in c("lower", "upper")) {
    boundary <- f32_split_boundary(thresholds, side)
    # What the model, comparing as a float, does with a value on the boundary.
    model_takes_left <- if (side == "lower") {
      as_f32(boundary) < thresholds
    } else {
      as_f32(boundary) <= thresholds
    }
    # What the generated expression does with it.
    expr_takes_left <- if (side == "lower") {
      boundary < boundary
    } else {
      boundary <= boundary
    }
    expect_equal(expr_takes_left, model_takes_left, info = side)
  }
})

test_that("f32_split_boundary() separates the floats either side of it", {
  # Every double that rounds to the threshold belongs on the threshold's side,
  # and every double that rounds to the adjacent float on the other.
  set.seed(2)
  thresholds <- as_f32(rnorm(200, sd = 20))

  lower <- f32_split_boundary(thresholds, "lower")
  expect_false(any(thresholds < lower))
  expect_true(all(as_f32(next_double(lower, -1)) < thresholds))

  upper <- f32_split_boundary(thresholds, "upper")
  expect_true(all(thresholds <= upper))
  expect_false(any(as_f32(next_double(upper, 1)) <= thresholds))
})

test_that("f32_split_boundary() handles negative thresholds", {
  x <- as_f32(-3.19)
  expect_lt(f32_split_boundary(x, "lower"), x)
  expect_gt(f32_split_boundary(x, "upper"), x)
})

test_that("f32_split_boundary() handles zero", {
  expect_lt(f32_split_boundary(0, "lower"), 0)
  expect_gt(f32_split_boundary(0, "upper"), 0)
})

test_that("f32_split_boundary() preserves NA and length", {
  x <- c(1.5, NA, -2.5)
  expect_equal(length(f32_split_boundary(x, "lower")), 3)
  expect_true(is.na(f32_split_boundary(x, "lower")[2]))
  expect_equal(f32_split_boundary(NA_real_, "upper"), NA_real_)
})

test_that("f32_split_boundary() rejects an unknown side", {
  expect_error(f32_split_boundary(1, "sideways"))
})
