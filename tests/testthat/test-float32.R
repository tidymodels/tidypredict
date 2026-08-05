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
