# Helper to create test model
#
# Uses mtcars[, -9], every column except `am`, because `am` is the label.
#
# This does not avoid split boundaries: xgboost picks 3.19 as a `wt` threshold
# here, which is exactly an observed value, and comparing it in doubles rather
# than 32-bit floats used to route two rows down the wrong branch. See the
# f32 tests below.
make_xgb_model <- function(
  max_depth = 2L,
  nrounds = 4L,
  objective = "reg:squarederror"
) {
  xgb_data <- xgboost::xgb.DMatrix(
    nthread = 1L,
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )

  xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = max_depth,
      objective = objective,
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = nrounds,
    verbose = 0
  )
}

# Helper to get the standard xgb.DMatrix for testing
make_xgb_data <- function() {
  xgboost::xgb.DMatrix(nthread = 1L, as.matrix(mtcars[, -9]), label = mtcars$am)
}

# Parser tests ---------------------------------------------------------------

test_that("parse_model returns correct structure", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  pm <- parse_model(model)

  expect_s3_class(pm, "parsed_model")
  expect_s3_class(pm, "pm_xgb")

  expect_equal(pm$general$model, "xgb.Booster")
  expect_equal(pm$general$type, "xgb")
  expect_equal(pm$general$version, 3)

  expect_gt(length(pm$trees), 0)
})

test_that("correct number of trees extracted", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model(nrounds = 5L)
  pm <- parse_model(model)

  expect_length(pm$trees, 5)
})

test_that("each tree has leaves with predictions and paths", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  pm <- parse_model(model)

  tree1 <- pm$trees[[1]]
  expect_gt(length(tree1), 0)

  for (leaf in tree1) {
    expect_contains(names(leaf), "prediction")
    expect_contains(names(leaf), "path")
    expect_type(leaf$prediction, "double")
    expect_type(leaf$path, "list")
  }
})

test_that("path conditions have correct structure", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  pm <- parse_model(model)

  tree1 <- pm$trees[[1]]
  leaves_with_paths <- which(vapply(tree1, \(x) length(x$path) > 0, logical(1)))

  if (length(leaves_with_paths) > 0) {
    leaf_with_path <- tree1[[leaves_with_paths[1]]]

    cond <- leaf_with_path$path[[1]]
    expect_equal(cond$type, "conditional")
    expect_contains(names(cond), c("col", "val", "op", "missing"))
    expect_contains(c("less", "more-equal"), cond$op)
  }
})

test_that("feature names are extracted", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  pm <- parse_model(model)

  # Uses mtcars[, -9] which has all columns except 'am'
  expected_names <- colnames(mtcars)[-9]
  expect_equal(pm$general$feature_names, expected_names)
})

test_that("params are extracted", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  pm <- parse_model(model)

  expect_contains(names(pm$general), "params")
  expect_equal(pm$general$params$objective, "reg:squarederror")
})

test_that("niter and nfeatures are extracted", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model(nrounds = 7L)
  pm <- parse_model(model)

  expect_equal(pm$general$niter, 7)
  # Uses mtcars[, -9] which has 10 columns
  expect_equal(pm$general$nfeatures, 10)
})

test_that("base_score is extracted", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  pm <- parse_model(model)

  expect_type(pm$general$params$base_score, "double")
})

test_that("path contains both less and more-equal operators", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  pm <- parse_model(model)

  all_ops <- unlist(lapply(pm$trees[[1]], function(leaf) {
    vapply(leaf$path, \(p) p$op, character(1))
  }))

  expect_contains(all_ops, "more-equal")
  expect_contains(all_ops, "less")
})

test_that("deeper tree paths are traced correctly", {
  skip_if_not_installed("xgboost")

  set.seed(123)
  n <- 100
  X <- matrix(rnorm(n * 3), ncol = 3)
  colnames(X) <- c("a", "b", "c")
  y <- X[, 1] + X[, 2] * 2 + X[, 3] * 3 + rnorm(n, sd = 0.1)

  dtrain <- xgboost::xgb.DMatrix(
    nthread = 1L,
    X,
    label = y,
    feature_names = c("a", "b", "c")
  )
  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 4L,
      objective = "reg:squarederror"
    ),
    data = dtrain,
    nrounds = 1L,
    verbose = 0
  )

  pm <- parse_model(model)
  tree <- pm$trees[[1]]

  path_lengths <- vapply(tree, \(leaf) length(leaf$path), integer(1))
  expect_true(any(path_lengths >= 2))
})

test_that("model without explicit feature names still works", {
  skip_if_not_installed("xgboost")

  set.seed(789)
  X <- data.matrix(mtcars[, c("mpg", "cyl")])
  y <- mtcars$hp

  dtrain <- xgboost::xgb.DMatrix(nthread = 1L, X, label = y)

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 2L,
      objective = "reg:squarederror"
    ),
    data = dtrain,
    nrounds = 3L,
    verbose = 0
  )

  pm <- parse_model(model)

  expect_s3_class(pm, "pm_xgb")
  expect_length(pm$trees, 3)
  expect_equal(pm$general$nfeatures, 2)
})

# Fit formula tests ----------------------------------------------------------

test_that("tidypredict_fit returns language object", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()

  fit_formula <- tidypredict_fit(model)

  expect_type(fit_formula, "language")
})

test_that("tidypredict_fit works on parsed model", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  pm <- parse_model(model)

  fit_formula <- tidypredict_fit(pm)

  expect_type(fit_formula, "language")
})

test_that("produced case_when uses .default", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()

  fit <- tidypredict_fit(model)
  fit_text <- rlang::expr_text(fit)

  expect_match(fit_text, "\\.default")
})

test_that("reg:squarederror predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model(objective = "reg:squarederror")

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("a saved and reloaded booster still predicts correctly (#292)", {
  skip_if_not_installed("xgboost")

  # `xgb.load()` sets neither `attr(model, "param")` nor `model$params`, so a
  # reloaded booster used to take the pre-2.0 code path and fail outright, and
  # once past that had no objective to apply, returning the raw margin as
  # though it were a probability.
  X <- as.matrix(mtcars[, c("wt", "disp", "hp")])

  for (objective in c("reg:squarederror", "binary:logistic", "count:poisson")) {
    label <- switch(
      objective,
      "binary:logistic" = mtcars$am,
      "count:poisson" = mtcars$carb,
      mtcars$mpg
    )

    set.seed(1)
    model <- xgboost::xgb.train(
      params = list(max_depth = 3L, objective = objective),
      data = xgboost::xgb.DMatrix(X, label = label),
      nrounds = 5L,
      verbose = 0
    )

    path <- withr::local_tempfile(fileext = ".ubj")
    xgboost::xgb.save(model, path)
    reloaded <- xgboost::xgb.load(path)

    # The objective has to survive the round trip, or a logit is returned as a
    # probability without anything to signal it.
    expect_equal(
      parse_model(reloaded)$general$params$objective,
      objective
    )

    expect_equal(
      rlang::eval_tidy(tidypredict_fit(reloaded), mtcars),
      rlang::eval_tidy(tidypredict_fit(model), mtcars),
      info = objective
    )
    expect_equal(
      rlang::eval_tidy(tidypredict_fit(reloaded), mtcars),
      as.numeric(predict(reloaded, X)),
      tolerance = 1e-5,
      info = objective
    )
    expect_equal(
      rlang::eval_tidy(tidypredict_fit(parse_model(reloaded)), mtcars),
      as.numeric(predict(reloaded, X)),
      tolerance = 1e-5,
      info = objective
    )
  }
})

test_that("values sitting on a split boundary match native predict", {
  skip_if_not_installed("xgboost")

  # Every threshold in the generated formula, probed from both sides and from
  # exactly on it. The last of those is the one worth having: the boundary is
  # the midpoint between two floats, so a value can land precisely on it, and
  # which side it belongs to is decided by how xgboost rounds the tie.
  #
  # Not `mtcars`: none of the thresholds xgboost picks there resolve the tie
  # towards the neighbouring float, so the model agrees either way and the probe
  # proves nothing.
  set.seed(42)
  n <- 200
  df <- data.frame(
    x1 = round(rnorm(n), 3),
    x2 = round(runif(n, 0, 10), 3),
    x3 = round(rnorm(n, 5, 2), 3),
    x4 = round(runif(n, -3, 3), 3)
  )
  df$y <- 2 * df$x1 - 0.5 * df$x2 + sin(df$x3) + rnorm(n, sd = 0.3)
  cols <- c("x1", "x2", "x3", "x4")

  model <- xgboost::xgb.train(
    params = list(
      max_depth = 3L,
      objective = "reg:squarederror",
      base_score = 0.5
    ),
    data = xgboost::xgb.DMatrix(as.matrix(df[, cols]), label = df$y),
    nrounds = 5L,
    verbose = 0
  )
  fit <- tidypredict_fit(model)

  thresholds <- list()
  collect <- function(e) {
    if (!is.call(e)) {
      return()
    }
    if (identical(as.character(e[[1]])[1], "<") && is.numeric(e[[3]])) {
      thresholds[[length(thresholds) + 1L]] <<- list(
        col = as.character(e[[2]]),
        val = e[[3]]
      )
    }
    for (i in seq_along(e)) {
      collect(e[[i]])
    }
  }
  collect(fit)
  expect_gt(length(thresholds), 0)

  probes <- lapply(thresholds, function(th) {
    row <- df[rep(1, 3), cols, drop = FALSE]
    for (col in cols) {
      row[[col]] <- median(df[[col]])
    }
    row[[th$col]] <- c(
      th$val,
      next_double(th$val, -1),
      next_double(th$val, 1)
    )
    row
  })
  probe <- do.call(rbind, probes)

  expect_equal(
    rlang::eval_tidy(fit, probe),
    as.numeric(predict(model, as.matrix(probe[, cols]))),
    tolerance = 1e-6
  )
})

test_that("binary:logistic predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model(objective = "binary:logistic")

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("reg:logistic predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model(objective = "reg:logistic")

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("binary:logitraw predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model(objective = "binary:logitraw")

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("count:poisson predictions match native predict", {
  skip_if_not_installed("xgboost")

  # Add 0.1 to avoid exact split boundaries (float32 vs float64 precision)
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- mtcars_adj[, -9] + 0.1

  xgb_data <- xgboost::xgb.DMatrix(
    nthread = 1L,
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$carb
  )

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 2L,
      objective = "count:poisson",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(
    model,
    mtcars_adj,
    xg_df = xgb_data,
    threshold = 1e-6
  )

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("reg:tweedie predictions match native predict", {
  skip_if_not_installed("xgboost")

  # Add 0.1 to avoid exact split boundaries (float32 vs float64 precision)
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- mtcars_adj[, -9] + 0.1

  xgb_data <- xgboost::xgb.DMatrix(
    nthread = 1L,
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$hp
  )

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 2L,
      objective = "reg:tweedie",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(
    model,
    mtcars_adj,
    xg_df = xgb_data,
    threshold = 1e-6
  )

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("reg:squaredlogerror predictions match native predict", {
  skip_if_not_installed("xgboost")

  # Add 0.1 to avoid exact split boundaries (float32 vs float64 precision)
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- mtcars_adj[, -9] + 0.1

  xgb_data <- xgboost::xgb.DMatrix(
    nthread = 1L,
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$hp
  )

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 2L,
      objective = "reg:squaredlogerror",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(
    model,
    mtcars_adj,
    xg_df = xgb_data,
    threshold = 1e-6
  )

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("reg:gamma predictions match native predict", {
  skip_if_not_installed("xgboost")

  # Add 0.1 to avoid exact split boundaries (float32 vs float64 precision)
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- mtcars_adj[, -9] + 0.1

  xgb_data <- xgboost::xgb.DMatrix(
    nthread = 1L,
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$hp
  )

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 2L,
      objective = "reg:gamma",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(
    model,
    mtcars_adj,
    xg_df = xgb_data,
    threshold = 1e-6
  )

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("reg:pseudohubererror predictions match native predict", {
  skip_if_not_installed("xgboost")

  # Add 0.1 to avoid exact split boundaries (float32 vs float64 precision)
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- mtcars_adj[, -9] + 0.1

  xgb_data <- xgboost::xgb.DMatrix(
    nthread = 1L,
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$hp
  )

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 2L,
      objective = "reg:pseudohubererror",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(
    model,
    mtcars_adj,
    xg_df = xgb_data,
    threshold = 1e-6
  )

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("reg:absoluteerror predictions match native predict", {
  skip_if_not_installed("xgboost")

  # Snap to a 1/64 grid so every value is exactly representable in float32.
  # xgboost stores split thresholds as float32, so a value that is not
  # float32-exact can land on the other side of a threshold here than it does
  # in predict(). Which splits are chosen varies by xgboost version, so an
  # arbitrary offset is not enough to stay clear of the boundaries.
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- round(mtcars_adj[, -9] * 64) / 64

  xgb_data <- xgboost::xgb.DMatrix(
    nthread = 1L,
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$hp
  )

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 2L,
      objective = "reg:absoluteerror",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(
    model,
    mtcars_adj,
    xg_df = xgb_data,
    threshold = 1e-5
  )

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("binary:hinge predictions match native predict", {
  skip_if_not_installed("xgboost")

  # Add 0.1 to avoid exact split boundaries (float32 vs float64 precision)
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- mtcars_adj[, -9] + 0.1

  xgb_data <- xgboost::xgb.DMatrix(
    nthread = 1L,
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$am
  )

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 2L,
      objective = "binary:hinge",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(
    model,
    mtcars_adj,
    xg_df = xgb_data,
    threshold = 1e-7
  )

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("DART booster with rate_drop = 0 predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 2L,
      objective = "reg:squarederror",
      base_score = 0.5,
      booster = "dart",
      rate_drop = 0
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("DART booster with rate_drop > 0 predictions match native predict", {
  skip_if_not_installed("xgboost")

  # Snap to a 1/64 grid so every value is exactly representable in float32; see
  # the reg:absoluteerror test above.
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- round(mtcars_adj[, -9] * 64) / 64

  xgb_data <- xgboost::xgb.DMatrix(
    nthread = 1L,
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$am
  )

  # `one_drop` forces a tree to be dropped every round, so the dropout weights
  # are actually exercised. `rate_drop` alone leaves that to the RNG.
  model <- suppressWarnings(xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 2L,
      objective = "reg:squarederror",
      base_score = 0.5,
      booster = "dart",
      rate_drop = 0.3,
      one_drop = 1,
      seed = 123
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  ))

  result <- tidypredict_test(
    model,
    mtcars_adj,
    xg_df = xgb_data,
    threshold = 1e-6
  )

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("DART booster weight_drop is extracted correctly", {
  skip_if_not_installed("xgboost")

  xgb_data <- xgboost::xgb.DMatrix(
    nthread = 1L,
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )

  # `one_drop` forces a tree to be dropped every round. `rate_drop` alone is
  # not enough: whether any tree is actually dropped depends on the RNG, and
  # xgboost 3.4 drops nothing here, which would leave `weight_drop` all ones.
  model <- suppressWarnings(xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 2L,
      objective = "reg:squarederror",
      base_score = 0.5,
      booster = "dart",
      rate_drop = 0.3,
      one_drop = 1,
      seed = 123
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  ))

  pm <- parse_model(model)

  expect_equal(pm$general$booster_name, "dart")
  expect_type(pm$general$weight_drop, "double")
  expect_length(pm$general$weight_drop, 4)
  # At least one weight should be different from 1 when trees are dropped
  expect_false(all(pm$general$weight_drop == 1))
})

test_that("gbtree booster has no weight_drop", {
  skip_if_not_installed("xgboost")

  xgb_data <- xgboost::xgb.DMatrix(
    nthread = 1L,
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 2L,
      objective = "reg:squarederror",
      base_score = 0.5,
      booster = "gbtree"
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  pm <- parse_model(model)

  expect_equal(pm$general$booster_name, "gbtree")
  expect_null(pm$general$weight_drop)
})

test_that("dropout weights are found when the booster is saved as gbtree", {
  # xgboost >= 3.4 canonicalises `booster = "dart"` to `"gbtree"`
  txt <- paste0(
    '{"learner":{"gradient_booster":{"name":"gbtree",',
    '"weight_drop":[1E0,7E-1,5E-1],"model":{}},',
    '"learner_model_param":{"base_score":"[5E-1]"}}}'
  )

  params <- parse_xgb_json_params(txt)

  expect_equal(params$booster_name, "dart")
  expect_equal(params$weight_drop, c(1, 0.7, 0.5))
})

test_that("all-ones dropout weights are not treated as dropout", {
  txt <- paste0(
    '{"learner":{"gradient_booster":{"name":"gbtree",',
    '"weight_drop":[1E0,1E0],"model":{}},',
    '"learner_model_param":{"base_score":"[5E-1]"}}}'
  )

  params <- parse_xgb_json_params(txt)

  expect_equal(params$booster_name, "gbtree")
  expect_null(params$weight_drop)
})

test_that("model with custom base_score works correctly", {
  skip_if_not_installed("xgboost")

  xgb_data <- xgboost::xgb.DMatrix(
    nthread = 1L,
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 2L,
      objective = "reg:logistic",
      base_score = mean(mtcars$am)
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("base_score of 0 is not included in formula", {
  skip_if_not_installed("xgboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$am
  dtrain <- xgboost::xgb.DMatrix(
    nthread = 1L,
    X,
    label = y,
    feature_names = c("mpg", "cyl", "disp")
  )

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 1L,
      objective = "reg:squarederror",
      base_score = 0
    ),
    data = dtrain,
    nrounds = 1L,
    verbose = 0
  )

  res <- tidypredict_fit(model)
  res_text <- rlang::expr_text(res)
  expect_false(grepl("\\+ 0$", res_text))
})

test_that("base_score of 0.5 is included in formula", {
  skip_if_not_installed("xgboost")

  set.seed(123)
  X <- data.matrix(mtcars[, c("mpg", "cyl", "disp")])
  y <- mtcars$am
  dtrain <- xgboost::xgb.DMatrix(
    nthread = 1L,
    X,
    label = y,
    feature_names = c("mpg", "cyl", "disp")
  )

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 1L,
      objective = "reg:squarederror",
      base_score = 0.5
    ),
    data = dtrain,
    nrounds = 1L,
    verbose = 0
  )

  res <- tidypredict_fit(model)
  res_text <- rlang::expr_text(res)
  expect_match(res_text, "\\+ \\s*0\\.5")
})

test_that("predictions with missing values work", {
  skip_if_not_installed("xgboost")

  set.seed(456)
  X <- as.matrix(mtcars[, -9])
  y <- mtcars$am
  X_train <- X
  X_train[1:3, 1] <- NA
  dtrain <- xgboost::xgb.DMatrix(nthread = 1L, X_train, label = y)

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 2L,
      objective = "reg:squarederror"
    ),
    data = dtrain,
    nrounds = 3L,
    verbose = 0
  )

  X_pred <- X
  X_pred[5:7, 1] <- NA
  X_pred[10:12, 2] <- NA

  fit_formula <- tidypredict_fit(model)
  dpred <- xgboost::xgb.DMatrix(nthread = 1L, X_pred)
  native_preds <- predict(model, dpred)

  pred_df <- as.data.frame(X_pred)
  tidy_preds <- rlang::eval_tidy(fit_formula, pred_df)

  # Check formula runs without error on data with NA values
  expect_type(tidy_preds, "double")
  expect_length(tidy_preds, nrow(mtcars))
})

test_that("unsupported objective throws error", {
  skip_if_not_installed("xgboost")

  pm <- list(
    general = list(
      params = list(objective = "unsupported_objective"),
      model = "xgb.Booster",
      type = "xgb"
    ),
    trees = list(list(list(prediction = 1, path = list())))
  )
  class(pm) <- c("pm_xgb", "parsed_model", "list")

  expect_snapshot(tidypredict_fit(pm), error = TRUE)
})

test_that("stump trees (no splits) predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- xgboost::xgb.DMatrix(
    nthread = 1L,
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 2L,
      gamma = 100,
      objective = "reg:squarederror",
      base_score = 0.5
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  # Verify model contains stump trees (single leaf, no splits)
  pm <- parse_model(model)
  leaves_per_tree <- vapply(pm$trees, length, integer(1), USE.NAMES = FALSE)
  path_lengths <- vapply(
    pm$trees,
    \(tree) length(tree[[1]]$path),
    integer(1),
    USE.NAMES = FALSE
  )
  expect_all_equal(leaves_per_tree, 1L)
  expect_all_equal(path_lengths, 0L)

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("NULL objective warns user", {
  skip_if_not_installed("xgboost")

  pm <- list(
    general = list(
      params = list(base_score = 0),
      model = "xgb.Booster",
      type = "xgb"
    ),
    trees = list(list(list(prediction = 5.0, path = list())))
  )
  class(pm) <- c("pm_xgb", "parsed_model", "list")

  expect_snapshot(tidypredict_fit(pm))
})

test_that("stump tree (empty path) works", {
  skip_if_not_installed("xgboost")

  pm <- list(
    general = list(
      params = list(objective = "reg:squarederror", base_score = 0),
      model = "xgb.Booster",
      type = "xgb"
    ),
    trees = list(list(list(prediction = 42.5, path = list())))
  )
  class(pm) <- c("pm_xgb", "parsed_model", "list")

  result <- tidypredict_fit(pm)
  value <- rlang::eval_tidy(result, data.frame(x = 1))

  expect_equal(value, 42.5)
})

test_that("large model predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model(
    max_depth = 2L,
    nrounds = 50L,
    objective = "reg:logistic"
  )

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("deep tree predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model(
    max_depth = 20L,
    nrounds = 4L,
    objective = "binary:logistic"
  )

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

# SQL generation tests -------------------------------------------------------

test_that("tidypredict_sql returns SQL class", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("dbplyr")
  model <- make_xgb_model()

  sql_result <- tidypredict_sql(model, dbplyr::simulate_dbi())

  expect_s3_class(sql_result, "sql")
})

test_that("tidypredict_sql works with parsed model", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("dbplyr")
  model <- make_xgb_model()
  pm <- parse_model(model)

  sql_result <- tidypredict_sql(pm, dbplyr::simulate_dbi())

  expect_s3_class(sql_result, "sql")
})

test_that("SQL predictions can be generated with SQLite", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")

  model <- make_xgb_model()

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Use all columns except 'am' (column 9)
  test_data <- mtcars[, -9]
  DBI::dbWriteTable(con, "test_data", test_data)

  sql_query <- tidypredict_sql(model, con)

  # SQL query can be executed without error
  db_result <- DBI::dbGetQuery(
    con,
    paste0("SELECT ", sql_query, " AS pred FROM test_data")
  )

  expect_equal(nrow(db_result), nrow(mtcars))
  expect_type(db_result$pred, "double")
})

test_that("SQL predictions work for binary classification with SQLite", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")

  model <- make_xgb_model(objective = "binary:logistic")

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Use all columns except 'am' (column 9)
  test_data <- mtcars[, -9]
  DBI::dbWriteTable(con, "test_data", test_data)

  sql_query <- tidypredict_sql(model, con)

  # SQL query can be executed without error
  db_result <- DBI::dbGetQuery(
    con,
    paste0("SELECT ", sql_query, " AS pred FROM test_data")
  )

  expect_equal(nrow(db_result), nrow(mtcars))
  expect_type(db_result$pred, "double")
  # Binary logistic predictions should be between 0 and 1
  expect_true(all(db_result$pred >= 0 & db_result$pred <= 1))
})

# Integration tests ----------------------------------------------------------

test_that("tidypredict_test works for regression", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()
  xgb_data <- make_xgb_data()

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("tidypredict_test works for binary classification", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model(objective = "binary:logistic")

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_s3_class(result, "tidypredict_test")
  expect_false(result$alert)
})

test_that("tidypredict_test xg_df argument is required", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model()

  # Without xg_df, tidypredict_test should fail
  expect_snapshot(tidypredict_test(model, mtcars), error = TRUE)
})

test_that("tidypredict_test respects max_rows parameter", {
  skip_if_not_installed("xgboost")

  model <- make_xgb_model()
  xgb_data <- make_xgb_data()

  # Create a subset DMatrix for max_rows = 10
  X <- as.matrix(mtcars[1:10, -9])
  xgb_subset <- xgboost::xgb.DMatrix(nthread = 1L, X)

  result <- tidypredict_test(
    model,
    mtcars[1:10, ],
    xg_df = xgb_subset,
    max_rows = 10
  )

  expect_equal(nrow(result$raw_results), 10)
})

test_that("tidypredict_trees returns an unnamed list of expressions", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model(nrounds = 4L)

  trees <- tidypredict_trees(model)

  expect_type(trees, "list")
  expect_length(trees, 4)
  expect_null(names(trees))
  for (tree in trees) {
    expect_type(tree, "language")
  }
})

test_that("tidypredict_n_trees counts every tree", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model(nrounds = 4L)

  expect_identical(tidypredict_n_trees(model), 4L)
  expect_identical(
    tidypredict_n_trees(model),
    length(tidypredict_trees(model))
  )
})

test_that("tidypredict_trees combined results match tidypredict_fit", {
  skip_if_not_installed("xgboost")
  model <- make_xgb_model(nrounds = 4L, objective = "reg:squarederror")

  trees <- tidypredict_trees(model)
  eval_env <- rlang::new_environment(
    data = as.list(mtcars),
    parent = asNamespace("dplyr")
  )
  tree_preds <- lapply(trees, rlang::eval_tidy, env = eval_env)
  pm <- parse_model(model)
  base_score <- pm$general$params$base_score
  combined <- Reduce(`+`, tree_preds) + base_score

  fit_result <- rlang::eval_tidy(tidypredict_fit(model), mtcars)

  expect_equal(combined, fit_result)
})

test_that("tidypredict_trees errors on non-xgb.Booster", {
  expect_snapshot(tidypredict_trees(list()), error = TRUE)
})

test_that("tidypredict_trees combined results match tidypredict_fit for DART", {
  skip_if_not_installed("xgboost")

  # Add 0.1 to avoid exact split boundaries (float32 vs float64 precision)
  mtcars_adj <- mtcars
  mtcars_adj[, -9] <- mtcars_adj[, -9] + 0.1

  xgb_data <- xgboost::xgb.DMatrix(
    nthread = 1L,
    as.matrix(mtcars_adj[, -9]),
    label = mtcars_adj$am
  )

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      max_depth = 2L,
      objective = "reg:squarederror",
      base_score = 0.5,
      booster = "dart",
      rate_drop = 0.3,
      seed = 123
    ),
    data = xgb_data,
    nrounds = 4L,
    verbose = 0
  )

  trees <- tidypredict_trees(model)
  eval_env <- rlang::new_environment(
    data = as.list(mtcars_adj),
    parent = asNamespace("dplyr")
  )
  tree_preds <- lapply(trees, rlang::eval_tidy, env = eval_env)
  pm <- parse_model(model)
  base_score <- pm$general$params$base_score
  combined <- Reduce(`+`, tree_preds) + base_score

  fit_result <- rlang::eval_tidy(tidypredict_fit(model), mtcars_adj)

  expect_equal(combined, fit_result)
})

# v1 backwards compatibility tests -------------------------------------------

test_that("gblinear booster is detected by get_xgb_json_params", {
  skip_if_not_installed("xgboost")

  xgb_data <- xgboost::xgb.DMatrix(
    nthread = 1L,
    as.matrix(mtcars[, -9]),
    label = mtcars$am
  )

  model <- xgboost::xgb.train(
    params = list(
      nthread = 1,
      booster = "gblinear",
      objective = "reg:squarederror"
    ),
    data = xgb_data,
    nrounds = 3L,
    verbose = 0
  )

  # get_xgb_json_params works even though parse_model/tidypredict_fit fail
  params <- tidypredict:::get_xgb_json_params(model)

  expect_equal(params$booster_name, "gblinear")
})

test_that("v1 parsed xgboost model produces correct predictions", {
  skip_if_not_installed("xgboost")

  pm <- readRDS(test_path("backwards-compat", "xgb-v2-regression.rds"))

  expect_equal(pm$general$version %||% 1, 1)
  expect_true(!is.null(pm$trees))

  fit <- tidypredict_fit(pm)
  expect_type(fit, "language")

  # Verify predictions can be generated
  pred <- rlang::eval_tidy(fit, mtcars)
  expect_type(pred, "double")
  expect_length(pred, nrow(mtcars))
})

test_that("NULL base_score in v1 parsed model defaults to 0.5", {
  skip_if_not_installed("xgboost")

  # v1 format goes through build_fit_formula_xgb (legacy flat case_when)
  pm <- list(
    general = list(
      params = list(objective = "reg:squarederror"),
      model = "xgb.Booster",
      type = "xgb"
    ),
    trees = list(list(list(prediction = 1.0, path = list())))
  )
  class(pm) <- c("pm_xgb", "parsed_model", "list")

  fit <- tidypredict_fit(pm)
  result <- rlang::eval_tidy(fit, data.frame(x = 1))

  # 1.0 + 0.5 base_score = 1.5
  expect_equal(result, 1.5)
})

test_that("NULL objective with non-zero base_score warns user", {
  skip_if_not_installed("xgboost")

  pm <- list(
    general = list(
      params = list(base_score = 0.3),
      model = "xgb.Booster",
      type = "xgb"
    ),
    trees = list(list(list(prediction = 1.0, path = list())))
  )
  class(pm) <- c("pm_xgb", "parsed_model", "list")

  expect_snapshot(fit <- tidypredict_fit(pm))
  result <- rlang::eval_tidy(fit, data.frame(x = 1))

  expect_equal(result, 1.3)
})

test_that("v1 parsed model with splits produces predictions", {
  skip_if_not_installed("xgboost")

  # v1 format with actual path conditions - tests get_xgb_case_fun
  pm <- list(
    general = list(
      params = list(objective = "reg:squarederror", base_score = 0),
      model = "xgb.Booster",
      type = "xgb"
    ),
    trees = list(list(
      list(
        prediction = 10.0,
        path = list(
          list(
            type = "conditional",
            col = "mpg",
            val = 20,
            op = "more-equal",
            missing = FALSE
          )
        )
      ),
      list(
        prediction = 30.0,
        path = list(
          list(
            type = "conditional",
            col = "mpg",
            val = 20,
            op = "less",
            missing = FALSE
          )
        )
      )
    ))
  )
  class(pm) <- c("pm_xgb", "parsed_model", "list")

  fit <- tidypredict_fit(pm)
  expect_type(fit, "language")

  # Test predictions - more-equal generates <, less generates >=
  test_data <- data.frame(mpg = c(15, 25))
  pred <- rlang::eval_tidy(fit, test_data)

  expect_equal(pred, c(10.0, 30.0))
})

test_that("v1 parsed model with missing=TRUE uses is.na", {
  skip_if_not_installed("xgboost")

  pm <- list(
    general = list(
      params = list(objective = "reg:squarederror", base_score = 0),
      model = "xgb.Booster",
      type = "xgb"
    ),
    trees = list(list(
      list(
        prediction = 10.0,
        path = list(
          list(
            type = "conditional",
            col = "mpg",
            val = 20,
            op = "more-equal",
            missing = TRUE
          )
        )
      ),
      list(
        prediction = 30.0,
        path = list(
          list(
            type = "conditional",
            col = "mpg",
            val = 20,
            op = "less",
            missing = FALSE
          )
        )
      )
    ))
  )
  class(pm) <- c("pm_xgb", "parsed_model", "list")

  fit <- tidypredict_fit(pm)

  # With missing=TRUE on more-equal, NA should match that condition
  test_data <- data.frame(mpg = c(15, NA, 25))
  pred <- rlang::eval_tidy(fit, test_data)

  expect_equal(pred, c(10.0, 10.0, 30.0))
})

test_that("v1 parsed model with missing=TRUE on less op", {
  skip_if_not_installed("xgboost")

  pm <- list(
    general = list(
      params = list(objective = "reg:squarederror", base_score = 0),
      model = "xgb.Booster",
      type = "xgb"
    ),
    trees = list(list(
      list(
        prediction = 10.0,
        path = list(
          list(
            type = "conditional",
            col = "mpg",
            val = 20,
            op = "more-equal",
            missing = FALSE
          )
        )
      ),
      list(
        prediction = 30.0,
        path = list(
          list(
            type = "conditional",
            col = "mpg",
            val = 20,
            op = "less",
            missing = TRUE
          )
        )
      )
    ))
  )
  class(pm) <- c("pm_xgb", "parsed_model", "list")

  fit <- tidypredict_fit(pm)

  # With missing=TRUE on less, NA should match that condition (>= with is.na)
  test_data <- data.frame(mpg = c(15, NA, 25))
  pred <- rlang::eval_tidy(fit, test_data)

  expect_equal(pred, c(10.0, 30.0, 30.0))
})

# YAML serialization tests ---------------------------------------------------

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("yaml")

  model <- make_xgb_model()
  pm <- parse_model(model)

  tmp_file <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, tmp_file)
  loaded <- yaml::read_yaml(tmp_file)
  pm_loaded <- as_parsed_model(loaded)

  expect_equal(pm_loaded$general$model, pm$general$model)
  expect_equal(pm_loaded$general$type, pm$general$type)
  expect_equal(pm_loaded$general$niter, pm$general$niter)
})

test_that("loaded model produces same predictions", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("yaml")

  model <- make_xgb_model()
  pm <- parse_model(model)

  tmp_file <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, tmp_file)
  loaded <- yaml::read_yaml(tmp_file)
  pm_loaded <- as_parsed_model(loaded)

  original_preds <- rlang::eval_tidy(tidypredict_fit(pm), mtcars)
  loaded_preds <- rlang::eval_tidy(tidypredict_fit(pm_loaded), mtcars)

  expect_equal(loaded_preds, original_preds, tolerance = 1e-5)
})

test_that("parsed model produces same predictions as the fitted model", {
  skip_if_not_installed("xgboost")

  model <- make_xgb_model()
  pm <- as_parsed_model(parse_model(model))

  direct <- rlang::eval_tidy(tidypredict_fit(model), mtcars)
  parsed <- rlang::eval_tidy(tidypredict_fit(pm), mtcars)

  expect_equal(parsed, direct)
})

test_that("parsed model predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model()
  pm <- as_parsed_model(parse_model(model))

  parsed <- rlang::eval_tidy(tidypredict_fit(pm), mtcars)

  expect_equal(parsed, predict(model, xgb_data), tolerance = 1e-6)
})

test_that("tidypredict_fit predictions match native predict", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model()

  direct <- rlang::eval_tidy(tidypredict_fit(model), mtcars)

  expect_equal(direct, predict(model, xgb_data), tolerance = 1e-6)
})

test_that("tidypredict_test flags differences in both directions", {
  skip_if_not_installed("xgboost")

  xgb_data <- make_xgb_data()
  model <- make_xgb_model()

  result <- tidypredict_test(model, mtcars, xg_df = xgb_data, threshold = 1e-7)

  expect_threshold_consistent(result, 1e-7)
})

# Parsnip integration tests --------------------------------------------------

test_that("tidypredict works with parsnip xgboost regression", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("parsnip")

  set.seed(123)
  # Use all columns except am (column 9) for consistency
  train_data <- mtcars

  model_spec <- parsnip::boost_tree(
    trees = 4,
    tree_depth = 2,
    min_n = 1
  ) |>
    parsnip::set_engine("xgboost") |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    am ~ . - hp,
    data = train_data
  )

  xgb_model <- model_fit$fit

  expect_s3_class(xgb_model, "xgb.Booster")

  pm <- parse_model(xgb_model)
  expect_s3_class(pm, "parsed_model")
  expect_s3_class(pm, "pm_xgb")
  expect_gt(length(pm$trees), 0)

  fit_formula <- tidypredict_fit(xgb_model)
  expect_type(fit_formula, "language")
})

test_that("tidypredict works with parsnip xgboost classification", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("parsnip")

  set.seed(456)
  train_data <- mtcars
  train_data$am <- factor(train_data$am)

  model_spec <- parsnip::boost_tree(
    trees = 4,
    tree_depth = 2,
    min_n = 1
  ) |>
    parsnip::set_engine("xgboost") |>
    parsnip::set_mode("classification")

  model_fit <- parsnip::fit(
    model_spec,
    am ~ . - hp,
    data = train_data
  )

  xgb_model <- model_fit$fit

  expect_s3_class(xgb_model, "xgb.Booster")

  fit_formula <- tidypredict_fit(xgb_model)
  expect_type(fit_formula, "language")
})

test_that("tidypredict_sql works with parsnip xgboost model", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("parsnip")
  skip_if_not_installed("dbplyr")

  set.seed(123)
  train_data <- mtcars

  model_spec <- parsnip::boost_tree(
    trees = 3,
    tree_depth = 2,
    min_n = 1
  ) |>
    parsnip::set_engine("xgboost") |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    am ~ . - hp,
    data = train_data
  )
  xgb_model <- model_fit$fit

  sql_result <- tidypredict_sql(xgb_model, dbplyr::simulate_dbi())

  expect_s3_class(sql_result, "sql")
})

test_that("tidypredict_test works with parsnip xgboost model", {
  skip_if_not_installed("xgboost")
  skip_if_not_installed("parsnip")

  set.seed(123)
  train_data <- mtcars

  model_spec <- parsnip::boost_tree(
    trees = 4,
    tree_depth = 2,
    min_n = 1
  ) |>
    parsnip::set_engine("xgboost") |>
    parsnip::set_mode("regression")

  model_fit <- parsnip::fit(
    model_spec,
    am ~ . - hp,
    data = train_data
  )
  xgb_model <- model_fit$fit

  # Test that formula can be generated and evaluated
  fit_formula <- tidypredict_fit(xgb_model)
  expect_type(fit_formula, "language")

  # Test that predictions can be generated
  preds <- rlang::eval_tidy(fit_formula, train_data)
  expect_type(preds, "double")
  expect_length(preds, nrow(train_data))
})

# Booster arguments that change predict() ------------------------------------

xgb_option_data <- function() {
  as.matrix(mtcars[, c("wt", "hp", "disp")])
}

expect_xgb_option_matches <- function(params, nrounds = 5) {
  x <- xgb_option_data()
  set.seed(1)
  model <- xgboost::xgb.train(
    params = params,
    data = xgboost::xgb.DMatrix(x, label = mtcars$mpg),
    nrounds = nrounds,
    verbose = 0
  )
  testthat::expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), as.data.frame(x)),
    as.numeric(predict(model, x)),
    tolerance = 1e-5
  )
}

test_that("num_parallel_tree predictions match native predict", {
  skip_if_not_installed("xgboost")

  expect_xgb_option_matches(list(
    objective = "reg:squarederror",
    num_parallel_tree = 3
  ))
})

test_that("objective hyperparameters match native predict", {
  skip_if_not_installed("xgboost")

  expect_xgb_option_matches(list(
    objective = "reg:tweedie",
    tweedie_variance_power = 1.9
  ))
  expect_xgb_option_matches(list(
    objective = "reg:pseudohubererror",
    huber_slope = 10,
    min_child_weight = 0
  ))
  expect_xgb_option_matches(list(
    objective = "count:poisson",
    max_delta_step = 0.1
  ))
  expect_xgb_option_matches(list(objective = "count:poisson", base_score = 3))
})

test_that("lossguide growth predictions match native predict", {
  skip_if_not_installed("xgboost")

  expect_xgb_option_matches(list(
    objective = "reg:squarederror",
    tree_method = "hist",
    grow_policy = "lossguide",
    max_depth = 0,
    max_leaves = 4
  ))
})

test_that("degenerate boosters match native predict", {
  skip_if_not_installed("xgboost")

  x <- as.matrix(mtcars[, "wt", drop = FALSE])
  set.seed(1)
  one_col <- xgboost::xgb.train(
    params = list(objective = "reg:squarederror"),
    data = xgboost::xgb.DMatrix(x, label = mtcars$mpg),
    nrounds = 5,
    verbose = 0
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(one_col), as.data.frame(x)),
    as.numeric(predict(one_col, x)),
    tolerance = 1e-5
  )

  full <- xgb_option_data()
  set.seed(1)
  shallow <- xgboost::xgb.train(
    params = list(objective = "reg:squarederror", max_depth = 1),
    data = xgboost::xgb.DMatrix(full, label = mtcars$mpg),
    nrounds = 5,
    verbose = 0
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(shallow), as.data.frame(full)),
    as.numeric(predict(shallow, full)),
    tolerance = 1e-5
  )
})

test_that("a booster of stumps matches native predict", {
  skip_if_not_installed("xgboost")

  full <- xgb_option_data()
  set.seed(1)
  constant <- xgboost::xgb.train(
    params = list(objective = "reg:squarederror"),
    data = xgboost::xgb.DMatrix(full, label = rep(5, nrow(full))),
    nrounds = 5,
    verbose = 0
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(constant), as.data.frame(full)),
    as.numeric(predict(constant, full)),
    tolerance = 1e-5
  )

  set.seed(1)
  one_row <- xgboost::xgb.train(
    params = list(objective = "reg:squarederror"),
    data = xgboost::xgb.DMatrix(full[1, , drop = FALSE], label = mtcars$mpg[1]),
    nrounds = 3,
    verbose = 0
  )
  expect_equal(
    rlang::eval_tidy(tidypredict_fit(one_row), as.data.frame(full)),
    as.numeric(predict(one_row, full)),
    tolerance = 1e-5
  )
})

test_that("a booster trained on data containing NA matches native predict", {
  skip_if_not_installed("xgboost")

  x <- xgb_option_data()
  x[1:3, "wt"] <- NA

  set.seed(1)
  model <- xgboost::xgb.train(
    params = list(objective = "reg:squarederror"),
    data = xgboost::xgb.DMatrix(x, label = mtcars$mpg),
    nrounds = 5,
    verbose = 0
  )

  expect_equal(
    rlang::eval_tidy(tidypredict_fit(model), as.data.frame(x)),
    as.numeric(predict(model, x)),
    tolerance = 1e-5
  )
})
