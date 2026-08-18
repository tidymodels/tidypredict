test_that("returns the right output", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "C5.0")
  expect_equal(pm$general$version, 3)

  expect_snapshot(rlang::expr_text(tf))
})

test_that("numeric predictors match predict()", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  fit_pred <- as.character(rlang::eval_tidy(tidypredict_fit(model), df))
  expect_equal(fit_pred, as.character(predict(model, df)))
})

test_that("binary categorical splits match predict()", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  df$gear <- factor(df$gear)
  model <- C50::C5.0(df[, c("wt", "gear", "mpg")], df$vs)

  fit_pred <- as.character(rlang::eval_tidy(tidypredict_fit(model), df))
  expect_equal(fit_pred, as.character(predict(model, df)))
})

test_that("multiway categorical splits match predict()", {
  skip_if_not_installed("C50")
  set.seed(3)
  n <- 400
  f <- factor(sample(c("a", "b", "c", "d"), n, TRUE))
  y <- factor(c(a = "p", b = "q", c = "r", d = "p")[as.character(f)])
  df <- data.frame(f = f, y = y)
  model <- C50::C5.0(
    df["f"],
    df$y,
    control = C50::C5.0Control(CF = 0.9, minCases = 1)
  )

  n_in <- lengths(regmatches(
    rlang::expr_text(tidypredict_fit(model)),
    gregexpr("%in%", rlang::expr_text(tidypredict_fit(model)))
  ))
  expect_gt(n_in, 1)
  fit_pred <- as.character(rlang::eval_tidy(tidypredict_fit(model), df))
  expect_equal(fit_pred, as.character(predict(model, df)))
})

test_that("one-branch-per-level splits match predict() (#232)", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$am <- factor(df$am, labels = c("auto", "manual"))
  df$cyl <- factor(df$cyl)
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("cyl", "vs")], df$am)

  fit_pred <- as.character(rlang::eval_tidy(tidypredict_fit(model), df))
  expect_equal(fit_pred, as.character(predict(model, df)))
})

test_that("stump trees (no splits) work correctly", {
  skip_if_not_installed("C50")
  df <- mtcars
  y <- factor(c(rep("a", 30), rep("b", 2)))
  model <- C50::C5.0(df[, c("wt", "cyl"), drop = FALSE], y)

  expect_equal(tidypredict_fit(model), "a")
})

test_that("produced case_when uses .default", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  expect_match(rlang::expr_text(tidypredict_fit(model)), "\\.default")
})

test_that("boosted models match predict()", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs, trials = 5)

  fit_pred <- as.character(rlang::eval_tidy(tidypredict_fit(model), df))
  expect_equal(fit_pred, as.character(predict(model, df)))
})

test_that("boosted multiclass models match predict()", {
  skip_if_not_installed("C50")
  model <- C50::C5.0(iris[, 1:4], iris$Species, trials = 5)

  fit_pred <- as.character(rlang::eval_tidy(tidypredict_fit(model), iris))
  expect_equal(fit_pred, as.character(predict(model, iris)))
})

test_that("boosted models with categorical splits match predict()", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  df$gear <- factor(df$gear)
  model <- C50::C5.0(df[, c("wt", "gear", "mpg")], df$vs, trials = 5)

  fit_pred <- as.character(rlang::eval_tidy(tidypredict_fit(model), df))
  expect_equal(fit_pred, as.character(predict(model, df)))
})

c50_boost_data <- function(seed, n_class = 4, ordered = TRUE, n = 300) {
  set.seed(seed)
  d <- data.frame(
    x1 = rnorm(n),
    x2 = runif(n, -5, 5),
    x3 = rnorm(n, 10, 3),
    g = factor(sample(c("a", "b", "c"), n, replace = TRUE)),
    o = factor(
      sample(c("lo", "mid", "hi"), n, replace = TRUE),
      levels = c("lo", "mid", "hi"),
      ordered = ordered
    )
  )
  score <- d$x1 +
    0.4 * d$x2 -
    0.2 * d$x3 +
    as.integer(d$g) +
    0.8 * as.integer(d$o) +
    rnorm(n)
  cuts <- stats::quantile(score, seq(0, 1, length.out = n_class + 1))
  d$y <- factor(
    paste0("c", as.integer(cut(score, cuts, include.lowest = TRUE))),
    levels = paste0("c", seq_len(n_class))
  )
  d
}

test_that("boosted trials vote with the right confidence (#287)", {
  skip_if_not_installed("C50")
  # A trial votes with `(freq + prior) / (n_leaf + 1)`, where `prior` is the
  # class proportion at the root of that trial's own tree, not with the Laplace
  # ratio `(freq + 1) / (n_leaf + 2)`. The two are close enough that a
  # disagreement needs several trials and several classes to show up.
  for (seed in 1:6) {
    d <- c50_boost_data(seed)
    for (trials in c(3L, 5L, 10L)) {
      model <- C50::C5.0(d[c("x1", "x2", "x3", "g", "o")], d$y, trials = trials)

      expect_equal(
        as.character(rlang::eval_tidy(tidypredict_fit(model), d)),
        as.character(predict(model, d))
      )
    }
  }
})

test_that("a boosted vote tie goes to the default class (#287)", {
  skip_if_not_installed("C50")
  # `SelectClass` starts from the default class, so it wins any tie. Every
  # class here gets no vote at all, which is the tie that always exists.
  tree_info <- list(
    nodeID = 0L,
    leftChild = NA_integer_,
    rightChild = NA_integer_,
    splitvarName = NA_character_,
    terminal = TRUE,
    prediction = "b",
    confidence = 0,
    leaf_freq = list(NULL),
    node_splits = list(list(NULL)),
    majority_left = NA,
    use_surrogates = FALSE
  )

  fit <- c50_boosted_case_when(list(tree_info), c("a", "b", "c"), "c")
  expect_equal(rlang::eval_tidy(fit, data.frame(x = 1)), "c")
})

test_that("boosted models with NA in newdata match predict() (#416)", {
  skip_if_not_installed("C50")
  set.seed(11)
  n <- 400
  df <- data.frame(
    v1 = rnorm(n),
    v2 = rnorm(n),
    f1 = factor(sample(letters[1:4], n, TRUE))
  )
  lp <- df$v1 - 2 * df$v2 + 0.7 * as.integer(df$f1) + rnorm(n)
  df$g <- factor(c("A", "B", "C")[cut(lp, 3, labels = FALSE)])

  nd <- df
  for (col in c("v1", "v2", "f1")) {
    nd[[col]][sample(n, 60)] <- NA
  }

  for (trials in c(3L, 5L)) {
    model <- C50::C5.0(g ~ ., data = df, trials = trials)
    expected <- as.character(predict(model, nd))

    expect_equal(
      as.character(rlang::eval_tidy(tidypredict_fit(model), nd)),
      expected
    )
    expect_equal(
      as.character(rlang::eval_tidy(tidypredict_fit(parse_model(model)), nd)),
      expected
    )
  }
})

test_that("boosted models round-trip through parse_model()", {
  skip_if_not_installed("C50")
  model <- C50::C5.0(iris[, 1:4], iris$Species, trials = 5)

  pm <- parse_model(model)
  fit_pred <- as.character(rlang::eval_tidy(tidypredict_fit(pm), iris))
  expect_equal(fit_pred, as.character(predict(model, iris)))
})

test_that("the [ordered] marker is not read as part of a level (#287)", {
  # C5.0 declares an ordered predictor as `o: [ordered]lo,mid,hi.`, and the
  # marker used to be left on the first level, making it unmatchable.
  model <- list(
    names = paste(
      "outcome.",
      "",
      "outcome: hi,lo.",
      "x1: continuous.",
      "o: [ordered]lo,mid,hi.",
      "g: a,b,c.",
      sep = "\n"
    )
  )

  expect_equal(
    c50_attr_levels(model)[["o"]],
    c("lo", "mid", "hi")
  )
  expect_equal(c50_attr_levels(model)[["g"]], c("a", "b", "c"))
})

test_that("a model with no tree is reported clearly (#287)", {
  # `C5.0()` leaves the tree empty when fitting failed, which a level
  # containing a comma does, since that separates the levels in the model text.
  expect_snapshot(
    tidypredict_fit(structure(
      list(tree = "", levels = c("hi", "lo"), names = ""),
      class = "C5.0"
    )),
    error = TRUE
  )
})

test_that("errors on unsupported configurations", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)

  fuzzy <- C50::C5.0(
    df[, c("wt", "cyl")],
    df$vs,
    control = C50::C5.0Control(fuzzyThreshold = TRUE)
  )
  costs <- C50::C5.0(
    df[, c("wt", "cyl")],
    df$vs,
    costs = matrix(
      c(0, 1, 2, 0),
      nrow = 2,
      dimnames = list(levels(df$vs), levels(df$vs))
    )
  )

  expect_snapshot(tidypredict_fit(fuzzy), error = TRUE)
  expect_snapshot(tidypredict_fit(costs), error = TRUE)
})

test_that("SQL translation works", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  expect_s3_class(tidypredict_sql(model, dbplyr::simulate_dbi()), "sql")
})

test_that("predictions round-trip through a SQLite database", {
  skip_if_not_installed("C50")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")

  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  dplyr::copy_to(con, df, "mt")
  db_pred <- dplyr::tbl(con, "mt") |>
    dplyr::mutate(pred = !!tidypredict_fit(model)) |>
    dplyr::pull(pred)

  expect_equal(as.character(db_pred), as.character(predict(model, df)))
})

test_that("model can be saved and re-loaded", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  pm <- parse_model(model)
  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, tmp)
  pm2 <- as_parsed_model(yaml::read_yaml(tmp))

  reloaded <- as.character(rlang::eval_tidy(tidypredict_fit(pm2), df))
  expect_equal(reloaded, as.character(predict(model, df)))
})

test_that("rule-based models return the right output", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs, rules = TRUE)

  tf <- tidypredict_fit(model)
  pm <- parse_model(model)

  expect_type(tf, "language")

  expect_s3_class(pm, "list")
  expect_equal(length(pm), 2)
  expect_equal(pm$general$model, "C5.0")
  expect_equal(pm$general$version, 3)

  expect_snapshot(rlang::expr_text(tf))
})

test_that("rule-based models with numeric predictors match predict()", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs, rules = TRUE)

  fit_pred <- as.character(rlang::eval_tidy(tidypredict_fit(model), df))
  expect_equal(fit_pred, as.character(predict(model, df)))
})

test_that("rule-based models with categorical predictors match predict()", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  df$gear <- factor(df$gear)
  model <- C50::C5.0(df[, c("wt", "gear", "mpg")], df$vs, rules = TRUE)

  fit_pred <- as.character(rlang::eval_tidy(tidypredict_fit(model), df))
  expect_equal(fit_pred, as.character(predict(model, df)))
})

test_that("rule-based multiclass models match predict()", {
  skip_if_not_installed("C50")
  model <- C50::C5.0(iris[, 1:4], iris$Species, rules = TRUE)

  fit_pred <- as.character(rlang::eval_tidy(tidypredict_fit(model), iris))
  expect_equal(fit_pred, as.character(predict(model, iris)))
})

test_that("rule-based models with subset and equality conditions match predict()", {
  skip_if_not_installed("C50")
  skip_if_not_installed("modeldata")
  data(attrition, package = "modeldata", envir = environment())
  x <- attrition[, setdiff(names(attrition), "Attrition")]
  model <- C50::C5.0(x, attrition$Attrition, rules = TRUE)

  fit_pred <- as.character(rlang::eval_tidy(tidypredict_fit(model), attrition))
  expect_equal(fit_pred, as.character(predict(model, attrition)))
})

test_that("rule-based models round-trip through parse_model()", {
  skip_if_not_installed("C50")
  model <- C50::C5.0(iris[, 1:4], iris$Species, rules = TRUE)

  pm <- parse_model(model)
  fit_pred <- as.character(rlang::eval_tidy(tidypredict_fit(pm), iris))
  expect_equal(fit_pred, as.character(predict(model, iris)))
})

test_that("rule-based models can be saved and re-loaded", {
  skip_if_not_installed("C50")
  model <- C50::C5.0(iris[, 1:4], iris$Species, rules = TRUE)

  pm <- parse_model(model)
  tmp <- withr::local_tempfile(fileext = ".yml")
  yaml::write_yaml(pm, tmp)
  pm2 <- as_parsed_model(yaml::read_yaml(tmp))

  reloaded <- as.character(rlang::eval_tidy(tidypredict_fit(pm2), iris))
  expect_equal(reloaded, as.character(predict(model, iris)))
})

test_that("boosted rule-based models are not supported", {
  skip_if_not_installed("C50")
  model <- C50::C5.0(iris[, 1:4], iris$Species, rules = TRUE, trials = 3)

  expect_snapshot(tidypredict_fit(model), error = TRUE)
})

test_that("rule-based predictions round-trip through a SQLite database", {
  skip_if_not_installed("C50")
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("dbplyr")

  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs, rules = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con))
  dplyr::copy_to(con, df, "mt")
  db_pred <- dplyr::tbl(con, "mt") |>
    dplyr::mutate(pred = !!tidypredict_fit(model)) |>
    dplyr::pull(pred)

  expect_equal(as.character(db_pred), as.character(predict(model, df)))
})

test_that(".c50_tree_info_full is exported and works", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  info <- .c50_tree_info_full(model)
  expect_type(info, "list")
  expect_true(all(
    c("nodeID", "leftChild", "rightChild", "terminal") %in% names(info)
  ))
})

test_that("tidypredict_test() agrees with predict()", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  result <- tidypredict_test(model, df)
  expect_false(result$alert)
})

test_that("tidypredict_test() works for boosted models", {
  skip_if_not_installed("C50")
  model <- C50::C5.0(iris[, 1:4], iris$Species, trials = 3)

  result <- tidypredict_test(model, iris)
  expect_false(result$alert)
})

test_that("tidypredict_test() works for rule-based models", {
  skip_if_not_installed("C50")
  model <- C50::C5.0(iris[, 1:4], iris$Species, rules = TRUE)

  result <- tidypredict_test(model, iris)
  expect_false(result$alert)
})

test_that("tidypredict_test() honours max_rows", {
  skip_if_not_installed("C50")
  model <- C50::C5.0(iris[, 1:4], iris$Species)

  result <- tidypredict_test(model, iris, max_rows = 10)
  expect_equal(nrow(result$raw_results), 10)
})

test_that("tidypredict_test() compares against predict(), not against itself", {
  skip_if_not_installed("C50")
  model <- C50::C5.0(iris[, 1:4], iris$Species)

  result <- tidypredict_test(model, iris)

  # Guards against the failure mode where both sides of the comparison come
  # from tidypredict, which makes `alert` unable to ever be TRUE.
  expect_equal(result$raw_results$fit, as.character(predict(model, iris)))
  expect_true(any(result$raw_results$fit != "setosa"))
})

test_that("training data containing NA matches predict()", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  df$wt[1:5] <- NA_real_
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  expect_equal(
    as.character(rlang::eval_tidy(tidypredict_fit(model), df)),
    as.character(predict(model, df))
  )
})

test_that("an unused outcome level matches predict()", {
  skip_if_not_installed("C50")
  df <- mtcars
  y <- factor(as.character(df$vs), levels = c("0", "1", "2"))
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], y)

  expect_equal(
    as.character(rlang::eval_tidy(tidypredict_fit(model), df)),
    as.character(predict(model, df))
  )
})

test_that("ordered factor predictors match predict()", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  df$o <- factor(df$gear, ordered = TRUE)
  model <- C50::C5.0(df[, c("o", "wt")], df$vs)

  expect_equal(
    as.character(rlang::eval_tidy(tidypredict_fit(model), df)),
    as.character(predict(model, df))
  )
})

test_that("a single-column model matrix matches predict()", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, "wt", drop = FALSE], df$vs)

  expect_equal(
    as.character(rlang::eval_tidy(tidypredict_fit(model), df)),
    as.character(predict(model, df))
  )
})

test_that("newdata containing NA matches predict() (#387)", {
  skip_if_not_installed("C50")
  df <- mtcars
  df$vs <- factor(df$vs)
  model <- C50::C5.0(df[, c("wt", "cyl", "mpg")], df$vs)

  nd <- df
  nd$cyl[6:9] <- NA_real_

  expect_equal(
    as.character(rlang::eval_tidy(tidypredict_fit(model), nd)),
    as.character(predict(model, nd))
  )
})

test_that("NA in a factor predictor matches predict() (#387)", {
  skip_if_not_installed("C50")
  set.seed(24)
  n <- 400
  df <- data.frame(
    v1 = rnorm(n),
    v2 = rnorm(n),
    f1 = factor(sample(letters[1:4], n, TRUE)),
    f2 = factor(sample(c("p", "q", "r", "s", "t"), n, TRUE))
  )
  lp <- df$v1 -
    2 * df$v2 +
    0.7 * as.integer(df$f1) +
    0.4 * as.integer(df$f2) +
    rnorm(n)
  df$g <- factor(c("A", "B", "C")[cut(lp, 3, labels = FALSE)])
  model <- C50::C5.0(
    g ~ .,
    data = df,
    control = C50::C5.0Control(subset = TRUE)
  )

  nd <- df
  for (col in c("v1", "v2", "f1", "f2")) {
    nd[[col]][sample(n, 60)] <- NA
  }
  expected <- as.character(predict(model, nd))

  expect_equal(
    as.character(rlang::eval_tidy(tidypredict_fit(model), nd)),
    expected
  )
  expect_equal(
    as.character(rlang::eval_tidy(tidypredict_fit(parse_model(model)), nd)),
    expected
  )
})

test_that("class probabilities with NA in newdata match predict() (#417)", {
  skip_if_not_installed("C50")
  set.seed(51)
  n <- 400
  df <- data.frame(
    v1 = rnorm(n),
    v2 = rnorm(n),
    f1 = factor(sample(letters[1:4], n, TRUE))
  )
  lp <- df$v1 - 2 * df$v2 + 0.7 * as.integer(df$f1) + rnorm(n)
  df$g <- factor(c("A", "B", "C")[cut(lp, 3, labels = FALSE)])
  model <- C50::C5.0(g ~ ., data = df)

  nd <- df
  for (col in c("v1", "v2", "f1")) {
    nd[[col]][sample(n, 60)] <- NA
  }
  expected <- predict(model, nd, type = "prob")

  tree_info <- c50_classprob_tree_info(model)
  for (cl in colnames(expected)) {
    expect_equal(
      rlang::eval_tidy(classprob_tree_expr(tree_info[[cl]]), nd),
      unname(expected[, cl]),
      tolerance = 1e-6
    )
  }
})

test_that("values on a cut boundary match C5.0's float comparison (#287)", {
  set.seed(3)
  n <- 600
  df <- as.data.frame(matrix(rnorm(n * 4), ncol = 4))
  names(df) <- paste0("v", 1:4)
  df$g <- factor(
    c("a", "b", "c")[cut(
      as.matrix(df) %*% c(1, -2, 0.5, 1) + rnorm(n),
      3,
      labels = FALSE
    )]
  )

  model <- C50::C5.0(g ~ ., data = df)

  lines <- strsplit(model$tree, "\n")[[1]]
  cuts <- as.numeric(gsub(
    '.*cut="([^"]*)".*',
    "\\1",
    grep('cut="', lines, value = TRUE)
  ))
  cuts <- sort(unique(cuts[is.finite(cuts)]))
  expect_gt(length(cuts), 0)

  # values between a cut and its 32-bit float image: C5.0 compares these as
  # floats and treats them as equal to the cut, R compares them as doubles.
  for (col in paste0("v", 1:4)) {
    probe <- df[rep(1, length(cuts)), paste0("v", 1:4), drop = FALSE]
    probe[[col]] <- (cuts + as_f32(cuts)) / 2

    expect_equal(
      as.character(rlang::eval_tidy(tidypredict_fit(model), probe)),
      as.character(predict(model, probe))
    )
  }
})
