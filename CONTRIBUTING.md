# Contributing to tidypredict

## Testing a new model

Every supported model needs the same battery of tests. The list below
exists because coverage had drifted by more than an order of magnitude
between model files (64 lines for `rq` against 2392 for `lightgbm`), and
the thin files were where real bugs survived: an xgboost round-trip that
compared tidypredict against itself, and a
[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
method for ranger that could never fail.

Tests for `R/model-{name}.R` go in `tests/testthat/test-model-{name}.R`.
Work through this list:

1.  **[`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
    structure.** Check `general$model`, `general$type`, and
    `general$version` are what you expect, plus whatever the builder
    relies on.
2.  **[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
    output.** Snapshot the generated expression with `round_print()` so
    the snapshot does not depend on floating point noise or the
    platform.
3.  **Numeric agreement against the package’s own
    [`predict()`](https://rdrr.io/r/stats/predict.html).** This is the
    assertion that matters. Evaluate the fitted formula with
    [`rlang::eval_tidy()`](https://rlang.r-lib.org/reference/eval_tidy.html)
    and compare against the model package’s
    [`predict()`](https://rdrr.io/r/stats/predict.html), never against
    another tidypredict result.
4.  **[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md).**
    Assert directly on the result (`expect_false(result$alert)`), rather
    than wrapping it in `expect_snapshot()`. A numeric regression should
    read as a failure, not as a snapshot diff.
5.  **[`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md).**
    At least one test that the column actually lands in the data frame.
6.  **[`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md).**
    Confirm the formula survives translation.
7.  **YAML round-trip.**
    [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md),
    write, read,
    [`as_parsed_model()`](https://tidypredict.tidymodels.org/reference/as_parsed_model.md),
    then compare against the model’s own
    [`predict()`](https://rdrr.io/r/stats/predict.html). Comparing the
    reloaded parsed model against the un-serialized one only proves
    serialization is lossless: a parser that is wrong the same way on
    both sides passes.
8.  **Print snapshot**, if the model has bespoke print output.
9.  **Unsupported configurations.** Snapshot the error for anything the
    parser rejects (a family, an objective, a classification mode).
10. **Factor predictors**, if the model accepts them.
11. **`NA` handling.** Predict on data that actually contains `NA`, not
    just a structural check that a missing-value branch was generated.

### Edge cases every parser must cover

Items 10 and 11 above are the short form of a longer list. A triage of
the bug-labelled issues found that almost all of them shared one root
cause: each model’s tests fit a clean numeric model on clean data and
compared against [`predict()`](https://rdrr.io/r/stats/predict.html),
and every bug lived somewhere else. Five sweeps across the supported
model classes each turned up several independent bugs, in these five
categories. Work through all five for any model you add or change.

The reasons matter more than the list. A new model class will have edge
cases nobody anticipated, and the way to find them is to ask the same
underlying questions.

1.  **Factors and categorical splits.** Test a factor predictor with
    contrast coding other than `contr.treatment`, since an ordered
    factor gets `contr.poly` and produces `.L`/`.Q` columns whose names
    and values a parser written against dummy variables will not
    reconstruct. Test a factor with an unused level, a level whose name
    collides with another variable in the data, and a level containing a
    special character such as `:`. The parser has to recover the mapping
    from model matrix column names back to variable and level, and every
    one of these breaks a naive string split.
2.  **Missing values.** Predict on newdata containing `NA` and compare
    against [`predict()`](https://rdrr.io/r/stats/predict.html), for
    every model. Eight of sixteen regression models disagreed with their
    own [`predict()`](https://rdrr.io/r/stats/predict.html) here. Also
    fit a model whose *training* data contains `NA`, which is a
    different case: it changes what the model stores (LightGBM’s
    `missing_type`) and can add surrogate splits (rpart) that the
    generated formula has to honor. A structural check that a
    missing-value branch was generated is not enough; the branch has to
    produce the right number.
3.  **Threshold precision.** Models that store thresholds in float32 are
    compared against float64 data at prediction time, so test a value
    exactly at a split threshold, a value at the float32 representation
    of that threshold, and a boundary tie. Whether the comparison is `<`
    or `<=`, and at what precision, decides which leaf a row lands in.
4.  **Options the parser might ignore.** Any model argument that changes
    [`predict()`](https://rdrr.io/r/stats/predict.html) output but that
    the parser never reads is a silent wrong answer. Fit the model with
    such arguments set to non-default values and compare numerically.
    Past examples: LightGBM’s `sigmoid`, `reg_sqrt`, and
    `zero_as_missing`, catboost objectives with a link function, glmnet
    offsets, and mboost’s `mstop`.
5.  **Degenerate fit shapes.** Fit a stump (a root-only tree), a
    single-column model matrix, a rank-deficient fit with `NA`
    coefficients, a model on a constant outcome, and single-row or
    single-class training data. These produce structures the parser’s
    loops were never written for: an empty split table, a vector where a
    matrix was assumed, a level that never appears.

### Fixing a bug reported in an issue

Two habits, both learned from getting them wrong:

- **Check the stated mechanism against the code before implementing the
  proposed fix.** Six issues in the last triage proposed changes that
  would have been regressions, because they reasoned from the symptom
  rather than from the source. One named `"partykit"` as the stored
  model type when parsed models actually store `"party"`. One asserted
  that `cross_entropy` applies LightGBM’s `sigmoid` scaling, which it
  does not. One proposed returning `TRUE` from a path handler in a way
  that would have emitted `TRUE & x > 4` into the generated SQL. Read
  the parser and confirm the described behavior is real before changing
  it.
- **Verify numerically, not structurally.** A regression test for a bug
  fix has to compare against the modelling package’s own
  [`predict()`](https://rdrr.io/r/stats/predict.html) on data that
  reproduces the bug. Assertions of the form `expect_false(is.null(x))`,
  or that the call runs without error, have repeatedly passed while the
  computed prediction was wrong. If the test would still pass with the
  fix reverted, it is not a test of the fix.

### Conventions

- Use `skip_if_not_installed("pkg")` in every test that needs a
  suggested package, rather than `skip_on_cran()`.
- Use `withr::local_tempfile(fileext = ".yml")` for temp files, so they
  are cleaned up.
- No [`library()`](https://rdrr.io/r/base/library.html) calls. Namespace
  everything.
- Prefer a direct assertion for correctness and reserve snapshots for
  printed output and error messages.
- `catboost` is not on CRAN, so it is deliberately absent from
  `Suggests`. Tests and `vignettes/catboost.Rmd` that need it are
  guarded by `skip_if_not_installed("catboost")`. To run them locally,
  install it from the upstream releases: see
  <https://catboost.ai/docs/en/installation/r-installation-binary-installation>.

### Local setup for h2o and catboost

Two backends need more than an
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
before their tests will run, so a fresh checkout will report skips for
them. This is expected, and neither is required to contribute.

**h2o** needs a Java runtime as well as the `h2o` and `agua` packages.
`skip_if_no_h2o()` in `tests/testthat/helper-h2o.R` skips when Java is
missing, when either package is missing, when a cluster cannot be
started, or when the running cluster’s version does not match the
installed `h2o` R package. That last case matters if you already have a
long-lived cluster on `localhost:54321` from an older `h2o`: shut it
down with `h2o::h2o.shutdown(prompt = FALSE)` and let the helper start a
fresh one. The helper starts the cluster at most once per run and shuts
it down at the end of the suite.

**catboost** is not on CRAN and has to be installed from its GitHub
release, for example with
`install.packages(<release URL>, repos = NULL, type = "source")`. Its
tests skip cleanly when it is absent.

### Comparing against the fitted model, not against tidypredict

The single most common way a test here has been wrong is comparing two
tidypredict outputs to each other. `tidypredict_fit(pm)` against
`tidypredict_fit(pm_loaded)`, or a parsed model against a fitted one, is
a real assertion but a weak one: it cannot catch a parser that is
consistently wrong. Every model needs at least one test whose right-hand
side comes from the modelling package itself.
