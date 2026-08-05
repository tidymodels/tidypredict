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

### Conventions

- Use `skip_if_not_installed("pkg")` in every test that needs a
  suggested package, rather than `skip_on_cran()`.
- Use `withr::local_tempfile(fileext = ".yml")` for temp files, so they
  are cleaned up.
- No [`library()`](https://rdrr.io/r/base/library.html) calls. Namespace
  everything.
- Prefer a direct assertion for correctness and reserve snapshots for
  printed output and error messages.

### Comparing against the fitted model, not against tidypredict

The single most common way a test here has been wrong is comparing two
tidypredict outputs to each other. `tidypredict_fit(pm)` against
`tidypredict_fit(pm_loaded)`, or a parsed model against a fitted one, is
a real assertion but a weak one: it cannot catch a parser that is
consistently wrong. Every model needs at least one test whose right-hand
side comes from the modelling package itself.
