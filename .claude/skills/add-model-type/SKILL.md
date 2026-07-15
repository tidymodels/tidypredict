---
name: add-model-type
description: Add tidypredict support for a new underlying model class/algorithm. Use when a user wants tidypredict to parse a model class it does not yet handle (a new function like quantreg::rq, or a proposed model type such as svm_linear, naive_Bayes, pls, C5_rules). This is the heavier of the two model-support skills: it implements the S3 generics, the parser, and the Tidy Eval formula builder for the new class. To only expose an already-supported class through a new parsnip engine, use add-model-engine instead.
---

# Add a new model class to tidypredict

This covers adding support for an R model class that tidypredict does not yet parse: either a new function (like `quantreg::rq`) or a not-yet-supported model type (`svm_linear()`, `naive_Bayes()`, `pls()`, etc.). tidypredict turns a fitted model into a Tidy Eval formula; supporting a new class means implementing a small set of S3 generics plus, for novel algorithms, new parsing and formula-building logic. (A tracking issue of candidate model types may exist, historically tidymodels/tidypredict#232.)

## First: which family does the model fit?

The amount of new logic depends on whether an existing formula builder already produces the shape you need.

- **Linear / additive-in-coefficients** (`lm`, `glm`, `earth`, `glmnet`, `rq`). Prediction is a weighted sum of terms. These reuse `parse_model_lm()` and `build_fit_formula()` almost verbatim — adding one is mostly S3 wiring. `R/model-rq.R` is the smallest such file; `R/model-lm.R` and `R/model-earth.R` show fuller versions.
- **Tree / ensemble** (`rpart`, `ranger`, `randomForest`, `xgboost`, `lightgbm`, `catboost`, `cubist`, `partykit`). Prediction walks a tree into nested `case_when()` (parsed-model version 3). Substantial; study the closest existing `R/model-*.R`.
- **Genuinely new structure** (SVM, naive Bayes, PLS, discriminant, MLP). No existing builder fits. You must design a parser that captures the model's parameters and a builder that emits a Tidy Eval expression using only SQL-translatable operations (arithmetic, `case_when`, `exp`/`log`, comparisons). This is real design work — start by writing, by hand, the prediction formula for a tiny fitted example and confirming it matches `predict()`, then codify it.

If the family is unclear, ask the user which existing model is most similar, or spike the by-hand formula first.

## The three S3 generics

Every model dispatches on its S3 class. Define all of these (each `#' @export`) in a new `R/model-<name>.R`, with comment-banner sections matching the existing model files (`# Predict`, `# Parse model`).

1. **`tidypredict_fit.<class>`** — usually `parse_model(model)` then hand the parsed model to a builder. For linear models: `build_fit_formula(parse_model(model))`.
2. **`parse_model.<class>`** — extract the model into a serializable parsed structure. For linear models this is just `parse_model_lm(model)`. New algorithms build a list with a `$general` block (`model`, `type`, `version`) plus algorithm-specific fields. `parse_model` (`R/parsemodel.R`) documents the parsed-model contract and the `pm_*` type/subclass system used for dispatch of `tidypredict_fit`.
3. **`acceptable_formula.<class>`** — reuse `acceptable_lm(model)` for formula-based models; otherwise implement the checks that reject formula features you cannot translate (unsupported contrasts, inline functions).

### Minimal linear example

```r
# Predict ---------------------------------------
#' @export
tidypredict_fit.myclass <- function(model) {
  parsedmodel <- parse_model(model)
  build_fit_formula(parsedmodel)
}

# Parse model --------------------------------------
#' @export
parse_model.myclass <- function(model) parse_model_lm(model)

#' @export
acceptable_formula.myclass <- function(model) acceptable_lm(model)
```

If `parse_model_lm()` does not already reach your class, add your class where the existing dispatch branches (see the `glm` branch at `R/model-lm.R:107`). Read `parse_model_lm` before assuming it works — check it pulls coefficients and terms the way your class stores them.

## tidypredict_test

`tidypredict_test` (`R/tidypredict_test.R`) has a `.default` method that works for any model with a standard `predict()`. Only add a `tidypredict_test.<class>` method if the model needs matrix input or non-standard prediction (see the xgboost/lightgbm/catboost methods). Multiple-output models (multiple quantiles, multiclass) return a named list of expressions from `tidypredict_fit`; mirror the `rqs` / catboost-multiclass handling.

## Register, then document

```
Rscript -e "devtools::document()"
```

Confirm the new `S3method(...)` lines appear in `NAMESPACE`.

## Tests

Create `tests/testthat/test-model-<name>.R`, modeled on `test-model-rq.R` (a simple linear model) or `test-model-rpart.R` / `test-model-lightgbm.R` (trees/boosting). The suite has a strong shared shape; follow it.

**The canonical first test.** Nearly every model file opens with a test named `"returns the right output"` (boosting files call it `"parse_model returns correct structure"`) asserting the full bundle:

- `tidypredict_fit(model)` returns `"language"` (`expect_type(tf, "language")`).
- `parse_model(model)` is a list of length 2 (`general` + one payload slot): `expect_s3_class(pm, "list")`, `expect_equal(length(pm), 2)`.
- `pm$general$model` equals the recorded model string, and `pm$general$version` equals the parser version (linear-family models are v2, trees v3, glmnet v1).
- Snapshot the fit expression: `expect_snapshot(rlang::expr_text(tf))`.

**Correctness vs `predict()`.** Either snapshot `tidypredict_test(model, df)`, or evaluate the fit expression directly (`rlang::eval_tidy(tf, df)` / `dplyr::mutate(df, pred = !!tf)`) and `expect_equal` to `predict(model, ...)`. Include a factor/character predictor to exercise contrasts and categorical splits.

**Cover the arguments that change the fitted model.** Read the model function's signature and identify every argument that alters coefficients, tree structure, or the prediction, then fit with non-default values for each and assert both `tidypredict_fit()` works and `tidypredict_test()` matches `predict()`. Do not just test defaults. Examples: `test-model-rq.R` sweeps `tau`, `method`, `weights` and multiple quantiles; `test-model-lightgbm.R` has a "... predictions match native predict" test per `objective`; `test-model-earth.R` sweeps `pmethod`, `degree`, and the formula-vs-XY interface.

**Conventions to match:**

- `skip_if_not_installed("<pkg>")` in each `test_that` for any Suggests-only package.
- **Numeric stability**: coefficient snapshots are unstable across OS. Round `model$coefficients` before snapshotting (see `lm`/`glm`/`earth`), or use the `round_print()` helper in `helper-printing.R` (see glmnet). Ranger tests additionally `skip_on_cran()` and `skip_on_os(c("windows", "linux"))`.
- **Error paths**: use `expect_snapshot(..., error = TRUE)` for unsupported configurations (bad objective/family, unsupported multi-output). A regression-only model must error on classification in **both** `tidypredict_fit` and `parse_model` (see rf/ranger).
- **Save/reload roundtrip**: add a `"Model can be saved and re-loaded"` test that writes `parse_model()` to YAML and back (`yaml::write_yaml()` -> `yaml::read_yaml()` -> `as_parsed_model()`) and asserts the fit is unchanged. Present in lm, glm, cubist, earth, glmnet, and the boosting files.
- **Embed the issue number** in the test description when a test guards a specific fix (e.g. `"Gamma family works (#200)"`), matching the existing tests.

**Classification, multiclass, multi-output.** Classification models compare against `predict(model, type = "class"|"prob"|"response")`. Multiclass and multi-output models return a **named list** of expressions from `tidypredict_fit` (see `rqs` quantiles, lightgbm/catboost multiclass): assert `expect_named`, that each element is a language object, that probabilities sum to 1, and note that `tidypredict_test` intentionally errors on multiclass.

**Tree-specific tests** (if applicable): a `"produced case_when uses .default"` test grepping the expr text for `\.default`, a stump test (no splits -> fit returns the bare mean), and the nested-`case_when` builder tests in `test-tree-nested.R`.

**Other test files to touch:**

- `tests/testthat/test-acceptable.R`: add a case for `acceptable_formula.<class>` if you implemented it (it should reject inline formula functions and accept valid syntax).
- `tests/testthat/test-sql.R` or inline: confirm SQL translation. Minimally `tidypredict_sql(model, dbplyr::simulate_dbi())` returns class `"sql"`. Tree/boosting models additionally round-trip through a real SQLite DB (guarded by `skip_if_not_installed` on `DBI`/`RSQLite`/`dbplyr`) comparing DB-computed predictions to `predict()`.
- If your parser's format later changes, backwards compatibility is covered by frozen `.rds` fixtures in `tests/testthat/backwards-compat/` with a `generate-*.R` script; a brand-new class needs no fixture at creation.

```
Rscript -e "devtools::test_active_file('R/model-<name>.R')"
Rscript -e "testthat::snapshot_review('model-<name>')"
```

## Docs, NEWS, DESCRIPTION

- **DESCRIPTION**: add the modeling package to `Suggests`. If it is a headline model type, add it to the `Description:` supported-models list.
- **NEWS.md**: one bullet, alphabetical by function name, mention the function and issue/PR number, no line wrapping.
- **Vignettes**: a small addition (like `rq`) can join the nearest existing vignette (`vignettes/lm.Rmd`). A brand-new headline model type gets its own `vignettes/<name>.Rmd` plus a `_pkgdown.yml` navbar entry under "Model list".
- If you add a new exported doc topic, add it to `_pkgdown.yml` and run `Rscript -e "pkgdown::check_pkgdown()"`.

## Orbital helpers

tidypredict does some of the heavy lifting for the orbital package, which builds its own predictions from tidypredict's parsing internals rather than from the final Tidy Eval formula. Many supported models export a small set of helper functions specifically for orbital.

**Whether you need them depends on the family.** Simple linear-family classes usually do **not** add their own helpers: `R/model-rq.R` has no `# For {orbital}` section at all, because orbital consumes it through the shared linear path (the `.build_linear_pred` extractor lives in `R/model-glmnet.R`). Trees/forests and glmnet-style models, on the other hand, each export dedicated helpers, and for those a new class is **not complete without them**. If your class reuses `parse_model_lm()` and `build_fit_formula()` verbatim like `rq`, you can likely skip this section; if it introduces new tree or coefficient structure, add helpers matching the closest existing model.

The convention (grep `# For {orbital}` to see every example):

- A `# For {orbital}` banner section in `R/model-<name>.R`.
- Helper names are **dot-prefixed** (`.extract_xgb_trees`, `.rpart_tree_info_full`, `.extract_rf_classprob`, `.build_linear_pred`, `.extract_glmnet_multiclass`). The dot keeps them out of tab-completion while still exported.
- Roxygen with both `@keywords internal` and `@export` (exported so orbital can call them, but kept out of the reference index).
- They expose the raw structure orbital needs, not a formula. Established shapes:
  - **Linear models**: a linear-predictor builder / coefficient extractor (see `.build_linear_pred`, `.extract_glmnet_multiclass` in `R/model-glmnet.R`).
  - **Trees / forests**: tree-info extractors and, for classification, class-probability extractors (`.extract_rf_trees` / `.extract_rf_classprob`, `.rpart_tree_info_full` / `.extract_rpart_classprob`, and the shared `.build_nested_case_when_tree` in `R/tree-nested.R`).

Match the shape used by the closest existing model so orbital can consume your class the same way.

Test each helper directly (see the `# Tests for .extract_rf_classprob()` and `# Tests for .extract_rf_trees()` blocks in `test-model-rf.R`) with this three-part pattern:

- **Structure**: `expect_type(result, "list")`, `expect_length()` to the tree/class count, `expect_named()` by class level for classprob/multiclass, and `testthat::expect_all_true(vapply(result, is.language, logical(1)))` that every element is a language object (as rf/ranger do).
- **Correctness by reconstruction**: `rlang::eval_tidy` each expression, then combine the way the model does (average trees for regression, sum votes / n_trees for probabilities, softmax for glmnet multiclass) and `expect_equal` to `predict(model, type = "prob"|"response")`.
- **Error guards** via `expect_snapshot(..., error = TRUE)`: wrong model class, wrong task (classprob on a regression model, trees on a classification model), and any structural precondition (e.g. ranger needs `probability = TRUE`). Also cover edge counts: binary classification and a single-tree forest.

## parsnip engine

If the class is reachable through a parsnip engine, finish by exposing and testing that path with the **add-model-engine** skill (add a `test-tidymodels.R` test; `model_fit` delegation usually needs no new code).

## Wrap up

```
air format .
Rscript -e "devtools::test()"
Rscript -e "devtools::document()"
```

Review that DESCRIPTION, NEWS.md, NAMESPACE, tests, and docs are consistent. To see the full file set a real model-adding change touches, find a recent one in the history and inspect it, e.g. `git log --oneline --grep="[Aa]dd support"` then `git show --stat <sha>`. If a supported-models tracking issue is open (historically tidymodels/tidypredict#232), note in the PR which checkbox to check.
