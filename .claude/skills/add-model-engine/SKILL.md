---
name: add-model-engine
description: Add tidypredict support for a new parsnip engine of an already-supported model type. Use when a user wants an existing tidymodels model type to work with an additional engine (e.g. "add the glm engine to linear_reg()", "support partykit in rand_forest()"). This is the lighter-weight of the two model-support skills. If the engine's fitted object is a class tidypredict does not yet parse, use the add-model-type skill first.
---

# Add a parsnip engine to tidypredict

This covers adding a new engine for a parsnip model type that tidypredict already supports. The prediction structure is already handled; you are exposing another engine path. (tidymodels/tidypredict#232, if it still exists, is a checklist of model types and engines worth adding.)

## The key question: is the underlying class already supported?

When you `parsnip::fit()` a spec, the result is a `model_fit` whose `$fit` is the raw engine object. tidypredict's `tidypredict_fit.model_fit` and `parse_model.model_fit` (in `R/tidymodels.R`) simply delegate to that `$fit`, dispatching on its class.

So there are two cases:

1. **The engine's fitted object is a class tidypredict already parses.** Then there is likely **no R code to write at all** — the engine works through delegation, and your job is just to verify it, add a test, and document it (touching only `NEWS.md`, `tests/testthat/test-tidymodels.R`, the snapshot, and a vignette). The `glm` engine for `linear_reg()` was added this way.

2. **The engine produces a class tidypredict does not yet handle.** Then you must add class support first with the **add-model-type** skill, then come back here to wire up and test the engine. This is what happened with the `quantreg` engine, which introduced the `rq`/`rqs` classes.

Verify which case you are in early:

```
Rscript -e 'devtools::load_all(); m <- parsnip::fit(parsnip::set_engine(parsnip::MODEL_TYPE(), "ENGINE"), mpg ~ wt + cyl, data = mtcars); print(class(m$fit)); print(tidypredict_fit(m))'
```

If `tidypredict_fit(m)` returns a language object, you are in case 1. If it errors with no applicable method, you are in case 2 (go to add-model-type first).

## Special engine preprocessing

Some engines need adjustment before delegation. These live in `R/tidymodels.R`:

- **glmnet** needs the penalty resolved into concrete coefficients (`glmnet_set_lambda()`).
- **catboost** with categorical features has bespoke handling (`tidypredict_fit_catboost_parsnip`).

If your engine needs the spec's arguments (like a penalty or a tuning value) baked into the fit before parsing, follow those patterns and add the branch in `R/tidymodels.R`.

## Steps

1. Confirm which case you are in (above).
2. If case 2, complete the add-model-type skill for the underlying class first.
3. Add any needed engine preprocessing to `R/tidymodels.R`.
4. Add a test in `tests/testthat/test-tidymodels.R` next to the existing engine tests:

   ```r
   test_that("works with MODEL_TYPE() and the ENGINE engine", {
     skip_if_not_installed("PKG")
     model <- parsnip::fit(
       parsnip::set_engine(parsnip::MODEL_TYPE(), "ENGINE"),
       mpg ~ wt + cyl,
       data = mtcars
     )
     expect_type(tidypredict_fit(model), "language")
     expect_snapshot(tidypredict_test(model, df = mtcars))
   })
   ```

   **Cover the arguments that change the model, not just the default fit.** Look at what the engine and its parsnip spec expose, and test the ones that alter the prediction: every `mode` the model type supports (the mars test in `test-tidymodels.R` fits both `"classification"` and `"regression"`), and engine-specific arguments set via `set_engine()`/`set_args()`/`set_mode()` (the quantreg engine test sets `quantile_levels`). Assert `tidypredict_fit()` works and `tidypredict_test()` matches `predict()` for each.

   If the underlying class supports SQL, also confirm it survives the parsnip wrapper: `tidypredict_sql(model, dbplyr::simulate_dbi())` returns class `"sql"`. Tree/boosting engines (e.g. the bonsai lightgbm/catboost tests) go further and round-trip through a real SQLite DB. Note that a `model_fit` carries the recipe's `xlevels`, so categorical handling can differ from a bare fit and is worth a dedicated test.

5. Document it: add a note to the relevant vignette (e.g. `vignettes/glm.Rmd`, `vignettes/lm.Rmd`) showing the engine working, mirroring how sibling engines are described.
6. Add a `NEWS.md` bullet (alphabetical by function name, mention the engine and the issue/PR number, no line wrapping). Example: `` * `linear_reg()` models can now use the `"glm"` engine (#239). ``
7. If the modeling package is newly required by tests, add it to `Suggests` in `DESCRIPTION`.

## Wrap up

```
air format .
Rscript -e "devtools::test(filter = '^tidymodels')"
Rscript -e "testthat::snapshot_review('tidymodels')"   # if snapshot changed
```

Then sanity-check the touched file set against a comparable past change: `git log --oneline --grep="engine"` to find one, then `git show --stat <sha>`. A pure new-engine change touches only tests, a snapshot, `NEWS.md`, and a vignette; if it also added a class, expect `R/` and `NAMESPACE` changes too.

## Tracking issue (if present)

If a tracking issue for supported model types and engines exists (historically tidymodels/tidypredict#232), note in your PR that the relevant checkbox should be checked. Skip this if no such issue is open.
