# Plan: Add LightGBM Model Support to tidypredict

## Overview

Add support for LightGBM models (`lgb.Booster`) to tidypredict, enabling SQL-compatible prediction formulas that can run inside databases. This follows the existing xgboost implementation pattern.

---

## Phase 1: Model Parsing ✅

- [x] Create `R/model-lightgbm.R`
- [x] Implement `parse_model.lgb.Booster()`
- [x] Implement `get_lgb_trees()`
- [x] Implement `get_lgb_tree()`
- [x] Implement `get_lgb_children_map()`
- [x] Implement `get_lgb_path()`
- [x] Add `lightgbm` to DESCRIPTION Suggests
- [x] Create `tests/testthat/test-model-lightgbm.R`
- [x] Test basic structure extraction
- [x] Test tree/leaf/path structure
- [x] Test feature names extraction
- [x] Test params extraction
- [x] Test `niter` and `nfeatures` extraction
- [x] Test `default_left = TRUE` handling
- [x] Test `default_left = FALSE` handling
- [x] Test deep tree path tracing
- [x] Test single leaf tree (stump)
- [x] Test mixed `default_left` values
- [x] Test models with missing values in training data
- [x] Test models without explicit feature names
- [x] Add validation error for categorical splits
- [x] Validate implementation against LightGBM source code
- [x] Pass R CMD check with 0 errors, 0 warnings, 0 notes

**Parsed Model Structure:**
```r
pm$general$model         # "lgb.Booster"
pm$general$type          # "lgb"
pm$general$version       # 1
pm$general$params        # Model parameters (objective, etc.)
pm$general$feature_names # Feature names
pm$general$niter         # Number of boosting rounds
pm$general$nfeatures     # Number of features
pm$trees                 # List of trees with leaves containing:
                         #   - prediction: leaf value
                         #   - path: list of conditions
```

---

## Phase 2: Fit Formula Generation

### 2.1 Basic Infrastructure ✅

- [x] Add `tidypredict_fit.lgb.Booster()` S3 method
- [x] Add `tidypredict_fit.pm_lgb()` S3 method
- [x] Implement `build_fit_formula_lgb()`
- [x] Implement `get_lgb_case()` - combines path conditions
- [x] Implement `get_lgb_case_fun()` - single condition to expression
- [x] Implement `get_lgb_case_tree()` - all leaves in a tree
- [x] Handle `missing = TRUE` with `is.na()` in conditions
- [x] Handle `missing = FALSE` without `is.na()`
- [x] Sum all trees (additive model)
- [x] Update NAMESPACE with new S3 methods

### 2.2 Regression Objectives ✅

- [x] Support `regression` / `regression_l2` (identity transform)
- [x] Support `regression_l1` (identity transform)
- [x] Support `huber` (identity transform)
- [x] Support `fair` (identity transform)
- [x] Support `poisson` (`exp(sum)` transform)
- [x] Support `gamma` (`exp(sum)` transform)
- [x] Support `tweedie` (`exp(sum)` transform)
- [x] Support `quantile` (identity transform)
- [x] Support `mape` (identity transform)

### 2.3 Binary Classification Objectives ✅

- [x] Support `binary` (sigmoid transform)
- [x] Support `cross_entropy` (sigmoid transform)

### 2.4 Multiclass Classification Objectives ✅

- [x] Support `multiclass` (softmax transform)
- [x] Support `multiclassova` (per-class sigmoid)
- [x] Handle multiple output columns for multiclass

### 2.5 Fit Formula Tests ✅

- [x] Test regression predictions match `predict(model)`
- [x] Test binary classification predictions match
- [x] Test multiclass predictions match
- [x] Test with missing values in prediction data
- [x] Test formula structure is correct (returns language object)
- [x] Test `tidypredict_sql()` generates valid SQL
- [x] Test with SQLite backend
- [x] Test unsupported objective throws error
- [x] Test tidypredict_fit works on both model and parsed model

---

## Phase 3: Categorical Feature Support ✅

### 3.1 Parsing Categorical Splits ✅

- [x] Research LightGBM categorical split encoding
- [x] Remove error for `decision_type = "=="`
- [x] Parse categorical threshold/bitmap information (`"0||1||3"` format)
- [x] Generate `type = "set"` conditions with `op = "in"` / `op = "not-in"`
- [x] Handle `default_left` for categorical splits

### 3.2 Categorical Fit Formula ✅

- [x] Verify `path_formula()` in `R/tree.R` handles `type = "set"` (already supported)
- [x] Test categorical conditions generate correct SQL
- [x] Handle category encoding (integer values from LightGBM)

### 3.3 Categorical Tests ✅

- [x] Test categorical feature parsing
- [x] Test categorical predictions match LightGBM
- [x] Test mixed numerical + categorical features
- [x] Test SQL generation with categorical IN clauses

---

## Phase 4: Additional Features

### 4.1 Model Serialization ✅

- [x] Test parsed model save/load with YAML
- [x] Test predictions match after reload (with tolerance for YAML precision)
- [x] Test multiclass serialization

### 4.2 tidypredict_test Support ✅

- [x] Implement `tidypredict_test.lgb.Booster()`
- [x] Test comparison output format
- [x] Test threshold parameter
- [x] Test max_rows parameter
- [x] Test error for multiclass (not supported)
- [x] Test error when matrix not provided

### 4.3 parsnip Integration ✅

- [x] Test models fitted via parsnip
- [x] Test `boost_tree() %>% set_engine("lightgbm")`
- [x] Test regression and binary classification
- [x] Test tidypredict_sql with parsnip model
- [x] Test tidypredict_test with parsnip model
- [x] Document parsnip workflow (in vignette)

### 4.4 Documentation ✅

- [x] Add LightGBM to DESCRIPTION description text
- [x] Create LightGBM vignette
- [x] Add LightGBM example to README
- [x] Document supported objectives
- [x] Document limitations (unsupported objectives, etc.)

### 4.5 orbital Package Support ✅

- [x] Implement `.extract_lgb_trees()` for orbital
- [x] Test tree extraction
- [x] Test error on invalid model type

---

## LightGBM Objectives Reference

### Regression
| Objective | Transform | Status |
|-----------|-----------|--------|
| `regression` / `regression_l2` | identity | ✅ |
| `regression_l1` | identity | ✅ |
| `huber` | identity | ✅ |
| `fair` | identity | ✅ |
| `poisson` | `exp()` | ✅ |
| `gamma` | `exp()` | ✅ |
| `tweedie` | `exp()` | ✅ |
| `quantile` | identity | ✅ |
| `mape` | identity | ✅ |

### Classification
| Objective | Transform | Status |
|-----------|-----------|--------|
| `binary` | sigmoid | ✅ |
| `cross_entropy` | sigmoid | ✅ |
| `multiclass` | softmax | ✅ |
| `multiclassova` | per-class sigmoid | ✅ |

### Ranking (Low Priority)
| Objective | Transform | Status |
|-----------|-----------|--------|
| `lambdarank` | TBD | ⬜ |
| `rank_xendcg` | TBD | ⬜ |

---

## Operator Mapping Reference

| Our Operator | Condition | With `missing=TRUE` |
|--------------|-----------|---------------------|
| `"less-equal"` | `col <= val` | `(col <= val OR IS NULL)` |
| `"more"` | `col > val` | `(col > val OR IS NULL)` |

---

## Key Differences from XGBoost

| Aspect | XGBoost | LightGBM |
|--------|---------|----------|
| Node ID | Single `Node` column | Separate `split_index`/`leaf_index` |
| Parent tracking | `Yes`/`No`/`Missing` child refs | `node_parent`/`leaf_parent` |
| Split direction | Implicit | Explicit via row ordering |
| Missing values | `Missing` column | `default_left` column |
| Leaf values | `Gain` or `Quality` | `leaf_value` |
| Categorical | Not native | Native with `decision_type = "=="` |

---

## Files Reference

| File | Purpose | Status |
|------|---------|--------|
| `R/model-lightgbm.R` | Parser + fit formula | ✅ |
| `R/predict-fit.R` | S3 method registration | ✅ |
| `tests/testthat/test-model-lightgbm.R` | Tests | ✅ (222 tests) |
| `DESCRIPTION` | Package metadata | ✅ |
| `NAMESPACE` | Exports | ✅ |
| `vignettes/lightgbm.Rmd` | Documentation | ⬜ |
