# tidypredict (development version)

## New Model Supports

Added support for the following model classes, and for the parsnip model types and engines that fit them.

### Boosting

- `mboost::blackboost()` gradient boosting, via `boost_tree()` with the `"mboost"` engine. (#249)
  - Only the `Gaussian()` family is supported.
- `dbarts::bart()` Bayesian additive regression trees, via `bart()` with the `"dbarts"` engine. (#268)
  - The model has to be fit with `keeptrees = TRUE`, and only continuous outcomes are supported since binary outcomes are fit with a probit link.

### Decision trees and rule-based models

- `C50::C5.0()` classification trees, including rule-based ones (`rules = TRUE`), via `decision_tree()`, `boost_tree()` and `C5_rules()` with the `"C5.0"` engine. (#245, #248, #251)
  - Boosted models (`trials > 1`) combine trials by confidence-weighted voting, but boosted rule-based models are not supported.
  - Fuzzy thresholds (`fuzzyThreshold = TRUE`) and cost matrices (`costs`) are not supported.
- `xrf::xrf()` rule-based models (RuleFit), via `rule_fit()` with the `"xrf"` engine. (#256)
  - Regression (`family = "gaussian"`) and binary classification (`family = "binomial"`) only; multinomial models are not supported.

### Discriminant analysis

- `MASS::lda()` linear discriminant analysis, via `discrim_linear()` with the `"MASS"` engine. (#258)
- `MASS::qda()` quadratic discriminant analysis, via `discrim_quad()` with the `"MASS"` engine. (#271)
- `mda::fda()` flexible discriminant analysis, via `discrim_linear()` with the `"mda"` engine. (#259)
  - Only linear regression methods: `mda::polyreg()` with `degree = 1`, or `mda::gen.ridge()`.
- `sda::sda()` shrinkage discriminant analysis, via `discrim_linear()` with the `"sda"` engine. (#260)
- The regularized linear discriminant analysis models in `sparsediscrim` (`lda_diag()`, `lda_shrink_mean()`, `lda_shrink_cov()` and `lda_emp_bayes_eigen()`), via `discrim_linear()` with the `"sparsediscrim"` engine. (#261)

### Forests and bagged ensembles

- `partykit::cforest()` random forests, via `rand_forest()` with the `"partykit"` engine. (#246)
  - Regression only.
- `aorsf::orsf()` oblique random forests, via `rand_forest()` with the `"aorsf"` engine. (#247)
  - Regression only, and only numeric predictors.
- `baguette::bagger()` bagged tree ensembles fit with the `"CART"` or `"C5.0"` base model, via `bag_tree()` with the `"rpart"` or `"C5.0"` engine. (#269, #270)

### H2O

- H2O gradient boosting models (`H2ORegressionModel`, `H2OBinomialModel` and `H2OMultinomialModel`), via `boost_tree()` with the `"h2o_gbm"` engine. (#250)
  - Only GBM models, not H2O's XGBoost, and only the gaussian, bernoulli and multinomial distributions.
- H2O RuleFit models (`h2o::h2o.rulefit()`), via `rule_fit()` with the `"h2o"` engine. (#257)
  - Regression and binary classification only.

### Naive Bayes

Both are supported when fit without kernel density estimates (`usekernel = FALSE`), and return a named list of class-probability expressions.

- `klaR::NaiveBayes()` with Gaussian densities, via `naive_Bayes()` with the `"klaR"` engine. (#264)
- `naivebayes::naive_bayes()`, via `naive_Bayes()` with the `"naivebayes"` engine. (#266)
  - Gaussian, categorical, Bernoulli and Poisson conditional distributions.

### Neural networks and multinomial regression

- `nnet::nnet()` single hidden layer neural networks, via `mlp()` with the `"nnet"` engine. (#267)
- `nnet::multinom()` multinomial log-linear models, via `multinom_reg()` with the `"nnet"` engine. (#255)

### Support vector machines

- `kernlab::ksvm()` linear support vector machines (`vanilladot` kernel), via `svm_linear()` with the `"kernlab"` engine, for regression and binary classification. (#252)
  - Non-linear kernels and multiclass classification are not supported, and classification requires a probability model (`prob.model = TRUE`).
- `LiblineaR::LiblineaR()` support vector machines, via `svm_linear()` with the `"LiblineaR"` engine, for regression (`type` 11, 12, 13) and binary classification (`type` 1-5). (#253)
  - Classification returns the SVM decision value rather than a probability.
- `LiblineaR::LiblineaR()` binary logistic regression (`type` 0, 6, 7), via `logistic_reg()` with the `"LiblineaR"` engine. (#243)

### Other models

- The partial least squares models in `mixOmics` (`pls()`, `spls()`, `plsda()` and `splsda()`), via `pls()` with the `"mixOmics"` engine, for regression and classification. (#262)
- `quantreg::rq()` quantile regression, via `linear_reg()` with the `"quantreg"` engine. (#241)
- `parsnip::nullmodel()`, via `null_model()` with the `"parsnip"` engine. (#263)

## Improvements

### Model support

- Added support for multinomial `glmnet::glmnet()` models (`family = "multinomial"`), including `multinom_reg()` parsnip models fitted with the `"glmnet"` engine. `tidypredict_fit()` returns a named list of class-probability expressions (softmax). (#198, #254)

- Added support for `decision_tree()` parsnip models fitted with the `"rpart"` engine. (#244)

- Added support for `linear_reg()` parsnip models fitted with the `"glm"` engine. (#239)

- `tidypredict_fit()` now supports `C50::C5.0()` models that split a discrete predictor into one branch per level. (#245)

### New functions

- `tidypredict_save()` and `tidypredict_load()` write a parsed model to a YAML file and read it back. Use them instead of `yaml::write_yaml()`, which stores only 7 significant digits by default and so rounds split thresholds enough to send rows down a different branch when the model is re-loaded. (#307)

### Error messages and input validation

- `acceptable_formula()`, `parse_model()`, `tidypredict_fit()` and `tidypredict_interval()` now report a model class they do not support with a message naming the class, rather than failing with R's "no applicable method" error. `tidypredict_interval()` gives a parsed model the same message it gives a fitted one, instead of "Model type not supported.". (#313)

- `as_parsed_model()` now rejects an object that is not a parsed model. (#313)

- `tidypredict_interval()` now rejects an `interval` that is not a single number strictly between 0 and 1.(#313)

- `tidypredict_interval()` now reports a list that is not a parsed model, (#308, #313)

- `tidypredict_sql()` and `tidypredict_sql_interval()` now check that dbplyr is installed before using it. (#314)

- `tidypredict_to_column()` now validates `vars`, `add_interval` and `interval`. (#313)

- `tidypredict_to_column()` now explains that a model returning more than one formula is unsupported, instead of incorrectly claiming that tree based models are unsupported. (#279)

### Documentation

- New articles for `kernlab::ksvm()`, `mboost::blackboost()` and `xrf::xrf()`, and the model list menu now links to the `LiblineaR` and `quantreg` sections directly. (#317)

- The lm, glm, ranger, randomForest and rpart articles recommended `tidypredict_to_sql()`, which does not exist. They now point at `tidypredict_sql()`. (#317)

- The Cubist article now documents two limits on how closely `tidypredict_fit()` can match `Cubist::predict()`. The instance-based correction that `predict()` applies when `neighbors` is greater than zero is not reproduced, because it adjusts each prediction using training rows that are not part of the fitted model. Separately, Cubist stores its coefficients as 32-bit floats, so the agreement has a relative ceiling near 1e-7 rather than an absolute one, and an outcome on a large scale leaves a proportionally large absolute difference. (#375)

- The glm article now documents the one inverse link `tidypredict_fit()` does not reproduce exactly: `probit`, whose inverse is `pnorm()`, is written as the Bowling et al. logistic approximation to the normal CDF because no SQL backend has a normal CDF. It costs about 1e-4 of probability. (#355)

- The naive Bayes article now documents the one case where `tidypredict_fit()` does not reproduce `predict()` for `klaR::NaiveBayes()` and `naivebayes::naive_bayes()` models: both replace a normal density that underflowed to zero with their `threshold` argument, which takes a value roughly 38 standard deviations from the class mean, and the log scale used throughout never underflows. (#300)

- The models article now documents a limit on `kernlab::ksvm()` models fitted through the matrix interface, `ksvm(x, y)`. `ksvm()` mangles its model matrix column names with `make.names()` and keeps no record of the originals, and unlike the formula interface there is no `terms` object to detect this against, so a non-syntactic column name such as `a:b` yields a formula referring to a column the data does not have. This cannot be caught automatically, because every name `make.names()` produces is also a name it leaves alone, so `a.b` from a mangled `a:b` is indistinguishable from a correct model with a column genuinely named `a.b`. (#418)

## Bug Fixes

- `.build_case_when_tree()`, which {orbital} calls, now returns the bare prediction of a classification stump, instead of the `case_when(.default = "a")` that dplyr rejects. (#310)

- `acceptable_formula()` now checks the contrast of every factor predictor, not just one, and names the offending field. A model mixing contrasts was accepted and then silently mis-parsed. (#291)

- `acceptable_formula()` no longer rejects a `MASS::lda()`, `MASS::qda()` or `earth::earth()` fit whose factor has a level containing a colon, which the contrast check read as an interaction. (#391)

- `set_catboost_categories()` now takes its hashes from CatBoost's own hash function, so it names every category of any factor. A factor with four or more levels errored with "No category mapping found for hash", and a two-level one could be named the wrong way round. (#297)

- `tidypredict_fit()` now works on a LightGBM model whose trees are bare leaves, which failed with "Model has no trees.". A multiclass model with only some bare leaves also assigned trees to the wrong classes. (#401)

- `tidypredict_fit()` now returns one prediction per row for a `ranger::ranger()`, `xgboost`, `baguette::bagger()` or `xrf::xrf()` model whose formula mentions no column, such as an all-stump forest or an intercept-only lasso. The value was correct, its length was not. (#397)

- `tidypredict_fit()` now applies the bias correction of a `randomForest::randomForest()` model fitted with `corr.bias = TRUE`, which left predictions off by as much as 0.21 for a model of `mpg` on `mtcars`. (#395)

- `tidypredict_fit()` now sends a split threshold that is not finite, or that overflows the 32-bit float range, down the branch the model does, rather than turning it into a `NaN` that fails every comparison. (#313)

- `tidypredict_fit()` now works on a parsed LightGBM model fit with `linear_tree = TRUE`, which failed with "`..1 (right)` must be a vector, not `NULL`". (#346)

- `tidypredict_fit()` now follows the per-node missing value direction a `ranger::ranger()` model learns under the `na.action = "na.learn"` default of ranger 0.17.0, instead of always sending them left. (#394)

- `tidypredict_fit()` now matches `predict()` for a `ranger::ranger()` model fitted with `respect.unordered.factors = "partition"` on a factor with more than 31 levels, whose split mask `ranger::treeInfo()` silently blanks out. (#414)

- `tidypredict_fit()` no longer returns `NULL` for a parsed model saved by tidypredict 1.0.1 or earlier that came from a `partykit` or `rpart` single tree. Any parsed model type that is still unhandled now raises an error. (#304)

- `tidypredict_fit()` no longer fails with "`x` must be a formula" on a parsed model saved by tidypredict 1.0.1 or earlier that contains a `ranger::ranger()` or `randomForest::randomForest()` stump. (#310)

- `tidypredict_fit()` now handles three parsed model shapes that only a hand-written or edited parsed model contains: a path mixing a `type = "all"` element with real conditions, a rule whose linear prediction is a single non-intercept term, and a rule whose terms are all zero. (#310)

- `tidypredict_fit()` now assigns rules to the right committee for `Cubist::cubist()` models fitted with more than 20 committees, where the printed model it read the counts from truncates them. (#286)

- `tidypredict_fit()` now applies the per-rule extrapolation limits of `Cubist::cubist()` models, which hold each rule to the span of the training outcomes it covers. This engages on rows of the training data too. (#285)

- `tidypredict_fit()` now supports factor predictors for `Cubist::cubist()` models, which previously produced a formula that could not be evaluated (`object '"f"' not found`). (#322)

- `tidypredict_fit()` now reads the coefficient labels of an `lm()`, `glm()` or `quantreg::rq()` model from the model's own term structure. A factor level containing a `:` was taken apart as an interaction, and a label equal to another predictor's name was read as that predictor. (#308)

- `tidypredict_fit()` now rejects an `earth::earth()` model fit with a contrast other than the treatment one. An ordered factor gave a formula comparing the column against contrast values such as `-0.2236`. (#323)

- `tidypredict_fit()` now routes missing values by each node's `missing_type` for `lightgbm` models. Consulting `default_left` alone was wrong for every model trained without missing data, which is the common case. (#288)

- `tidypredict_fit()` now honors `zero_as_missing` for `lightgbm` models, where an exact zero takes the same branch as a missing value. Predictions were wrong on the training data itself. (#288)

- `tidypredict_fit()` no longer sends a missing value down the left branch of a categorical split for `lightgbm` models. LightGBM sends it right whatever `default_left` says. (#288)

- `tidypredict_fit()` no longer returns `NaN` for every class probability of a row whose class scores are large, for any model whose prediction is a softmax. `exp(s) / sum(exp(s))` overflows once a score passes about 710, and is now written as the equivalent `1 / sum(exp(s_j - s_k))`. (#299)

- `tidypredict_fit()` now rejects a `glmnet` model fit with an `offset`, whose values glmnet never records, rather than silently dropping it and predicting wrong by its size. (#296)

- `tidypredict_fit()` now rejects a `ranger::ranger()` probability or survival forest, read from `treetype`, instead of emitting `case_when(x <= 0.0066 ~ NULL, .default = NULL)`. (#301)

- `tidypredict_fit()` now sends a value sitting exactly on a split boundary the way the model does, for the backends that compare split thresholds as 32-bit floats: `xgboost`, `lightgbm`, `catboost`, `Cubist::cubist()` and `C50::C5.0()`. About half of all thresholds round that tie towards the neighbouring float. (#350)

- `tidypredict_fit()` now honors `sigmoid` for `lightgbm` models fit with the `binary` or `multiclassova` objective. Every probability of a model fit with any other value was rescaled. (#288)

- `tidypredict_fit()` now honors `reg_sqrt` for `lightgbm` models, whose predictions were left on the square-root scale. (#288)

- `tidypredict_fit()` and `parse_model()` now work on an `xgboost` booster that has been saved and reloaded with `xgb.save()` / `xgb.load()`, which failed with `argument "model" is missing, with no default`. The objective is now recovered from the saved model, which a reloaded booster records nowhere else. (#292)

- `tidypredict_fit()` now works for rank-deficient `lm()` and `glm()` models, which aborted with "Unable to calculate inverse of QR decomposition". A duplicated predictor column or one with no variance is enough to hit it, and `tidypredict_interval()` keeps working for these models. (#308)

- `tidypredict_fit()` now supports splits with more than two branches for `partykit` models, such as those from `ctree_control(multiway = TRUE)`. Every branch after the second was dropped. (#295)

- `tidypredict_fit()` now honors `partysplit(right = FALSE)` for `partykit` models, where a value falling exactly on the break took the wrong branch. (#295)

- `tidypredict_fit()` now handles ordered factor predictors for `partykit` models, which previously errored with "Result must be length 1, not 2". (#295)

- `tidypredict_fit()` no longer swaps the two branches of every `partykit::party` converted from an `rpart` model. (#295)

- `tidypredict_fit()` now decodes factor splits for `ranger::ranger()` models, in all three `respect.unordered.factors` modes and for ordered factors, rather than comparing the split value as a numeric threshold. (#283)

- `tidypredict_fit()` now decodes factor splits for `randomForest::randomForest()` models, where an unordered factor's split point is a bit mask and an ordered factor's is a level code, rather than reading either as a numeric threshold. (#282)

- `tidypredict_fit()` and `parse_model()` now handle a stump in a `randomForest::randomForest()` forest, instead of aborting with "argument of length 0". A constant outcome or a zero-variance predictor makes one routine. (#362)

- `tidypredict_fit()` now substitutes the training mean for a missing predictor in `Cubist::cubist()` models, matching `predict()`, in the rule conditions as well as the linear models. (#294)

- `tidypredict_fit()` now sends a missing predictor down the left branch for `ranger::ranger()` models, matching `predict()`. (#294)

- `tidypredict_fit()` now routes missing values through surrogate splits for `rpart::rpart()` models, and for `baguette::bagger()` models using the `"CART"` base model, in all three `usesurrogate` modes, instead of sending them right. (#294)

- `tidypredict_fit()` now returns `NA` for a row that reaches a split on a predictor it is missing, for `partykit::ctree()`, `partykit::cforest()` and `mboost::blackboost()` models. These resolve a missing value by random sampling, so `predict()` returns a different answer on each call. (#294)

- `tidypredict_fit()` now returns `NA` for a row with a missing predictor for `randomForest::randomForest()` and `aorsf::orsf()` models, neither of which will predict from an incomplete row. Rows are kept rather than dropped. (#294, #325)

- `tidypredict_fit()` now returns correct predictions for `catboost` models whose predictor values fall on a split border, which catboost compares as 32-bit floats. (#298)

- `tidypredict_fit()` now picks the right factor predictor when three or more variable names are nested prefixes of one another, such as `x`, `xy` and `xyz`. The wrong variable was silently chosen for `lm()`, `glm()`, `quantreg::rq()`, `nnet::multinom()`, `nnet::nnet()` and `earth::earth()`. (#290)

- `tidypredict_fit()` now uses a strict inequality (`<`) for the continuous splits of `rpart::rpart()` models, matching how `rpart` assigns values exactly equal to a cut point. (#232)

- `tidypredict_fit()` now returns correct predictions for `randomForest::randomForest()` models saved and reloaded with `parse_model()` and `as_parsed_model()`, which named every split variable after the first leaf of a tree incorrectly. (#232)

- `tidypredict_fit()` now returns correct predictions for `Cubist::cubist()` models whose predictor values fall exactly on a split threshold, which Cubist compares as 32-bit floats. (#232)

- `tidypredict_fit()` now keeps small probabilities for models with a logit link, such as `glm()` with `family = binomial` and `LiblineaR::LiblineaR()`. The inverse link rounded to exactly 0 once the linear predictor fell below about -37. (#232)

- `tidypredict_fit()` now returns correct predictions for xgboost models whose feature values fall exactly on a split threshold, which xgboost compares as 32-bit floats. (#45)

- `tidypredict_fit()` now returns correct predictions for xgboost models that have been saved and reloaded with `parse_model()` and `as_parsed_model()`. Previously every tree collapsed to a single leaf value. (#232)

- `tidypredict_interval()` now works for `glm()` models, which returned `numeric(0)` for every gaussian glm because the residual variance was read from `summary()$sigma`, which only `summary.lm()` has. (#293)

- `tidypredict_interval()` now honors its `interval` argument, which was hardcoded to 0.95. `tidypredict_to_column(add_interval = TRUE)` and `tidypredict_sql_interval()` were affected too. (#232)

- `tidypredict_sql()` now returns a single query for an intercept-only model, whose bare-number formula was mistaken for the list a multiclass model produces. (#313)

## Breaking Changes

- `tidypredict_fit()` now returns predictions on the response scale for CatBoost models fit with the `Poisson` or `Tweedie` objective, applying `exp()` to the raw score as the other CatBoost objectives already invert their own links. Anyone using such a model will see their predictions change from the log scale to the count or mean scale; they now match `catboost.predict(prediction_type = "Exponent")` instead of the `"RawFormulaVal"` default. (#356)

## Developer

- New generics expose the pieces `tidypredict_fit()` is assembled from, so that packages generating their own code from a fitted model can reuse tidypredict's parsing: `tidypredict_trees()` returns per-tree expressions, `tidypredict_class_trees()` returns them for each outcome level, `tidypredict_class_exprs()` returns one finished expression per outcome level, and `tidypredict_n_trees()` returns the number of trees. See `?tidypredict_extractors`. (#433)

- `tidypredict_combine_trees()` is a new generic that turns per-tree expressions back into a model's prediction, with methods for `randomForest`, `ranger`, xgboost, LightGBM, CatBoost, `cforest`, `blackboost` and `aorsf`. Summing or averaging the trees, as the shape of the list invites, is wrong for any backend carrying an offset, a scale or a link. (#436)

- `tidypredict_trees()` and `tidypredict_n_trees()` gain methods for `partykit::cforest()`, `mboost::blackboost()` and `aorsf::orsf()`. (#436)

- Boosted `C50::C5.0()` models deliberately have no `tidypredict_trees()` method, and `tidypredict_combine_trees()` refuses them with an explanation. Their trials vote with a class label and a confidence rather than contributing numbers, so there is nothing to sum or average. (#436)

- `?tidypredict_extractors` now documents which model classes implement each of the extractor generics, and what to implement when adding a new one. The three seam topics cross-reference each other. (#436)

- New generics describe what a model's fitted expressions compute, which the expressions themselves do not say: `tidypredict_output_type()` returns one of `"numeric"`, `"prob"`, `"decision"` or `"class"`, `tidypredict_outcome_levels()` returns the outcome levels in model order, and `tidypredict_normalized()` reports whether per-level probabilities already sum to one. None of it is recoverable from the shape of the result: a `LiblineaR` SVM classifier and a `LiblineaR` logistic regression both return a single expression, but only the second is a probability. See `?tidypredict_metadata`. (#433, #435)

- `tidypredict_class_exprs()` on a `partykit` model is named by outcome level. The `.extract_partykit_classprob()` it replaces returned an unnamed list, which left callers assuming its order matched `levels()` of the outcome. (#433)

- The error raised when no method knows how to handle a model at all now carries the condition class `tidypredict_unsupported_model`, so a wrapper such as orbital can tell it apart from the many errors reporting an unsupported *configuration* of a model that is otherwise handled. (#432)

# tidypredict 1.1.1

- `parse_model()` and `tidypredict_fit()` now detect xgboost dropout (DART) models from the saved dropout weights rather than the serialised booster name, so they keep applying `weight_drop` with xgboost 3.4.0 and later, which canonicalises `booster = "dart"` to `"gbtree"`. (#238)

# tidypredict 1.1.0

## New Model Supports

- Added support for rpart decision tree models (`rpart`). (#226)

- Added support for CatBoost models (`catboost.Model`). (#179, #187, #188)
  - Objectives: RMSE, MAE, Quantile, MAPE, Poisson, Huber, LogCosh, Expectile, Tweedie, Logloss, CrossEntropy, MultiClass, and MultiClassOneVsAll.
  - Tree types: oblivious (default `SymmetricTree`) and non-oblivious (`Depthwise` or `Lossguide` grow policy).
  - Categorical features are handled automatically for parsnip/bonsai models; for raw CatBoost models use `set_catboost_categories()`.

- Added support for LightGBM models (`lgb.Booster`). (#177, #186)
  - Objectives: regression, binary classification, and multiclass classification.
  - Supports categorical features.
  - Supports linear trees (`linear_tree = TRUE`), which fit a linear model at each leaf instead of a constant.

## Improvements

- Tree models (rpart, partykit, ranger, randomForest, xgboost, lightgbm, catboost) now generate nested `case_when()` expressions that mirror the tree structure, instead of flat expressions with all leaf conditions at the same level. This produces more efficient SQL and R code because conditions are evaluated hierarchically. (#227)

- `parse_model()` now documents the parsed model version system (v1/v2/v3) and model type classes in its help page. (#227)

- `earth()` models now support additional GLM families and link functions: Gamma, inverse.gaussian, probit, and cloglog. (#194, #195)

- `glm()` models now support additional families and link functions: Gamma family with inverse link, inverse.gaussian family with 1/mu^2 link, probit link, cloglog link, and sqrt link. (#203, #204, #205, #206, #207)

- `glmnet()` models now support `Gamma` family and Cox proportional hazards (`family = "cox"`) models. (#200, #201)

- xgboost support now includes additional objectives: `binary:hinge`, `reg:absoluteerror`, `reg:gamma`, `reg:pseudohubererror`, and `reg:squaredlogerror`. (#184)

- Added a vignette on floating-point precision issues with tree-based models. (#231)

## Bug Fixes

- `tidypredict_fit()` now correctly handles xgboost models with stump trees (single leaf, no splits). (#182)

- `tidypredict_fit()` now correctly handles xgboost DART booster models with `rate_drop > 0`. DART uses tree weight normalization during training, and these weights are now properly applied to each tree's predictions. (#183)

- `tidypredict_fit()` now correctly incorporates `base_score` for xgboost models with `count:poisson` and `reg:tweedie` objectives. Previously, predictions were incorrect when `base_score` was not the default value. (#184)

- `tidypredict_fit()` now correctly averages tree predictions for LightGBM models with `boosting="rf"` instead of summing them. (#185)

- `tidypredict_fit()` now uses the correct split operator (`<=` instead of `<`) for ranger models. Previously, predictions were incorrect when data values exactly matched split values. (#189)

- `tidypredict_fit()` now correctly averages tree predictions for ranger models instead of summing them. Previously, predictions were `num.trees` times too large. (#190)

- `tidypredict_fit()` now throws a clear error for ranger and randomForest classification models, which are not supported. (#191, #193)

- `tidypredict_fit()` now uses the correct split operator (`<=` instead of `<`) for randomForest models. (#192)

- `tidypredict_fit()` now correctly handles partykit stump trees (models with no splits). (#196)

- `tidypredict_fit()` now works with `glmnet()` models that use family function syntax (e.g., `family = gaussian()`) instead of string syntax (e.g., `family = "gaussian"`). (#197)

- `tidypredict_fit()` now works with models that use family function syntax (e.g., `family = gaussian()`) instead of string syntax (e.g., `family = "gaussian"`). (#202)

# tidypredict 1.0.1

## Bug Fixes

- Fixed bug where `base_score` wasn't extracted correctly xgboost for version 3 or higher. (#173)

# tidypredict 1.0.0

## Breaking Changes

- Random forest implementations (ranger and randomForest) will now produce a single formula instead of a list of expressions. (#84)

## New Model Supports

- Added support for glmnet models. (#165)

## Improvements

- xgboost models with objectives `"reg:tweedie"` and `"count:poisson"` are now supported. (#72, @SimonCoulombe)

- tree based models now uses `.default` argument in produced `case_when()` code when applicable. (#153)

- Speed up `tidypredict_fit()` for partykit and ranger packages. (#125)

- Speed up `tidypredict_fit()` for xgboost models. (#130)

- randomForest models now support regression outcomes. (#77)

- An informative error will now be thrown if a lm model cannot be processed due to having linear combinations of predictors. (#124)

- linear models such as `lm()` and `glm()` now work with interactions created with `*` and `:`. (#74) 

- Cubist rules will return simplified rules whenever possible to avoid multiplying by 0 and 1. (#152)

- Make work with xgboost version > 2.0.0.0. (#169)

## Bug Fixes

- Fixed a bug where the intercept was added incorrectly to the result for cubist models. (#58)

- Fixed bug where tidypredict would error on Cubist models without conditions. (#127)

- Fixed bug where Cubist models incorrectly combined rules and committees. (#134)

# tidypredict 0.5.1

- Exported a number of internal functions to be used in {orbital} package

# tidypredict 0.5

- Changes maintainer to Edgar Ruiz

- Updates author's email addresses.

- Removes dependency with `stringr`

- Fixes issue with `earth` parsed_models (#108)

- Addresses issues with XGBoost models

- Improvements to XGBoosts tests

# tidypredict 0.4.9

- Fixes issue handling GLM Binomial earth models (#97)

- Adds capability to handle single simple Cubist models (#57)

- Fixed parenthesis issue in the creation of the interval formula (#76)

- Fixed bug in SQL query generation for XGBoost models with objective `binary:logistic`.

- Re-licensed package from GPL-3 to MIT. See [consent from copyright holders here](https://github.com/tidymodels/tidypredict/issues/95).

# tidypredict 0.4.8

- CRAN submission for a broken test case. 

# tidypredict 0.4.7

- Changed to work with version 5.1.2 and above of the `earth` package. As a result, `tidypredict` will only parse objects created by this and later versions of `earth`. 

# tidypredict 0.4.6

- Small release for `xgboost` changes. 

# tidypredict 0.4.5

- Switches maintainer to Max Kuhn

# tidypredict 0.4.3

- Adds support for categorical predictors in `partykit`

- Fixes `parsnip` tests to meet standards of new CRAN version

# tidypredict 0.4.2

- Simplifies tests that verify `ranger` 

- Adds fit method for parsed `xgboost` models

- Sets conditional requirement for `xgboost`, for test and vignette 

# tidypredict 0.4.0

## New features

- Parses `ranger` classification models.

- Adds method support for `broom`'s `tidy()` function.  Regression models only

- Adds `as_parsed_model()` function. It adds the proper class components to the list.

- Adds initial support for `partykit`'s `ctree()` model

- Adds support for `parsnip` fitted models: `lm`, `randomForest`, `ranger`, and `earth`

- Adds support for xgb.Booster models provided by the `xgboost` package (@Athospd, #43)

- Adds support for `Cubist::cubist()` models (# 36)

# tidypredict 0.3.0

## New features

- Adds support for MARS models provided by the `earth` package

## Improvements

- New parsed models are now list objects as opposed to data frames.

- tidypredict_to_column() no longer supports `ranger` and `randomForest` because of the multiple queries generated by multiple trees.

- All functions that read the parsed models and create the tidy eval formula now use the list object.  

- Most of the code that depends on dplyr programming has been removed.

- Removes dependencies on: tidyr, tibble

- The `x/y` interface for `earth` models can now be used. 

## Bug Fixes

- It now returns all of the trees instead of just one for tree based models (`randomForest` & `ranger`) (#29)

# tidypredict 0.2.1

## Bug Fixes

- tibble 2.0.0 compatibility fix (@krlmlr)

# tidypredict 0.2.0

## New features

- Add support for `ranger()` models.

## Bug fixes

- Using `x ~.` in a randomForest() formula fails (#18 @washcycle).
