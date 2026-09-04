# Changelog

## tidypredict 1.2.0

### New Model Supports

Added support for the following model classes, and for the parsnip model
types and engines that fit them.

#### Boosting

- [`mboost::blackboost()`](https://rdrr.io/pkg/mboost/man/blackboost.html)
  gradient boosting, via
  [`boost_tree()`](https://parsnip.tidymodels.org/reference/boost_tree.html)
  with the `"mboost"` engine.
  ([\#249](https://github.com/tidymodels/tidypredict/issues/249))
  - Only the [`Gaussian()`](https://rdrr.io/pkg/mboost/man/Family.html)
    family is supported.
- [`dbarts::bart()`](https://rdrr.io/pkg/dbarts/man/bart.html) Bayesian
  additive regression trees, via
  [`bart()`](https://parsnip.tidymodels.org/reference/bart.html) with
  the `"dbarts"` engine.
  ([\#268](https://github.com/tidymodels/tidypredict/issues/268))
  - The model has to be fit with `keeptrees = TRUE`, and only continuous
    outcomes are supported since binary outcomes are fit with a probit
    link.

#### Decision trees and rule-based models

- [`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html)
  classification trees, including rule-based ones (`rules = TRUE`), via
  [`decision_tree()`](https://parsnip.tidymodels.org/reference/decision_tree.html),
  [`boost_tree()`](https://parsnip.tidymodels.org/reference/boost_tree.html)
  and
  [`C5_rules()`](https://parsnip.tidymodels.org/reference/C5_rules.html)
  with the `"C5.0"` engine.
  ([\#245](https://github.com/tidymodels/tidypredict/issues/245),
  [\#248](https://github.com/tidymodels/tidypredict/issues/248),
  [\#251](https://github.com/tidymodels/tidypredict/issues/251))
  - Boosted models (`trials > 1`) combine trials by confidence-weighted
    voting, but boosted rule-based models are not supported.
  - Fuzzy thresholds (`fuzzyThreshold = TRUE`) and cost matrices
    (`costs`) are not supported.
- [`xrf::xrf()`](https://rdrr.io/pkg/xrf/man/xrf.html) rule-based models
  (RuleFit), via
  [`rule_fit()`](https://parsnip.tidymodels.org/reference/rule_fit.html)
  with the `"xrf"` engine.
  ([\#256](https://github.com/tidymodels/tidypredict/issues/256))
  - Regression (`family = "gaussian"`) and binary classification
    (`family = "binomial"`) only; multinomial models are not supported.

#### Discriminant analysis

- [`MASS::lda()`](https://rdrr.io/pkg/MASS/man/lda.html) linear
  discriminant analysis, via
  [`discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.html)
  with the `"MASS"` engine.
  ([\#258](https://github.com/tidymodels/tidypredict/issues/258))
- [`MASS::qda()`](https://rdrr.io/pkg/MASS/man/qda.html) quadratic
  discriminant analysis, via
  [`discrim_quad()`](https://parsnip.tidymodels.org/reference/discrim_quad.html)
  with the `"MASS"` engine.
  ([\#271](https://github.com/tidymodels/tidypredict/issues/271))
- [`mda::fda()`](https://rdrr.io/pkg/mda/man/fda.html) flexible
  discriminant analysis, via
  [`discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.html)
  with the `"mda"` engine.
  ([\#259](https://github.com/tidymodels/tidypredict/issues/259))
  - Only linear regression methods:
    [`mda::polyreg()`](https://rdrr.io/pkg/mda/man/polyreg.html) with
    `degree = 1`, or
    [`mda::gen.ridge()`](https://rdrr.io/pkg/mda/man/gen.ridge.html).
- [`sda::sda()`](https://rdrr.io/pkg/sda/man/sda.html) shrinkage
  discriminant analysis, via
  [`discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.html)
  with the `"sda"` engine.
  ([\#260](https://github.com/tidymodels/tidypredict/issues/260))
- The regularized linear discriminant analysis models in `sparsediscrim`
  (`lda_diag()`, `lda_shrink_mean()`, `lda_shrink_cov()` and
  `lda_emp_bayes_eigen()`), via
  [`discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.html)
  with the `"sparsediscrim"` engine.
  ([\#261](https://github.com/tidymodels/tidypredict/issues/261))

#### Forests and bagged ensembles

- [`partykit::cforest()`](https://rdrr.io/pkg/partykit/man/cforest.html)
  random forests, via
  [`rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
  with the `"partykit"` engine.
  ([\#246](https://github.com/tidymodels/tidypredict/issues/246))
  - Regression only.
- [`aorsf::orsf()`](https://docs.ropensci.org/aorsf/reference/orsf.html)
  oblique random forests, via
  [`rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
  with the `"aorsf"` engine.
  ([\#247](https://github.com/tidymodels/tidypredict/issues/247))
  - Regression only, and only numeric predictors.
- [`baguette::bagger()`](https://baguette.tidymodels.org/reference/bagger.html)
  bagged tree ensembles fit with the `"CART"` or `"C5.0"` base model,
  via
  [`bag_tree()`](https://parsnip.tidymodels.org/reference/bag_tree.html)
  with the `"rpart"` or `"C5.0"` engine.
  ([\#269](https://github.com/tidymodels/tidypredict/issues/269),
  [\#270](https://github.com/tidymodels/tidypredict/issues/270))

#### H2O

- H2O gradient boosting models (`H2ORegressionModel`, `H2OBinomialModel`
  and `H2OMultinomialModel`), via
  [`boost_tree()`](https://parsnip.tidymodels.org/reference/boost_tree.html)
  with the `"h2o_gbm"` engine.
  ([\#250](https://github.com/tidymodels/tidypredict/issues/250))
  - Only GBM models, not H2O’s XGBoost, and only the gaussian, bernoulli
    and multinomial distributions.
- H2O RuleFit models
  ([`h2o::h2o.rulefit()`](https://rdrr.io/pkg/h2o/man/h2o.rulefit.html)),
  via
  [`rule_fit()`](https://parsnip.tidymodels.org/reference/rule_fit.html)
  with the `"h2o"` engine.
  ([\#257](https://github.com/tidymodels/tidypredict/issues/257))
  - Regression and binary classification only.

#### Naive Bayes

Both are supported when fit without kernel density estimates
(`usekernel = FALSE`), and return a named list of class-probability
expressions.

- [`klaR::NaiveBayes()`](https://rdrr.io/pkg/klaR/man/NaiveBayes.html)
  with Gaussian densities, via
  [`naive_Bayes()`](https://parsnip.tidymodels.org/reference/naive_Bayes.html)
  with the `"klaR"` engine.
  ([\#264](https://github.com/tidymodels/tidypredict/issues/264))
- [`naivebayes::naive_bayes()`](https://majkamichal.github.io/naivebayes/reference/naive_bayes.html),
  via
  [`naive_Bayes()`](https://parsnip.tidymodels.org/reference/naive_Bayes.html)
  with the `"naivebayes"` engine.
  ([\#266](https://github.com/tidymodels/tidypredict/issues/266))
  - Gaussian, categorical, Bernoulli and Poisson conditional
    distributions.

#### Neural networks and multinomial regression

- [`nnet::nnet()`](https://rdrr.io/pkg/nnet/man/nnet.html) single hidden
  layer neural networks, via
  [`mlp()`](https://parsnip.tidymodels.org/reference/mlp.html) with the
  `"nnet"` engine.
  ([\#267](https://github.com/tidymodels/tidypredict/issues/267))
- [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)
  multinomial log-linear models, via
  [`multinom_reg()`](https://parsnip.tidymodels.org/reference/multinom_reg.html)
  with the `"nnet"` engine.
  ([\#255](https://github.com/tidymodels/tidypredict/issues/255))

#### Support vector machines

- [`kernlab::ksvm()`](https://rdrr.io/pkg/kernlab/man/ksvm.html) linear
  support vector machines (`vanilladot` kernel), via
  [`svm_linear()`](https://parsnip.tidymodels.org/reference/svm_linear.html)
  with the `"kernlab"` engine, for regression and binary classification.
  ([\#252](https://github.com/tidymodels/tidypredict/issues/252))
  - Non-linear kernels and multiclass classification are not supported,
    and classification requires a probability model
    (`prob.model = TRUE`).
- [`LiblineaR::LiblineaR()`](https://rdrr.io/pkg/LiblineaR/man/LiblineaR.html)
  support vector machines, via
  [`svm_linear()`](https://parsnip.tidymodels.org/reference/svm_linear.html)
  with the `"LiblineaR"` engine, for regression (`type` 11, 12, 13) and
  binary classification (`type` 1-5).
  ([\#253](https://github.com/tidymodels/tidypredict/issues/253))
  - Classification returns the SVM decision value rather than a
    probability.
- [`LiblineaR::LiblineaR()`](https://rdrr.io/pkg/LiblineaR/man/LiblineaR.html)
  binary logistic regression (`type` 0, 6, 7), via
  [`logistic_reg()`](https://parsnip.tidymodels.org/reference/logistic_reg.html)
  with the `"LiblineaR"` engine.
  ([\#243](https://github.com/tidymodels/tidypredict/issues/243))

#### Other models

- The partial least squares models in `mixOmics`
  ([`pls()`](https://parsnip.tidymodels.org/reference/pls.html),
  `spls()`, `plsda()` and `splsda()`), via
  [`pls()`](https://parsnip.tidymodels.org/reference/pls.html) with the
  `"mixOmics"` engine, for regression and classification.
  ([\#262](https://github.com/tidymodels/tidypredict/issues/262))
- [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) quantile
  regression, via
  [`linear_reg()`](https://parsnip.tidymodels.org/reference/linear_reg.html)
  with the `"quantreg"` engine.
  ([\#241](https://github.com/tidymodels/tidypredict/issues/241))
- [`parsnip::nullmodel()`](https://parsnip.tidymodels.org/reference/nullmodel.html),
  via
  [`null_model()`](https://parsnip.tidymodels.org/reference/null_model.html)
  with the `"parsnip"` engine.
  ([\#263](https://github.com/tidymodels/tidypredict/issues/263))

### Improvements

#### Model support

- Added support for multinomial
  [`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)
  models (`family = "multinomial"`), including
  [`multinom_reg()`](https://parsnip.tidymodels.org/reference/multinom_reg.html)
  parsnip models fitted with the `"glmnet"` engine.
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  returns a named list of class-probability expressions (softmax).
  ([\#198](https://github.com/tidymodels/tidypredict/issues/198),
  [\#254](https://github.com/tidymodels/tidypredict/issues/254))

- Added support for
  [`decision_tree()`](https://parsnip.tidymodels.org/reference/decision_tree.html)
  parsnip models fitted with the `"rpart"` engine.
  ([\#244](https://github.com/tidymodels/tidypredict/issues/244))

- Added support for
  [`linear_reg()`](https://parsnip.tidymodels.org/reference/linear_reg.html)
  parsnip models fitted with the `"glm"` engine.
  ([\#239](https://github.com/tidymodels/tidypredict/issues/239))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now supports
  [`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html)
  models that split a discrete predictor into one branch per level.
  ([\#245](https://github.com/tidymodels/tidypredict/issues/245))

#### New functions

- [`tidypredict_save()`](https://tidypredict.tidymodels.org/reference/tidypredict_save.md)
  and
  [`tidypredict_load()`](https://tidypredict.tidymodels.org/reference/tidypredict_save.md)
  write a parsed model to a YAML file and read it back. Use them instead
  of
  [`yaml::write_yaml()`](https://yaml.r-lib.org/reference/write_yaml.html),
  which stores only 7 significant digits by default and so rounds split
  thresholds enough to send rows down a different branch when the model
  is re-loaded.
  ([\#307](https://github.com/tidymodels/tidypredict/issues/307))

#### Error messages and input validation

- [`acceptable_formula()`](https://tidypredict.tidymodels.org/reference/acceptable_formula.md),
  [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md),
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  and
  [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md)
  now report a model class they do not support with a message naming the
  class, rather than failing with R’s “no applicable method” error.
  [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md)
  gives a parsed model the same message it gives a fitted one, instead
  of “Model type not supported.”.
  ([\#313](https://github.com/tidymodels/tidypredict/issues/313))

- [`as_parsed_model()`](https://tidypredict.tidymodels.org/reference/as_parsed_model.md)
  now rejects an object that is not a parsed model.
  ([\#313](https://github.com/tidymodels/tidypredict/issues/313))

- [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md)
  now rejects an `interval` that is not a single number strictly between
  0 and
  1.([\#313](https://github.com/tidymodels/tidypredict/issues/313))

- [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md)
  now reports a list that is not a parsed model,
  ([\#308](https://github.com/tidymodels/tidypredict/issues/308),
  [\#313](https://github.com/tidymodels/tidypredict/issues/313))

- [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md)
  and
  [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md)
  now check that dbplyr is installed before using it.
  ([\#314](https://github.com/tidymodels/tidypredict/issues/314))

- [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)
  now validates `vars`, `add_interval` and `interval`.
  ([\#313](https://github.com/tidymodels/tidypredict/issues/313))

- [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)
  now explains that a model returning more than one formula is
  unsupported, instead of incorrectly claiming that tree based models
  are unsupported.
  ([\#279](https://github.com/tidymodels/tidypredict/issues/279))

#### Documentation

- New articles for
  [`kernlab::ksvm()`](https://rdrr.io/pkg/kernlab/man/ksvm.html),
  [`mboost::blackboost()`](https://rdrr.io/pkg/mboost/man/blackboost.html)
  and [`xrf::xrf()`](https://rdrr.io/pkg/xrf/man/xrf.html), and the
  model list menu now links to the `LiblineaR` and `quantreg` sections
  directly.
  ([\#317](https://github.com/tidymodels/tidypredict/issues/317))

- The lm, glm, ranger, randomForest and rpart articles recommended
  `tidypredict_to_sql()`, which does not exist. They now point at
  [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md).
  ([\#317](https://github.com/tidymodels/tidypredict/issues/317))

- The Cubist article now documents two limits on how closely
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  can match `Cubist::predict()`. The instance-based correction that
  [`predict()`](https://rdrr.io/r/stats/predict.html) applies when
  `neighbors` is greater than zero is not reproduced, because it adjusts
  each prediction using training rows that are not part of the fitted
  model. Separately, Cubist stores its coefficients as 32-bit floats, so
  the agreement has a relative ceiling near 1e-7 rather than an absolute
  one, and an outcome on a large scale leaves a proportionally large
  absolute difference.
  ([\#375](https://github.com/tidymodels/tidypredict/issues/375))

- The glm article now documents the one inverse link
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  does not reproduce exactly: `probit`, whose inverse is
  [`pnorm()`](https://rdrr.io/r/stats/Normal.html), is written as the
  Bowling et al. logistic approximation to the normal CDF because no SQL
  backend has a normal CDF. It costs about 1e-4 of probability.
  ([\#355](https://github.com/tidymodels/tidypredict/issues/355))

- The naive Bayes article now documents the one case where
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  does not reproduce [`predict()`](https://rdrr.io/r/stats/predict.html)
  for
  [`klaR::NaiveBayes()`](https://rdrr.io/pkg/klaR/man/NaiveBayes.html)
  and
  [`naivebayes::naive_bayes()`](https://majkamichal.github.io/naivebayes/reference/naive_bayes.html)
  models: both replace a normal density that underflowed to zero with
  their `threshold` argument, which takes a value roughly 38 standard
  deviations from the class mean, and the log scale used throughout
  never underflows.
  ([\#300](https://github.com/tidymodels/tidypredict/issues/300))

- The models article now documents a limit on
  [`kernlab::ksvm()`](https://rdrr.io/pkg/kernlab/man/ksvm.html) models
  fitted through the matrix interface, `ksvm(x, y)`.
  [`ksvm()`](https://rdrr.io/pkg/kernlab/man/ksvm.html) mangles its
  model matrix column names with
  [`make.names()`](https://rdrr.io/r/base/make.names.html) and keeps no
  record of the originals, and unlike the formula interface there is no
  `terms` object to detect this against, so a non-syntactic column name
  such as `a:b` yields a formula referring to a column the data does not
  have. This cannot be caught automatically, because every name
  [`make.names()`](https://rdrr.io/r/base/make.names.html) produces is
  also a name it leaves alone, so `a.b` from a mangled `a:b` is
  indistinguishable from a correct model with a column genuinely named
  `a.b`. ([\#418](https://github.com/tidymodels/tidypredict/issues/418))

### Bug Fixes

- [`.build_case_when_tree()`](https://tidypredict.tidymodels.org/reference/dot-build_case_when_tree.md),
  which {orbital} calls, now returns the bare prediction of a
  classification stump, instead of the `case_when(.default = "a")` that
  dplyr rejects.
  ([\#310](https://github.com/tidymodels/tidypredict/issues/310))

- [`acceptable_formula()`](https://tidypredict.tidymodels.org/reference/acceptable_formula.md)
  now checks the contrast of every factor predictor, not just one, and
  names the offending field. A model mixing contrasts was accepted and
  then silently mis-parsed.
  ([\#291](https://github.com/tidymodels/tidypredict/issues/291))

- [`acceptable_formula()`](https://tidypredict.tidymodels.org/reference/acceptable_formula.md)
  no longer rejects a
  [`MASS::lda()`](https://rdrr.io/pkg/MASS/man/lda.html),
  [`MASS::qda()`](https://rdrr.io/pkg/MASS/man/qda.html) or
  [`earth::earth()`](https://rdrr.io/pkg/earth/man/earth.html) fit whose
  factor has a level containing a colon, which the contrast check read
  as an interaction.
  ([\#391](https://github.com/tidymodels/tidypredict/issues/391))

- [`set_catboost_categories()`](https://tidypredict.tidymodels.org/reference/set_catboost_categories.md)
  now takes its hashes from CatBoost’s own hash function, so it names
  every category of any factor. A factor with four or more levels
  errored with “No category mapping found for hash”, and a two-level one
  could be named the wrong way round.
  ([\#297](https://github.com/tidymodels/tidypredict/issues/297))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now works on a LightGBM model whose trees are bare leaves, which
  failed with “Model has no trees.”. A multiclass model with only some
  bare leaves also assigned trees to the wrong classes.
  ([\#401](https://github.com/tidymodels/tidypredict/issues/401))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns one prediction per row for a
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md),
  `xgboost`,
  [`baguette::bagger()`](https://baguette.tidymodels.org/reference/bagger.html)
  or [`xrf::xrf()`](https://rdrr.io/pkg/xrf/man/xrf.html) model whose
  formula mentions no column, such as an all-stump forest or an
  intercept-only lasso. The value was correct, its length was not.
  ([\#397](https://github.com/tidymodels/tidypredict/issues/397))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now applies the bias correction of a
  [`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  model fitted with `corr.bias = TRUE`, which left predictions off by as
  much as 0.21 for a model of `mpg` on `mtcars`.
  ([\#395](https://github.com/tidymodels/tidypredict/issues/395))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now sends a split threshold that is not finite, or that overflows the
  32-bit float range, down the branch the model does, rather than
  turning it into a `NaN` that fails every comparison.
  ([\#313](https://github.com/tidymodels/tidypredict/issues/313))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now works on a parsed LightGBM model fit with `linear_tree = TRUE`,
  which failed with “`..1 (right)` must be a vector, not `NULL`”.
  ([\#346](https://github.com/tidymodels/tidypredict/issues/346))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now follows the per-node missing value direction a
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  model learns under the `na.action = "na.learn"` default of ranger
  0.17.0, instead of always sending them left.
  ([\#394](https://github.com/tidymodels/tidypredict/issues/394))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now matches [`predict()`](https://rdrr.io/r/stats/predict.html) for a
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  model fitted with `respect.unordered.factors = "partition"` on a
  factor with more than 31 levels, whose split mask
  [`ranger::treeInfo()`](http://imbs-hl.github.io/ranger/reference/treeInfo.md)
  silently blanks out.
  ([\#414](https://github.com/tidymodels/tidypredict/issues/414))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  no longer returns `NULL` for a parsed model saved by tidypredict 1.0.1
  or earlier that came from a `partykit` or `rpart` single tree. Any
  parsed model type that is still unhandled now raises an error.
  ([\#304](https://github.com/tidymodels/tidypredict/issues/304))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  no longer fails with “`x` must be a formula” on a parsed model saved
  by tidypredict 1.0.1 or earlier that contains a
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  or
  [`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  stump. ([\#310](https://github.com/tidymodels/tidypredict/issues/310))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now handles three parsed model shapes that only a hand-written or
  edited parsed model contains: a path mixing a `type = "all"` element
  with real conditions, a rule whose linear prediction is a single
  non-intercept term, and a rule whose terms are all zero.
  ([\#310](https://github.com/tidymodels/tidypredict/issues/310))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now assigns rules to the right committee for
  [`Cubist::cubist()`](http://topepo.github.io/Cubist/reference/cubist.default.md)
  models fitted with more than 20 committees, where the printed model it
  read the counts from truncates them.
  ([\#286](https://github.com/tidymodels/tidypredict/issues/286))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now applies the per-rule extrapolation limits of
  [`Cubist::cubist()`](http://topepo.github.io/Cubist/reference/cubist.default.md)
  models, which hold each rule to the span of the training outcomes it
  covers. This engages on rows of the training data too.
  ([\#285](https://github.com/tidymodels/tidypredict/issues/285))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now supports factor predictors for
  [`Cubist::cubist()`](http://topepo.github.io/Cubist/reference/cubist.default.md)
  models, which previously produced a formula that could not be
  evaluated (`object '"f"' not found`).
  ([\#322](https://github.com/tidymodels/tidypredict/issues/322))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now reads the coefficient labels of an
  [`lm()`](https://rdrr.io/r/stats/lm.html),
  [`glm()`](https://rdrr.io/r/stats/glm.html) or
  [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) model
  from the model’s own term structure. A factor level containing a `:`
  was taken apart as an interaction, and a label equal to another
  predictor’s name was read as that predictor.
  ([\#308](https://github.com/tidymodels/tidypredict/issues/308))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now rejects an
  [`earth::earth()`](https://rdrr.io/pkg/earth/man/earth.html) model fit
  with a contrast other than the treatment one. An ordered factor gave a
  formula comparing the column against contrast values such as
  `-0.2236`.
  ([\#323](https://github.com/tidymodels/tidypredict/issues/323))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now routes missing values by each node’s `missing_type` for `lightgbm`
  models. Consulting `default_left` alone was wrong for every model
  trained without missing data, which is the common case.
  ([\#288](https://github.com/tidymodels/tidypredict/issues/288))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now honors `zero_as_missing` for `lightgbm` models, where an exact
  zero takes the same branch as a missing value. Predictions were wrong
  on the training data itself.
  ([\#288](https://github.com/tidymodels/tidypredict/issues/288))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  no longer sends a missing value down the left branch of a categorical
  split for `lightgbm` models. LightGBM sends it right whatever
  `default_left` says.
  ([\#288](https://github.com/tidymodels/tidypredict/issues/288))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  no longer returns `NaN` for every class probability of a row whose
  class scores are large, for any model whose prediction is a softmax.
  `exp(s) / sum(exp(s))` overflows once a score passes about 710, and is
  now written as the equivalent `1 / sum(exp(s_j - s_k))`.
  ([\#299](https://github.com/tidymodels/tidypredict/issues/299))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now rejects a `glmnet` model fit with an `offset`, whose values glmnet
  never records, rather than silently dropping it and predicting wrong
  by its size.
  ([\#296](https://github.com/tidymodels/tidypredict/issues/296))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now rejects a
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  probability or survival forest, read from `treetype`, instead of
  emitting `case_when(x <= 0.0066 ~ NULL, .default = NULL)`.
  ([\#301](https://github.com/tidymodels/tidypredict/issues/301))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now sends a value sitting exactly on a split boundary the way the
  model does, for the backends that compare split thresholds as 32-bit
  floats: `xgboost`, `lightgbm`, `catboost`,
  [`Cubist::cubist()`](http://topepo.github.io/Cubist/reference/cubist.default.md)
  and
  [`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html).
  About half of all thresholds round that tie towards the neighbouring
  float. ([\#350](https://github.com/tidymodels/tidypredict/issues/350))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now honors `sigmoid` for `lightgbm` models fit with the `binary` or
  `multiclassova` objective. Every probability of a model fit with any
  other value was rescaled.
  ([\#288](https://github.com/tidymodels/tidypredict/issues/288))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now honors `reg_sqrt` for `lightgbm` models, whose predictions were
  left on the square-root scale.
  ([\#288](https://github.com/tidymodels/tidypredict/issues/288))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  and
  [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
  now work on an `xgboost` booster that has been saved and reloaded with
  [`xgb.save()`](https://rdrr.io/pkg/xgboost/man/xgb.save.html) /
  [`xgb.load()`](https://rdrr.io/pkg/xgboost/man/xgb.load.html), which
  failed with `argument "model" is missing, with no default`. The
  objective is now recovered from the saved model, which a reloaded
  booster records nowhere else.
  ([\#292](https://github.com/tidymodels/tidypredict/issues/292))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now works for rank-deficient [`lm()`](https://rdrr.io/r/stats/lm.html)
  and [`glm()`](https://rdrr.io/r/stats/glm.html) models, which aborted
  with “Unable to calculate inverse of QR decomposition”. A duplicated
  predictor column or one with no variance is enough to hit it, and
  [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md)
  keeps working for these models.
  ([\#308](https://github.com/tidymodels/tidypredict/issues/308))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now supports splits with more than two branches for `partykit` models,
  such as those from `ctree_control(multiway = TRUE)`. Every branch
  after the second was dropped.
  ([\#295](https://github.com/tidymodels/tidypredict/issues/295))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now honors `partysplit(right = FALSE)` for `partykit` models, where a
  value falling exactly on the break took the wrong branch.
  ([\#295](https://github.com/tidymodels/tidypredict/issues/295))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now handles ordered factor predictors for `partykit` models, which
  previously errored with “Result must be length 1, not 2”.
  ([\#295](https://github.com/tidymodels/tidypredict/issues/295))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  no longer swaps the two branches of every
  [`partykit::party`](https://rdrr.io/pkg/partykit/man/party.html)
  converted from an `rpart` model.
  ([\#295](https://github.com/tidymodels/tidypredict/issues/295))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now decodes factor splits for
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  models, in all three `respect.unordered.factors` modes and for ordered
  factors, rather than comparing the split value as a numeric threshold.
  ([\#283](https://github.com/tidymodels/tidypredict/issues/283))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now decodes factor splits for
  [`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  models, where an unordered factor’s split point is a bit mask and an
  ordered factor’s is a level code, rather than reading either as a
  numeric threshold.
  ([\#282](https://github.com/tidymodels/tidypredict/issues/282))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  and
  [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
  now handle a stump in a
  [`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  forest, instead of aborting with “argument of length 0”. A constant
  outcome or a zero-variance predictor makes one routine.
  ([\#362](https://github.com/tidymodels/tidypredict/issues/362))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now substitutes the training mean for a missing predictor in
  [`Cubist::cubist()`](http://topepo.github.io/Cubist/reference/cubist.default.md)
  models, matching [`predict()`](https://rdrr.io/r/stats/predict.html),
  in the rule conditions as well as the linear models.
  ([\#294](https://github.com/tidymodels/tidypredict/issues/294))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now sends a missing predictor down the left branch for
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  models, matching [`predict()`](https://rdrr.io/r/stats/predict.html).
  ([\#294](https://github.com/tidymodels/tidypredict/issues/294))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now routes missing values through surrogate splits for
  [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html) models,
  and for
  [`baguette::bagger()`](https://baguette.tidymodels.org/reference/bagger.html)
  models using the `"CART"` base model, in all three `usesurrogate`
  modes, instead of sending them right.
  ([\#294](https://github.com/tidymodels/tidypredict/issues/294))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns `NA` for a row that reaches a split on a predictor it is
  missing, for
  [`partykit::ctree()`](https://rdrr.io/pkg/partykit/man/ctree.html),
  [`partykit::cforest()`](https://rdrr.io/pkg/partykit/man/cforest.html)
  and
  [`mboost::blackboost()`](https://rdrr.io/pkg/mboost/man/blackboost.html)
  models. These resolve a missing value by random sampling, so
  [`predict()`](https://rdrr.io/r/stats/predict.html) returns a
  different answer on each call.
  ([\#294](https://github.com/tidymodels/tidypredict/issues/294))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns `NA` for a row with a missing predictor for
  [`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  and
  [`aorsf::orsf()`](https://docs.ropensci.org/aorsf/reference/orsf.html)
  models, neither of which will predict from an incomplete row. Rows are
  kept rather than dropped.
  ([\#294](https://github.com/tidymodels/tidypredict/issues/294),
  [\#325](https://github.com/tidymodels/tidypredict/issues/325))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns correct predictions for `catboost` models whose predictor
  values fall on a split border, which catboost compares as 32-bit
  floats.
  ([\#298](https://github.com/tidymodels/tidypredict/issues/298))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now picks the right factor predictor when three or more variable names
  are nested prefixes of one another, such as `x`, `xy` and `xyz`. The
  wrong variable was silently chosen for
  [`lm()`](https://rdrr.io/r/stats/lm.html),
  [`glm()`](https://rdrr.io/r/stats/glm.html),
  [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html),
  [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html),
  [`nnet::nnet()`](https://rdrr.io/pkg/nnet/man/nnet.html) and
  [`earth::earth()`](https://rdrr.io/pkg/earth/man/earth.html).
  ([\#290](https://github.com/tidymodels/tidypredict/issues/290))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now uses a strict inequality (`<`) for the continuous splits of
  [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html) models,
  matching how `rpart` assigns values exactly equal to a cut point.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns correct predictions for
  [`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  models saved and reloaded with
  [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
  and
  [`as_parsed_model()`](https://tidypredict.tidymodels.org/reference/as_parsed_model.md),
  which named every split variable after the first leaf of a tree
  incorrectly.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns correct predictions for
  [`Cubist::cubist()`](http://topepo.github.io/Cubist/reference/cubist.default.md)
  models whose predictor values fall exactly on a split threshold, which
  Cubist compares as 32-bit floats.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now keeps small probabilities for models with a logit link, such as
  [`glm()`](https://rdrr.io/r/stats/glm.html) with `family = binomial`
  and
  [`LiblineaR::LiblineaR()`](https://rdrr.io/pkg/LiblineaR/man/LiblineaR.html).
  The inverse link rounded to exactly 0 once the linear predictor fell
  below about -37.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns correct predictions for xgboost models whose feature
  values fall exactly on a split threshold, which xgboost compares as
  32-bit floats.
  ([\#45](https://github.com/tidymodels/tidypredict/issues/45))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns correct predictions for xgboost models that have been
  saved and reloaded with
  [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
  and
  [`as_parsed_model()`](https://tidypredict.tidymodels.org/reference/as_parsed_model.md).
  Previously every tree collapsed to a single leaf value.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md)
  now works for [`glm()`](https://rdrr.io/r/stats/glm.html) models,
  which returned `numeric(0)` for every gaussian glm because the
  residual variance was read from `summary()$sigma`, which only
  [`summary.lm()`](https://rdrr.io/r/stats/summary.lm.html) has.
  ([\#293](https://github.com/tidymodels/tidypredict/issues/293))

- [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md)
  now honors its `interval` argument, which was hardcoded to 0.95.
  `tidypredict_to_column(add_interval = TRUE)` and
  [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md)
  were affected too.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md)
  now returns a single query for an intercept-only model, whose
  bare-number formula was mistaken for the list a multiclass model
  produces.
  ([\#313](https://github.com/tidymodels/tidypredict/issues/313))

### Breaking Changes

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns predictions on the response scale for CatBoost models fit
  with the `Poisson` or `Tweedie` objective, applying
  [`exp()`](https://rdrr.io/r/base/Log.html) to the raw score as the
  other CatBoost objectives already invert their own links. Anyone using
  such a model will see their predictions change from the log scale to
  the count or mean scale; they now match
  `catboost.predict(prediction_type = "Exponent")` instead of the
  `"RawFormulaVal"` default.
  ([\#356](https://github.com/tidymodels/tidypredict/issues/356))

### Developer

- New generics expose the pieces
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  is assembled from, so that packages generating their own code from a
  fitted model can reuse tidypredict’s parsing:
  [`tidypredict_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md)
  returns per-tree expressions,
  [`tidypredict_class_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md)
  returns them for each outcome level,
  [`tidypredict_class_exprs()`](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md)
  returns one finished expression per outcome level, and
  [`tidypredict_n_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md)
  returns the number of trees. See
  [`?tidypredict_extractors`](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md).
  ([\#433](https://github.com/tidymodels/tidypredict/issues/433))

- [`tidypredict_combine_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_combine_trees.md)
  is a new generic that turns per-tree expressions back into a model’s
  prediction, with methods for `randomForest`, `ranger`, xgboost,
  LightGBM, CatBoost, `cforest`, `blackboost` and `aorsf`. Summing or
  averaging the trees, as the shape of the list invites, is wrong for
  any backend carrying an offset, a scale or a link.
  ([\#436](https://github.com/tidymodels/tidypredict/issues/436))

- [`tidypredict_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md)
  and
  [`tidypredict_n_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md)
  gain methods for
  [`partykit::cforest()`](https://rdrr.io/pkg/partykit/man/cforest.html),
  [`mboost::blackboost()`](https://rdrr.io/pkg/mboost/man/blackboost.html)
  and
  [`aorsf::orsf()`](https://docs.ropensci.org/aorsf/reference/orsf.html).
  ([\#436](https://github.com/tidymodels/tidypredict/issues/436))

- Boosted
  [`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html)
  models deliberately have no
  [`tidypredict_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md)
  method, and
  [`tidypredict_combine_trees()`](https://tidypredict.tidymodels.org/reference/tidypredict_combine_trees.md)
  refuses them with an explanation. Their trials vote with a class label
  and a confidence rather than contributing numbers, so there is nothing
  to sum or average.
  ([\#436](https://github.com/tidymodels/tidypredict/issues/436))

- [`?tidypredict_extractors`](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md)
  now documents which model classes implement each of the extractor
  generics, and what to implement when adding a new one. The three seam
  topics cross-reference each other.
  ([\#436](https://github.com/tidymodels/tidypredict/issues/436))

- New generics describe what a model’s fitted expressions compute, which
  the expressions themselves do not say:
  [`tidypredict_output_type()`](https://tidypredict.tidymodels.org/reference/tidypredict_metadata.md)
  returns one of `"numeric"`, `"prob"`, `"decision"` or `"class"`,
  [`tidypredict_outcome_levels()`](https://tidypredict.tidymodels.org/reference/tidypredict_metadata.md)
  returns the outcome levels in model order, and
  [`tidypredict_normalized()`](https://tidypredict.tidymodels.org/reference/tidypredict_metadata.md)
  reports whether per-level probabilities already sum to one. None of it
  is recoverable from the shape of the result: a `LiblineaR` SVM
  classifier and a `LiblineaR` logistic regression both return a single
  expression, but only the second is a probability. See
  [`?tidypredict_metadata`](https://tidypredict.tidymodels.org/reference/tidypredict_metadata.md).
  ([\#433](https://github.com/tidymodels/tidypredict/issues/433),
  [\#435](https://github.com/tidymodels/tidypredict/issues/435))

- [`tidypredict_class_exprs()`](https://tidypredict.tidymodels.org/reference/tidypredict_extractors.md)
  on a `partykit` model is named by outcome level. The
  [`.extract_partykit_classprob()`](https://tidypredict.tidymodels.org/reference/deprecated-extractors.md)
  it replaces returned an unnamed list, which left callers assuming its
  order matched [`levels()`](https://rdrr.io/r/base/levels.html) of the
  outcome.
  ([\#433](https://github.com/tidymodels/tidypredict/issues/433))

- The error raised when no method knows how to handle a model at all now
  carries the condition class `tidypredict_unsupported_model`, so a
  wrapper such as orbital can tell it apart from the many errors
  reporting an unsupported *configuration* of a model that is otherwise
  handled.
  ([\#432](https://github.com/tidymodels/tidypredict/issues/432))

## tidypredict 1.1.1

CRAN release: 2026-08-24

- [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
  and
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now detect xgboost dropout (DART) models from the saved dropout
  weights rather than the serialised booster name, so they keep applying
  `weight_drop` with xgboost 3.4.0 and later, which canonicalises
  `booster = "dart"` to `"gbtree"`.
  ([\#238](https://github.com/tidymodels/tidypredict/issues/238))

## tidypredict 1.1.0

CRAN release: 2026-02-27

### New Model Supports

- Added support for rpart decision tree models (`rpart`).
  ([\#226](https://github.com/tidymodels/tidypredict/issues/226))

- Added support for CatBoost models (`catboost.Model`).
  ([\#179](https://github.com/tidymodels/tidypredict/issues/179),
  [\#187](https://github.com/tidymodels/tidypredict/issues/187),
  [\#188](https://github.com/tidymodels/tidypredict/issues/188))

  - Objectives: RMSE, MAE, Quantile, MAPE, Poisson, Huber, LogCosh,
    Expectile, Tweedie, Logloss, CrossEntropy, MultiClass, and
    MultiClassOneVsAll.
  - Tree types: oblivious (default `SymmetricTree`) and non-oblivious
    (`Depthwise` or `Lossguide` grow policy).
  - Categorical features are handled automatically for parsnip/bonsai
    models; for raw CatBoost models use
    [`set_catboost_categories()`](https://tidypredict.tidymodels.org/reference/set_catboost_categories.md).

- Added support for LightGBM models (`lgb.Booster`).
  ([\#177](https://github.com/tidymodels/tidypredict/issues/177),
  [\#186](https://github.com/tidymodels/tidypredict/issues/186))

  - Objectives: regression, binary classification, and multiclass
    classification.
  - Supports categorical features.
  - Supports linear trees (`linear_tree = TRUE`), which fit a linear
    model at each leaf instead of a constant.

### Improvements

- Tree models (rpart, partykit, ranger, randomForest, xgboost, lightgbm,
  catboost) now generate nested
  [`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
  expressions that mirror the tree structure, instead of flat
  expressions with all leaf conditions at the same level. This produces
  more efficient SQL and R code because conditions are evaluated
  hierarchically.
  ([\#227](https://github.com/tidymodels/tidypredict/issues/227))

- [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
  now documents the parsed model version system (v1/v2/v3) and model
  type classes in its help page.
  ([\#227](https://github.com/tidymodels/tidypredict/issues/227))

- [`earth()`](https://rdrr.io/pkg/earth/man/earth.html) models now
  support additional GLM families and link functions: Gamma,
  inverse.gaussian, probit, and cloglog.
  ([\#194](https://github.com/tidymodels/tidypredict/issues/194),
  [\#195](https://github.com/tidymodels/tidypredict/issues/195))

- [`glm()`](https://rdrr.io/r/stats/glm.html) models now support
  additional families and link functions: Gamma family with inverse
  link, inverse.gaussian family with 1/mu^2 link, probit link, cloglog
  link, and sqrt link.
  ([\#203](https://github.com/tidymodels/tidypredict/issues/203),
  [\#204](https://github.com/tidymodels/tidypredict/issues/204),
  [\#205](https://github.com/tidymodels/tidypredict/issues/205),
  [\#206](https://github.com/tidymodels/tidypredict/issues/206),
  [\#207](https://github.com/tidymodels/tidypredict/issues/207))

- [`glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html) models
  now support `Gamma` family and Cox proportional hazards
  (`family = "cox"`) models.
  ([\#200](https://github.com/tidymodels/tidypredict/issues/200),
  [\#201](https://github.com/tidymodels/tidypredict/issues/201))

- xgboost support now includes additional objectives: `binary:hinge`,
  `reg:absoluteerror`, `reg:gamma`, `reg:pseudohubererror`, and
  `reg:squaredlogerror`.
  ([\#184](https://github.com/tidymodels/tidypredict/issues/184))

- Added a vignette on floating-point precision issues with tree-based
  models.
  ([\#231](https://github.com/tidymodels/tidypredict/issues/231))

### Bug Fixes

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now correctly handles xgboost models with stump trees (single leaf, no
  splits).
  ([\#182](https://github.com/tidymodels/tidypredict/issues/182))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now correctly handles xgboost DART booster models with
  `rate_drop > 0`. DART uses tree weight normalization during training,
  and these weights are now properly applied to each tree’s predictions.
  ([\#183](https://github.com/tidymodels/tidypredict/issues/183))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now correctly incorporates `base_score` for xgboost models with
  `count:poisson` and `reg:tweedie` objectives. Previously, predictions
  were incorrect when `base_score` was not the default value.
  ([\#184](https://github.com/tidymodels/tidypredict/issues/184))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now correctly averages tree predictions for LightGBM models with
  `boosting="rf"` instead of summing them.
  ([\#185](https://github.com/tidymodels/tidypredict/issues/185))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now uses the correct split operator (`<=` instead of `<`) for ranger
  models. Previously, predictions were incorrect when data values
  exactly matched split values.
  ([\#189](https://github.com/tidymodels/tidypredict/issues/189))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now correctly averages tree predictions for ranger models instead of
  summing them. Previously, predictions were `num.trees` times too
  large. ([\#190](https://github.com/tidymodels/tidypredict/issues/190))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now throws a clear error for ranger and randomForest classification
  models, which are not supported.
  ([\#191](https://github.com/tidymodels/tidypredict/issues/191),
  [\#193](https://github.com/tidymodels/tidypredict/issues/193))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now uses the correct split operator (`<=` instead of `<`) for
  randomForest models.
  ([\#192](https://github.com/tidymodels/tidypredict/issues/192))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now correctly handles partykit stump trees (models with no splits).
  ([\#196](https://github.com/tidymodels/tidypredict/issues/196))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now works with
  [`glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html) models
  that use family function syntax (e.g., `family = gaussian()`) instead
  of string syntax (e.g., `family = "gaussian"`).
  ([\#197](https://github.com/tidymodels/tidypredict/issues/197))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now works with models that use family function syntax (e.g.,
  `family = gaussian()`) instead of string syntax (e.g.,
  `family = "gaussian"`).
  ([\#202](https://github.com/tidymodels/tidypredict/issues/202))

## tidypredict 1.0.1

CRAN release: 2025-12-13

### Bug Fixes

- Fixed bug where `base_score` wasn’t extracted correctly xgboost for
  version 3 or higher.
  ([\#173](https://github.com/tidymodels/tidypredict/issues/173))

## tidypredict 1.0.0

CRAN release: 2025-11-29

### Breaking Changes

- Random forest implementations (ranger and randomForest) will now
  produce a single formula instead of a list of expressions.
  ([\#84](https://github.com/tidymodels/tidypredict/issues/84))

### New Model Supports

- Added support for glmnet models.
  ([\#165](https://github.com/tidymodels/tidypredict/issues/165))

### Improvements

- xgboost models with objectives `"reg:tweedie"` and `"count:poisson"`
  are now supported.
  ([\#72](https://github.com/tidymodels/tidypredict/issues/72),
  [@SimonCoulombe](https://github.com/SimonCoulombe))

- tree based models now uses `.default` argument in produced
  [`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
  code when applicable.
  ([\#153](https://github.com/tidymodels/tidypredict/issues/153))

- Speed up
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  for partykit and ranger packages.
  ([\#125](https://github.com/tidymodels/tidypredict/issues/125))

- Speed up
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  for xgboost models.
  ([\#130](https://github.com/tidymodels/tidypredict/issues/130))

- randomForest models now support regression outcomes.
  ([\#77](https://github.com/tidymodels/tidypredict/issues/77))

- An informative error will now be thrown if a lm model cannot be
  processed due to having linear combinations of predictors.
  ([\#124](https://github.com/tidymodels/tidypredict/issues/124))

- linear models such as [`lm()`](https://rdrr.io/r/stats/lm.html) and
  [`glm()`](https://rdrr.io/r/stats/glm.html) now work with interactions
  created with `*` and `:`.
  ([\#74](https://github.com/tidymodels/tidypredict/issues/74))

- Cubist rules will return simplified rules whenever possible to avoid
  multiplying by 0 and 1.
  ([\#152](https://github.com/tidymodels/tidypredict/issues/152))

- Make work with xgboost version \> 2.0.0.0.
  ([\#169](https://github.com/tidymodels/tidypredict/issues/169))

### Bug Fixes

- Fixed a bug where the intercept was added incorrectly to the result
  for cubist models.
  ([\#58](https://github.com/tidymodels/tidypredict/issues/58))

- Fixed bug where tidypredict would error on Cubist models without
  conditions.
  ([\#127](https://github.com/tidymodels/tidypredict/issues/127))

- Fixed bug where Cubist models incorrectly combined rules and
  committees.
  ([\#134](https://github.com/tidymodels/tidypredict/issues/134))

## tidypredict 0.5.1

CRAN release: 2024-12-19

- Exported a number of internal functions to be used in {orbital}
  package

## tidypredict 0.5

CRAN release: 2023-01-18

- Changes maintainer to Edgar Ruiz

- Updates author’s email addresses.

- Removes dependency with `stringr`

- Fixes issue with `earth` parsed_models
  ([\#108](https://github.com/tidymodels/tidypredict/issues/108))

- Addresses issues with XGBoost models

- Improvements to XGBoosts tests

## tidypredict 0.4.9

CRAN release: 2022-05-25

- Fixes issue handling GLM Binomial earth models
  ([\#97](https://github.com/tidymodels/tidypredict/issues/97))

- Adds capability to handle single simple Cubist models
  ([\#57](https://github.com/tidymodels/tidypredict/issues/57))

- Fixed parenthesis issue in the creation of the interval formula
  ([\#76](https://github.com/tidymodels/tidypredict/issues/76))

- Fixed bug in SQL query generation for XGBoost models with objective
  `binary:logistic`.

- Re-licensed package from GPL-3 to MIT. See [consent from copyright
  holders here](https://github.com/tidymodels/tidypredict/issues/95).

## tidypredict 0.4.8

CRAN release: 2020-10-28

- CRAN submission for a broken test case.

## tidypredict 0.4.7

CRAN release: 2020-10-05

- Changed to work with version 5.1.2 and above of the `earth` package.
  As a result, `tidypredict` will only parse objects created by this and
  later versions of `earth`.

## tidypredict 0.4.6

CRAN release: 2020-07-23

- Small release for `xgboost` changes.

## tidypredict 0.4.5

CRAN release: 2020-02-10

- Switches maintainer to Max Kuhn

## tidypredict 0.4.3

CRAN release: 2019-09-03

- Adds support for categorical predictors in `partykit`

- Fixes `parsnip` tests to meet standards of new CRAN version

## tidypredict 0.4.2

CRAN release: 2019-07-15

- Simplifies tests that verify `ranger`

- Adds fit method for parsed `xgboost` models

- Sets conditional requirement for `xgboost`, for test and vignette

## tidypredict 0.4.0

CRAN release: 2019-07-12

### New features

- Parses `ranger` classification models.

- Adds method support for `broom`’s
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) function.
  Regression models only

- Adds
  [`as_parsed_model()`](https://tidypredict.tidymodels.org/reference/as_parsed_model.md)
  function. It adds the proper class components to the list.

- Adds initial support for `partykit`’s
  [`ctree()`](https://rdrr.io/pkg/partykit/man/ctree.html) model

- Adds support for `parsnip` fitted models: `lm`, `randomForest`,
  `ranger`, and `earth`

- Adds support for xgb.Booster models provided by the `xgboost` package
  ([@Athospd](https://github.com/Athospd),
  [\#43](https://github.com/tidymodels/tidypredict/issues/43))

- Adds support for
  [`Cubist::cubist()`](http://topepo.github.io/Cubist/reference/cubist.default.md)
  models (# 36)

## tidypredict 0.3.0

CRAN release: 2019-01-10

### New features

- Adds support for MARS models provided by the `earth` package

### Improvements

- New parsed models are now list objects as opposed to data frames.

- tidypredict_to_column() no longer supports `ranger` and `randomForest`
  because of the multiple queries generated by multiple trees.

- All functions that read the parsed models and create the tidy eval
  formula now use the list object.

- Most of the code that depends on dplyr programming has been removed.

- Removes dependencies on: tidyr, tibble

- The `x/y` interface for `earth` models can now be used.

### Bug Fixes

- It now returns all of the trees instead of just one for tree based
  models (`randomForest` & `ranger`)
  ([\#29](https://github.com/tidymodels/tidypredict/issues/29))

## tidypredict 0.2.1

CRAN release: 2018-12-20

### Bug Fixes

- tibble 2.0.0 compatibility fix ([@krlmlr](https://github.com/krlmlr))

## tidypredict 0.2.0

CRAN release: 2018-02-25

### New features

- Add support for
  [`ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  models.

### Bug fixes

- Using `x ~.` in a randomForest() formula fails
  ([\#18](https://github.com/tidymodels/tidypredict/issues/18)
  [@washcycle](https://github.com/washcycle)).
