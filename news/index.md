# Changelog

## tidypredict (development version)

- The glm article now documents the one inverse link
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  does not reproduce exactly: `probit`, whose inverse is
  [`pnorm()`](https://rdrr.io/r/stats/Normal.html), is written as the
  Bowling et al. logistic approximation to the normal CDF because no SQL
  backend has a normal CDF. It costs about 1e-4 of probability, which is
  enough for
  [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  to report a probit model as failing at its default threshold.
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

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now produces a formula R can evaluate for a
  [`dbarts::bart()`](https://rdrr.io/pkg/dbarts/man/bart.html) fit at
  the package default `ntree`. Terms are summed left to right, which
  nests the `+` calls as deeply as there are terms, and a bart fit sums
  `ndpost * ntree` leaf values: at the defaults R gave up with
  “evaluation nested too deeply”. A model with 1000 terms or more is now
  summed in a balanced shape instead, nesting `log2(n)` deep. Only a
  large ensemble reaches that, so every other model keeps the flat
  left-to-right sum it had before, along with the exact result and the
  formula layout that go with it.
  ([\#305](https://github.com/tidymodels/tidypredict/issues/305))

- [`.build_case_when_tree()`](https://tidypredict.tidymodels.org/reference/dot-build_case_when_tree.md),
  which {orbital} calls, now returns the bare prediction of a stump tree
  whether that prediction is a number or a class label. A classification
  stump previously produced `case_when(.default = "a")`, which dplyr
  rejects with “`...` can’t be empty”.
  ([\#310](https://github.com/tidymodels/tidypredict/issues/310))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  no longer fails with “`x` must be a formula” on a parsed model saved
  by tidypredict 1.0.1 or earlier that contains a
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  or
  [`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  stump, a tree whose root is its only node. Such a tree is now written
  as its constant prediction.
  ([\#310](https://github.com/tidymodels/tidypredict/issues/310))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns correct predictions for
  [`kernlab::ksvm()`](https://rdrr.io/pkg/kernlab/man/ksvm.html) models
  with a single numeric predictor, which previously produced a bare
  constant. kernlab leaves the column names of a one-column model matrix
  empty, so every term was dropped and only the intercept remained.
  ([\#289](https://github.com/tidymodels/tidypredict/issues/289))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now undoes kernlab’s predictor scaling when exactly one column was
  scaled for
  [`kernlab::ksvm()`](https://rdrr.io/pkg/kernlab/man/ksvm.html) models.
  This covers any fit with one numeric predictor plus factor predictors,
  since kernlab does not scale dummy columns, and the weights were left
  on the scaled scale because the centers and scales lose their names in
  that case.
  ([\#289](https://github.com/tidymodels/tidypredict/issues/289))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now applies the per-rule extrapolation limits for
  [`Cubist::cubist()`](http://topepo.github.io/Cubist/reference/cubist.default.md)
  models. Cubist holds each rule to the span of the training outcomes it
  covers, widened at both ends by `extrap` times that span and never
  crossing zero; without it a rule’s linear model runs away on data
  outside its range. This engages on rows of the training data too, not
  only on extrapolation.
  ([\#285](https://github.com/tidymodels/tidypredict/issues/285))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now supports factor predictors for
  [`Cubist::cubist()`](http://topepo.github.io/Cubist/reference/cubist.default.md)
  models, which previously produced a formula that could not be
  evaluated (`object '"f"' not found`). Rule conditions are now read
  from the model text rather than from `model$splits`, which records
  neither the quoted column name nor a condition naming a single level,
  so such a rule silently applied to every row.
  ([\#322](https://github.com/tidymodels/tidypredict/issues/322))

- [`set_catboost_categories()`](https://tidypredict.tidymodels.org/reference/set_catboost_categories.md)
  now names every category of a `catboost` model, for any number of
  factor levels. It used to discover the hash CatBoost stores for a
  level by training probe models and reading back a split, which only
  worked reliably for a three-level factor; a factor with four or more
  levels errored with “No category mapping found for hash”, and a
  two-level one could silently name the levels the wrong way round.
  Hashes are now taken from CatBoost’s own hash function, and a level
  that cannot be named is reported at once rather than at fit time. This
  also affects
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  on a parsnip or bonsai `catboost` fit.
  ([\#297](https://github.com/tidymodels/tidypredict/issues/297))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now rejects an
  [`earth::earth()`](https://rdrr.io/pkg/earth/man/earth.html) model fit
  with a contrast other than the treatment one, with the same message
  the rest of the linear family gives. An ordered factor, which R fits
  with `contr.poly` by default, previously produced a formula comparing
  the factor column against contrast values such as `-0.2236`, which
  could not be evaluated. `earth` records no contrasts, so they are now
  read back off the names it gave the columns each factor expanded into.
  ([\#323](https://github.com/tidymodels/tidypredict/issues/323))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now rejects an `h2o` model fit with an algorithm other than GBM or
  RuleFit. Every h2o algorithm returns one of the three model classes
  tidypredict dispatches on, so nothing had been checking which one was
  used: `h2o.randomForest()` silently gave predictions that were wrong
  by a factor of the number of trees, because h2o averages tree
  predictions where the code summed them, and classification forests use
  vote proportions rather than a logistic link. The tree-free
  algorithms, among them `h2o.glm()`, `h2o.deeplearning()` and
  `h2o.naiveBayes()`, failed with the unhelpful “argument must be
  coercible to non-negative integer”.
  ([\#284](https://github.com/tidymodels/tidypredict/issues/284))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now rejects a [`MASS::lda()`](https://rdrr.io/pkg/MASS/man/lda.html),
  [`MASS::qda()`](https://rdrr.io/pkg/MASS/man/qda.html) or
  [`mda::fda()`](https://rdrr.io/pkg/mda/man/fda.html) model fit with a
  contrast other than the treatment one. None of the three records the
  contrasts it used, so the existing check was a no-op and an ordered
  factor, which R fits with `contr.poly` by default, silently produced
  wrong posterior probabilities: the level recovered from a column named
  `f.L` matches no row, so the term was dropped without complaint.
  ([\#343](https://github.com/tidymodels/tidypredict/issues/343))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now rejects an
  [`nnet::nnet()`](https://rdrr.io/pkg/nnet/man/nnet.html) model fit
  with the matrix interface instead of returning an unusable formula.
  Such a fit keeps neither `terms` nor `coefnames`, so the names of the
  predictors are lost and every reference to an input unit was written
  as `NULL`. The formula did not error: it evaluated to a zero length
  result. Refit the model with the formula interface.
  ([\#303](https://github.com/tidymodels/tidypredict/issues/303))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now routes missing values by each node’s `missing_type` for `lightgbm`
  models, matching [`predict()`](https://rdrr.io/r/stats/predict.html).
  LightGBM consults `default_left` only when `missing_type` is `NaN` or
  `Zero`; a feature with no missing value in the training data gets
  `None`, where a missing value is coerced to `0` and compared against
  the threshold like any other. Routing purely by `default_left` was
  wrong for every model trained without missing data, which is the
  common case.
  ([\#288](https://github.com/tidymodels/tidypredict/issues/288))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now honours `zero_as_missing` for `lightgbm` models, where an exact
  zero takes the same branch as a missing value. Predictions were wrong
  on the training data itself, not only on new zeros.
  ([\#288](https://github.com/tidymodels/tidypredict/issues/288))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  no longer sends a missing value down the left branch of a categorical
  split for `lightgbm` models. LightGBM sends it right whatever
  `default_left` says.
  ([\#288](https://github.com/tidymodels/tidypredict/issues/288))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  no longer returns `NaN` for every class probability of a row whose
  class scores are large, for any model whose prediction is a softmax:
  [`MASS::lda()`](https://rdrr.io/pkg/MASS/man/lda.html),
  [`MASS::qda()`](https://rdrr.io/pkg/MASS/man/qda.html),
  [`mda::fda()`](https://rdrr.io/pkg/mda/man/fda.html), `sparsediscrim`,
  `sda`, `mixOmics`,
  [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html),
  [`nnet::nnet()`](https://rdrr.io/pkg/nnet/man/nnet.html), multinomial
  `glmnet`, naive Bayes, `h2o`, `lightgbm` and `catboost`. The
  probabilities were written as `exp(s) / sum(exp(s))`, which is
  `Inf / Inf` once a score passes about 710. They are now written as
  `1 / sum(exp(s_j - s_k))`, which is the same quantity and cannot
  overflow.
  ([\#299](https://github.com/tidymodels/tidypredict/issues/299))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now rejects a `glmnet` model fit with an `offset` rather than silently
  dropping it, for both the single-outcome and the multinomial paths.
  glmnet records only whether an offset was used, never the values, and
  [`predict()`](https://rdrr.io/r/stats/predict.html) requires them
  again as `newoffset`, so the prediction cannot be reproduced.
  Predictions were previously wrong by the size of the offset.
  ([\#296](https://github.com/tidymodels/tidypredict/issues/296))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now rejects a
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  probability or survival forest rather than producing an unusable
  formula. Neither records a value per leaf, so a guard that read one
  let both through and emitted
  `case_when(x <= 0.0066 ~ NULL, .default = NULL)`, which failed later
  with an unrelated vctrs error;
  [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
  returned a parsed model with no predictions and no error at all. The
  forest type is now read from `treetype`.
  ([\#301](https://github.com/tidymodels/tidypredict/issues/301))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now honours an `mstop` reduced after fitting for `mboost` models, as
  `model[m]` does. Subsetting a fitted model, which is the standard
  `cvrisk()` workflow, sets `mstop` but leaves the stored ensemble at
  its full length, so every boosting iteration was used regardless.
  ([\#306](https://github.com/tidymodels/tidypredict/issues/306))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now sends a value sitting exactly on a split boundary the way the
  model does, for the backends that compare split thresholds as 32-bit
  floats: `xgboost`, `lightgbm`, `catboost`,
  [`Cubist::cubist()`](http://topepo.github.io/Cubist/reference/cubist.default.md)
  and
  [`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html).
  The boundary is the midpoint between the stored threshold and the
  adjacent float, and a value can land precisely on it, where rounding
  to a float is a tie broken towards the even mantissa. About half of
  all thresholds resolve that tie towards the neighbour rather than the
  threshold, and those sent such a value down the wrong branch.
  ([\#350](https://github.com/tidymodels/tidypredict/issues/350))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now handles a [`MASS::lda()`](https://rdrr.io/pkg/MASS/man/lda.html)
  or [`nnet::nnet()`](https://rdrr.io/pkg/nnet/man/nnet.html) model
  whose outcome factor has a level no observation fell in. Both drop the
  empty group when fitting but keep the full level set in `lev`, which
  the code used to name the classes, so
  [`MASS::lda()`](https://rdrr.io/pkg/MASS/man/lda.html) failed with
  “subscript out of bounds” and a classification
  [`nnet::nnet()`](https://rdrr.io/pkg/nnet/man/nnet.html) with “‘names’
  attribute \[4\] must be the same length as the vector \[3\]”. The
  classes are now read from the fitted quantities, which is what
  [`predict()`](https://rdrr.io/r/stats/predict.html) labels its output
  with. ([\#302](https://github.com/tidymodels/tidypredict/issues/302))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now honours `sigmoid` for `lightgbm` models fit with the `binary` or
  `multiclassova` objective, which apply `1 / (1 + exp(-sigmoid * x))`
  rather than a plain logistic. Every probability of a model fit with
  any other value was rescaled. `cross_entropy` accepts the parameter
  but never applies it, and is left alone.
  ([\#288](https://github.com/tidymodels/tidypredict/issues/288))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now honours `reg_sqrt` for `lightgbm` models, which trains on
  `sqrt(|y|)` keeping the sign and squares the raw score back onto the
  response scale. Predictions were left on the square-root scale, which
  can be further from
  [`predict()`](https://rdrr.io/r/stats/predict.html) than the response
  itself. The `huber` objective accepts the parameter but does not act
  on it, and is left alone.
  ([\#288](https://github.com/tidymodels/tidypredict/issues/288))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  and
  [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
  now work on an `xgboost` booster that has been saved and reloaded with
  [`xgb.save()`](https://rdrr.io/pkg/xgboost/man/xgb.save.html) /
  [`xgb.load()`](https://rdrr.io/pkg/xgboost/man/xgb.load.html). Such a
  booster was routed to the pre-2.0 code path and failed with
  `argument "model" is missing, with no default`, because the attribute
  used to tell the two APIs apart is set by
  [`xgb.train()`](https://rdrr.io/pkg/xgboost/man/xgb.train.html) but
  not by [`xgb.load()`](https://rdrr.io/pkg/xgboost/man/xgb.load.html).
  The objective is also recovered from the saved model now, which a
  reloaded booster records nowhere else; without it the raw margin was
  returned as though it were a probability, behind a warning about
  custom objectives.
  ([\#292](https://github.com/tidymodels/tidypredict/issues/292))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now combines the trials of a boosted
  [`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html)
  model with the confidence C5.0 votes with,
  `(freq + prior) / (n_leaf + 1)`, where `prior` is the class proportion
  at the root of that trial’s own tree. It used the Laplace ratio
  `(freq + 1) / (n_leaf + 2)` instead, which changed the predicted class
  for 72 of 720 swept configurations. A tie in the total vote now goes
  to the default class, as `SelectClass` does.
  ([\#287](https://github.com/tidymodels/tidypredict/issues/287))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  no longer reads C5.0’s `[ordered]` marker as part of the first level
  of an ordered predictor.
  ([\#287](https://github.com/tidymodels/tidypredict/issues/287))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now reports a
  [`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html)
  model that records no tree, rather than failing with “subscript out of
  bounds”. [`C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html)
  leaves the tree empty when fitting failed, which a predictor name or
  level containing `,` or `:` causes.
  ([\#287](https://github.com/tidymodels/tidypredict/issues/287))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now works for rank-deficient [`lm()`](https://rdrr.io/r/stats/lm.html)
  and [`glm()`](https://rdrr.io/r/stats/glm.html) models, which aborted
  with “Unable to calculate inverse of QR decomposition” even though it
  needs no QR decomposition at all. Two everyday shapes hit this: a
  duplicated predictor column, and a predictor with no variance. The
  aliased coefficients R leaves as `NA` are now dropped, as
  [`predict()`](https://rdrr.io/r/stats/predict.html) drops them, and
  the QR decomposition the prediction interval needs is built from the
  columns the fit actually identified, so
  [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md)
  keeps working for these models too.
  ([\#308](https://github.com/tidymodels/tidypredict/issues/308))

- [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md)
  now reports a parsed model that carries no QR decomposition, instead
  of failing with “Must supply `.init` when `.x` is empty”.
  ([\#308](https://github.com/tidymodels/tidypredict/issues/308))

- [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md)
  now works for [`glm()`](https://rdrr.io/r/stats/glm.html) models. It
  returned `numeric(0)` for every gaussian glm, because the residual
  variance was read from `summary()$sigma`, which only
  [`summary.lm()`](https://rdrr.io/r/stats/summary.lm.html) has;
  [`summary.glm()`](https://rdrr.io/r/stats/summary.glm.html) reports it
  as `dispersion`. `tidypredict_to_column(add_interval = TRUE)` errored
  as a result.
  ([\#293](https://github.com/tidymodels/tidypredict/issues/293))

- [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md)
  and
  [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md)
  now check that dbplyr is installed before using it, and are no longer
  marked as internal in the documentation index.
  ([\#314](https://github.com/tidymodels/tidypredict/issues/314))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now supports splits with more than two branches for `partykit` models,
  such as those from `ctree_control(multiway = TRUE)` or a
  [`partysplit()`](https://rdrr.io/pkg/partykit/man/partysplit.html)
  with several breaks. Every branch after the second was previously
  dropped, silently for a factor split and with a warning for a numeric
  one. ([\#295](https://github.com/tidymodels/tidypredict/issues/295))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now honours `partysplit(right = FALSE)` for `partykit` models, where
  the left branch is `x < break` rather than `x <= break`. A value
  falling exactly on the break took the wrong branch.
  ([\#295](https://github.com/tidymodels/tidypredict/issues/295))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now handles ordered factor predictors for `partykit` models, which
  previously errored with “Result must be length 1, not 2”. `partykit`
  splits an ordered factor with a break on the level’s integer code
  rather than with a set of levels.
  ([\#295](https://github.com/tidymodels/tidypredict/issues/295))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  no longer swaps the two branches of every
  [`partykit::party`](https://rdrr.io/pkg/partykit/man/party.html)
  converted from an `rpart` model.
  [`as.party.rpart()`](https://rdrr.io/pkg/partykit/man/party-coercion.html)
  maps the interval below the break to the second child, and the child
  order was read directly instead of through that mapping.
  ([\#295](https://github.com/tidymodels/tidypredict/issues/295))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now decodes factor splits for
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  models, in all three `respect.unordered.factors` modes and for ordered
  factors. The split value names a position in the level order stored on
  the model, or under `"partition"` lists the level indices going right;
  it was compared as a numeric threshold against the factor column
  itself.
  ([\#283](https://github.com/tidymodels/tidypredict/issues/283))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now decodes factor splits for
  [`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  models. An unordered factor’s split point is an integer whose bits
  name the levels going left, and an ordered factor’s is compared
  against the level’s integer code; both were read as a numeric
  threshold on the column itself, which silently produced `NA` or a
  wrong branch.
  ([\#282](https://github.com/tidymodels/tidypredict/issues/282))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  and
  [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
  now handle a stump, a tree with a single root node and no split, in a
  [`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  forest, instead of aborting with “argument of length 0”.
  [`randomForest::getTree()`](https://rdrr.io/pkg/randomForest/man/getTree.html)
  drops its node table to a vector for such a tree and then fails on its
  own `1:nrow()`, so the table is now assembled directly. A stump
  appears whenever the outcome is constant within a bootstrap sample,
  which a constant outcome or a zero-variance predictor makes routine.
  ([\#362](https://github.com/tidymodels/tidypredict/issues/362))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now skips a feature whose value is missing or whose factor level was
  not seen while fitting, for
  [`klaR::NaiveBayes()`](https://rdrr.io/pkg/klaR/man/NaiveBayes.html)
  and
  [`naivebayes::naive_bayes()`](https://majkamichal.github.io/naivebayes/reference/naive_bayes.html)
  models, matching both packages’
  [`predict()`](https://rdrr.io/r/stats/predict.html) instead of
  returning `NA` for the whole row. A row missing every predictor falls
  back on the class prior alone.
  ([\#300](https://github.com/tidymodels/tidypredict/issues/300))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  no longer errors with “missing value where TRUE/FALSE needed” for a
  [`naivebayes::naive_bayes()`](https://majkamichal.github.io/naivebayes/reference/naive_bayes.html)
  model with an outcome class of fewer than two observations. Such a
  class has no standard deviation, and the resulting `NA` probabilities
  now match [`predict()`](https://rdrr.io/r/stats/predict.html).
  ([\#300](https://github.com/tidymodels/tidypredict/issues/300))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now substitutes the training mean for a missing predictor in
  [`Cubist::cubist()`](http://topepo.github.io/Cubist/reference/cubist.default.md)
  models, matching [`predict()`](https://rdrr.io/r/stats/predict.html).
  The mean is read from the model text at the precision Cubist itself
  stores it, and is used in the rule conditions as well as in the linear
  models.
  ([\#294](https://github.com/tidymodels/tidypredict/issues/294))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now sends a missing predictor down the left branch for
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)
  models, matching [`predict()`](https://rdrr.io/r/stats/predict.html).
  `ranger` compares as `value > splitval`, which a missing value fails,
  so it takes the same branch as a value at or below the split point.
  ([\#294](https://github.com/tidymodels/tidypredict/issues/294))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now routes missing values through surrogate splits for
  [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html) models,
  and for
  [`baguette::bagger()`](https://baguette.tidymodels.org/reference/bagger.html)
  models using the `"CART"` base model, matching
  [`predict()`](https://rdrr.io/r/stats/predict.html) instead of sending
  every missing value down the right branch. All three `usesurrogate`
  modes are followed, including stopping at the node when no surrogate
  resolves the row and there is no majority to go with.
  ([\#294](https://github.com/tidymodels/tidypredict/issues/294))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns `NA` for a row that reaches a split on a predictor it is
  missing, for
  [`partykit::ctree()`](https://rdrr.io/pkg/partykit/man/ctree.html),
  [`partykit::cforest()`](https://rdrr.io/pkg/partykit/man/cforest.html)
  and
  [`mboost::blackboost()`](https://rdrr.io/pkg/mboost/man/blackboost.html)
  models. These backends resolve a missing value by randomly sampling
  the split probabilities, so
  [`predict()`](https://rdrr.io/r/stats/predict.html) returns a
  different answer on each call and there is no value to reproduce. A
  row whose path never reaches a split on the missing column is
  unaffected.
  ([\#294](https://github.com/tidymodels/tidypredict/issues/294))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns `NA` for a row with a missing predictor for
  [`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  and
  [`aorsf::orsf()`](https://docs.ropensci.org/aorsf/reference/orsf.html)
  models, rather than a confident value the model itself would never
  produce. `randomForest::predict()` returns `NA` for any incomplete row
  and `aorsf` refuses to predict from one at all, so there is no value
  to match. Rows are kept rather than dropped.
  ([\#294](https://github.com/tidymodels/tidypredict/issues/294),
  [\#325](https://github.com/tidymodels/tidypredict/issues/325))

- [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  now handles missing predictions instead of erroring with “missing
  value where TRUE/FALSE needed”. A row where both the model and
  tidypredict return `NA` counts as a match, and a row where only one of
  them does is reported as a mismatch, so the function can be used to
  check how a model behaves on missing data.
  ([\#309](https://github.com/tidymodels/tidypredict/issues/309))

- [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  now errors when given data with no rows, rather than reporting that
  all results are within the difference threshold.
  ([\#309](https://github.com/tidymodels/tidypredict/issues/309))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns correct predictions for
  [`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html)
  models whose predictor values fall on a split cut point. C5.0 compares
  cut points as 32-bit floats, so values between a cut and its float
  image were sent down the wrong branch.
  ([\#287](https://github.com/tidymodels/tidypredict/issues/287))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns correct predictions for `catboost` models whose predictor
  values fall on a split border. catboost compares borders as 32-bit
  floats, so a value a fraction above a border was sent down the wrong
  branch.
  ([\#298](https://github.com/tidymodels/tidypredict/issues/298))

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

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now picks the right factor predictor when three or more variable names
  are nested prefixes of one another, such as `x`, `xy` and `xyz`. The
  longest match was selected by indexing with
  [`rank()`](https://rdrr.io/r/base/rank.html), which silently chose the
  wrong variable and produced wrong predictions for
  [`lm()`](https://rdrr.io/r/stats/lm.html),
  [`glm()`](https://rdrr.io/r/stats/glm.html),
  [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html),
  [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html),
  [`nnet::nnet()`](https://rdrr.io/pkg/nnet/man/nnet.html) and
  [`earth::earth()`](https://rdrr.io/pkg/earth/man/earth.html).
  ([\#290](https://github.com/tidymodels/tidypredict/issues/290))

- Added support for
  [`baguette::bagger()`](https://baguette.tidymodels.org/reference/bagger.html)
  bagged tree ensembles fit with the `"CART"` or `"C5.0"` base model,
  including
  [`bag_tree()`](https://parsnip.tidymodels.org/reference/bag_tree.html)
  parsnip models fitted with the `"rpart"` or `"C5.0"` engine.
  Regression predictions average the individual trees, and
  classification predictions return the class with the largest average
  class probability.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now supports
  [`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html)
  models that split a discrete predictor into one branch per level.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now uses a strict inequality (`<`) for the continuous splits of
  [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html) models,
  matching how `rpart` assigns values that are exactly equal to a cut
  point. ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns correct predictions for
  [`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html)
  models that have been saved and reloaded with
  [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
  and
  [`as_parsed_model()`](https://tidypredict.tidymodels.org/reference/as_parsed_model.md).
  Every split variable after the first leaf in a tree was named
  incorrectly, so the reloaded model split on the wrong columns.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns correct predictions for
  [`Cubist::cubist()`](http://topepo.github.io/Cubist/reference/cubist.default.md)
  models whose predictor values fall exactly on a split threshold.
  Cubist compares split thresholds as 32-bit floats, so a `disp` of 95.1
  was sent down the wrong branch when the comparison was made in R’s
  doubles.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  now supports
  [`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html)
  models, including boosted and rule-based ones.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md)
  now honours its `interval` argument. It was hardcoded to 0.95, so
  [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md),
  `tidypredict_to_column(add_interval = TRUE)`, and
  [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md)
  all returned a 95% interval regardless of what was asked for.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md)
  now reports an unsupported model class with a message naming the
  class, rather than R’s default “no applicable method” error.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)
  now explains that a model returning more than one formula is
  unsupported, instead of incorrectly claiming that tree based models
  are unsupported.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now reports an unsupported model class with a message naming the
  class, rather than R’s default “no applicable method” error.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now keeps small probabilities for models with a logit link, such as
  [`glm()`](https://rdrr.io/r/stats/glm.html) with `family = binomial`
  and
  [`LiblineaR::LiblineaR()`](https://rdrr.io/pkg/LiblineaR/man/LiblineaR.html).
  The inverse link was written in a form that rounded to exactly 0 once
  the linear predictor fell below about -37.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for
  [`dbarts::bart()`](https://rdrr.io/pkg/dbarts/man/bart.html) Bayesian
  additive regression trees, including
  [`bart()`](https://parsnip.tidymodels.org/reference/bart.html) parsnip
  models fitted with the `"dbarts"` engine. The model has to be fit with
  `keeptrees = TRUE`, and only continuous outcomes are supported since
  binary outcomes are fit with a probit link.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for
  [`klaR::NaiveBayes()`](https://rdrr.io/pkg/klaR/man/NaiveBayes.html)
  naive Bayes models with Gaussian densities (`usekernel = FALSE`),
  including
  [`naive_Bayes()`](https://parsnip.tidymodels.org/reference/naive_Bayes.html)
  parsnip models fitted with the `"klaR"` engine.
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  returns a named list of class-probability expressions (softmax of the
  summed log densities), and
  [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  is not supported for these multiclass models.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for
  [`naivebayes::naive_bayes()`](https://majkamichal.github.io/naivebayes/reference/naive_bayes.html)
  naive Bayes models fit without kernel density estimates
  (`usekernel = FALSE`), including Gaussian, categorical, Bernoulli, and
  Poisson conditional distributions, and
  [`naive_Bayes()`](https://parsnip.tidymodels.org/reference/naive_Bayes.html)
  parsnip models fitted with the `"naivebayes"` engine.
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  returns a named list of class-probability expressions, and
  [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  is not supported for these multiclass models.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for
  [`parsnip::nullmodel()`](https://parsnip.tidymodels.org/reference/nullmodel.html)
  models, including
  [`null_model()`](https://parsnip.tidymodels.org/reference/null_model.html)
  parsnip models fitted with the `"parsnip"` engine. Regression models
  return the outcome mean as a single expression, and classification
  models return a named list of constant class-probability expressions,
  for which
  [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  is not supported.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for the partial least squares models in `mixOmics`
  ([`pls()`](https://parsnip.tidymodels.org/reference/pls.html),
  `spls()`, `plsda()`, and `splsda()`), including
  [`pls()`](https://parsnip.tidymodels.org/reference/pls.html) parsnip
  models fitted with the `"mixOmics"` engine, for regression and
  classification. Single-outcome regression models return one
  expression, multivariate outcomes return a named list of expressions,
  and the discriminant variants return a named list of class-probability
  expressions (softmax), for which
  [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  is not supported.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for multinomial
  [`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html)
  models (`family = "multinomial"`), including
  [`multinom_reg()`](https://parsnip.tidymodels.org/reference/multinom_reg.html)
  parsnip models fitted with the `"glmnet"` engine.
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  returns a named list of class-probability expressions (softmax), and
  [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  is not supported for these multiclass models.
  ([\#198](https://github.com/tidymodels/tidypredict/issues/198))

- Added support for
  [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html)
  multinomial log-linear models, including
  [`multinom_reg()`](https://parsnip.tidymodels.org/reference/multinom_reg.html)
  parsnip models fitted with the `"nnet"` engine.
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  returns a named list of class-probability expressions (softmax), and
  [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  is not supported for these multiclass models.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for
  [`nnet::nnet()`](https://rdrr.io/pkg/nnet/man/nnet.html) single hidden
  layer neural networks, including
  [`mlp()`](https://parsnip.tidymodels.org/reference/mlp.html) parsnip
  models fitted with the `"nnet"` engine, for regression and
  classification. Regression models return a single expression, and
  classification models return a named list of class-probability
  expressions, for which
  [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  is not supported.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for [`sda::sda()`](https://rdrr.io/pkg/sda/man/sda.html)
  shrinkage discriminant analysis models, including
  [`discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.html)
  parsnip models fitted with the `"sda"` engine.
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  returns a named list of class-probability expressions (softmax), and
  [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  is not supported for these multiclass models.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for the regularized linear discriminant analysis models
  in `sparsediscrim` (`lda_diag()`, `lda_shrink_mean()`,
  `lda_shrink_cov()`, and `lda_emp_bayes_eigen()`), including
  [`discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.html)
  parsnip models fitted with the `"sparsediscrim"` engine.
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  returns a named list of class-probability expressions (softmax of the
  per-class discriminant scores), and
  [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  is not supported for these multiclass models.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for [`mda::fda()`](https://rdrr.io/pkg/mda/man/fda.html)
  flexible discriminant analysis models fit with a linear regression
  method ([`mda::polyreg()`](https://rdrr.io/pkg/mda/man/polyreg.html)
  with `degree = 1` or
  [`mda::gen.ridge()`](https://rdrr.io/pkg/mda/man/gen.ridge.html)),
  including
  [`discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.html)
  parsnip models fitted with the `"mda"` engine.
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  returns a named list of class-probability expressions (softmax of the
  per-class discriminant scores), and
  [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  is not supported for these multiclass models.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for
  [`MASS::lda()`](https://rdrr.io/pkg/MASS/man/lda.html) linear
  discriminant analysis models, including
  [`discrim_linear()`](https://parsnip.tidymodels.org/reference/discrim_linear.html)
  parsnip models fitted with the `"MASS"` engine.
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  returns a named list of class-probability expressions (softmax of the
  per-class discriminant scores), and
  [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  is not supported for these multiclass models.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for
  [`MASS::qda()`](https://rdrr.io/pkg/MASS/man/qda.html) quadratic
  discriminant analysis models, including
  [`discrim_quad()`](https://parsnip.tidymodels.org/reference/discrim_quad.html)
  parsnip models fitted with the `"MASS"` engine.
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  returns a named list of class-probability expressions (softmax of the
  per-class quadratic discriminant scores), and
  [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  is not supported for these multiclass models.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for
  [`kernlab::ksvm()`](https://rdrr.io/pkg/kernlab/man/ksvm.html) linear
  support vector machine models (`vanilladot` kernel), including
  [`svm_linear()`](https://parsnip.tidymodels.org/reference/svm_linear.html)
  parsnip models fitted with the `"kernlab"` engine, for regression and
  binary classification. Non-linear kernels and multiclass
  classification are not supported, and classification requires a
  probability model (`prob.model = TRUE`).
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for H2O gradient boosting models (`H2ORegressionModel`,
  `H2OBinomialModel`, and `H2OMultinomialModel`), including
  [`boost_tree()`](https://parsnip.tidymodels.org/reference/boost_tree.html)
  parsnip models fitted with the `"h2o_gbm"` engine, for regression and
  classification. Only GBM models are supported (not H2O’s XGBoost),
  predictions require a running H2O cluster, and gaussian, bernoulli,
  and multinomial distributions are supported.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for H2O RuleFit models
  ([`h2o::h2o.rulefit()`](https://rdrr.io/pkg/h2o/man/h2o.rulefit.html)),
  including
  [`rule_fit()`](https://parsnip.tidymodels.org/reference/rule_fit.html)
  parsnip models fitted with the `"h2o"` engine, for regression and
  binary classification. Predictions require a running H2O cluster, and
  multiclass models are not supported because `h2o.rule_importance()`
  does not expose the per-class coefficients.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for
  [`mboost::blackboost()`](https://rdrr.io/pkg/mboost/man/blackboost.html)
  gradient boosting regression models, including
  [`boost_tree()`](https://parsnip.tidymodels.org/reference/boost_tree.html)
  parsnip models fitted with the `"mboost"` engine. Only the
  `Gaussian()` family is supported.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for
  [`aorsf::orsf()`](https://docs.ropensci.org/aorsf/reference/orsf.html)
  oblique random forest regression models, including
  [`rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
  parsnip models fitted with the `"aorsf"` engine. Only numeric
  predictors are supported and classification is not supported.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for
  [`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html)
  classification tree models, including
  [`decision_tree()`](https://parsnip.tidymodels.org/reference/decision_tree.html)
  and
  [`boost_tree()`](https://parsnip.tidymodels.org/reference/boost_tree.html)
  parsnip models fitted with the `"C5.0"` engine. Boosted models
  (`trials > 1`) combine trials by confidence-weighted voting. Fuzzy
  thresholds (`fuzzyThreshold = TRUE`) and cost matrices (`costs`) are
  not supported.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for rule-based
  [`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html)
  classification models (`rules = TRUE`), including
  [`C5_rules()`](https://parsnip.tidymodels.org/reference/C5_rules.html)
  parsnip models fitted with the `"C5.0"` engine. Boosted rule-based
  models (`trials > 1`) are not supported.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for
  [`partykit::cforest()`](https://rdrr.io/pkg/partykit/man/cforest.html)
  random forest regression models, including
  [`rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
  parsnip models fitted with the `"partykit"` engine. Classification is
  not supported.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for
  [`LiblineaR::LiblineaR()`](https://rdrr.io/pkg/LiblineaR/man/LiblineaR.html)
  binary logistic regression models (`type` 0, 6, 7), including
  [`logistic_reg()`](https://parsnip.tidymodels.org/reference/logistic_reg.html)
  parsnip models fitted with the `"LiblineaR"` engine. Also added
  support for linear support vector machine models, including
  [`svm_linear()`](https://parsnip.tidymodels.org/reference/svm_linear.html)
  parsnip models fitted with the `"LiblineaR"` engine, for regression
  (`type` 11, 12, 13) and binary classification (`type` 1-5).
  Classification returns the SVM decision value rather than a
  probability.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for [`xrf::xrf()`](https://rdrr.io/pkg/xrf/man/xrf.html)
  rule-based models (RuleFit), including
  [`rule_fit()`](https://parsnip.tidymodels.org/reference/rule_fit.html)
  parsnip models fitted with the `"xrf"` engine, for regression
  (`family = "gaussian"`) and binary classification
  (`family = "binomial"`). Multinomial models are not supported.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for
  [`decision_tree()`](https://parsnip.tidymodels.org/reference/decision_tree.html)
  parsnip models fitted with the `"rpart"` engine.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for
  [`linear_reg()`](https://parsnip.tidymodels.org/reference/linear_reg.html)
  parsnip models fitted with the `"glm"` engine.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- Added support for
  [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html) quantile
  regression models, including
  [`linear_reg()`](https://parsnip.tidymodels.org/reference/linear_reg.html)
  parsnip models fitted with the `"quantreg"` engine. Models fitted with
  multiple quantiles return one fit expression per quantile, named by
  the quantile level.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns correct predictions for xgboost models whose feature
  values fall exactly on a split threshold. xgboost compares split
  thresholds as 32-bit floats, so a value such as a `wt` of 3.19 was
  sent down the wrong branch when the comparison was made in R’s
  doubles.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  now returns correct predictions for xgboost models that have been
  saved and reloaded with
  [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md)
  and
  [`as_parsed_model()`](https://tidypredict.tidymodels.org/reference/as_parsed_model.md).
  Previously every tree collapsed to a single leaf value.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  now flags rows where the fitted value is above the model’s own
  prediction for xgboost models. Previously only differences in one
  direction were reported, so real disagreements could go unnoticed.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  now reports the maximum fit, lower, and upper differences under the
  correct labels, and reports absolute rather than signed differences.
  Previously the fit and upper values were swapped, and the fit value
  was omitted entirely when `include_intervals = FALSE`.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  now reports a failure message for multiclass CatBoost models when
  results exceed the threshold. Previously it always claimed that all
  results were within the threshold, even when `alert` was `TRUE`.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  now compares ranger models against
  [`predict()`](https://rdrr.io/r/stats/predict.html). Previously the
  comparison silently measured tidypredict’s predictions against
  themselves and so always reported a difference of zero.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  now reports an absolute maximum difference for glmnet models, which
  could previously be negative.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  now names the model’s own predictions `fit` in `raw_results` for
  XGBoost, LightGBM, CatBoost and h2o models, matching every other model
  type. The column was previously called `base`.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  now reports `fit_diff` as a signed difference for LightGBM, CatBoost
  and h2o models, so the direction of the error is visible. The
  threshold is applied to its absolute value, as before.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

- [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  results for classification models are now reported consistently:
  `fit_diff` is a 0/1 indicator, the threshold is reported as 0 since
  labels are compared exactly, and the message counts records that do
  not match rather than quoting a maximum difference.
  ([\#232](https://github.com/tidymodels/tidypredict/issues/232))

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

- Fixed bug where Cubst models incorrectly combined rules and
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

- Change to with with version 5.1.2 and above of the `earth` package. As
  a result, `tidypredict` will only parse objects created by this and
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
