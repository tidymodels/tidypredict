# tidypredict (development version)

- The Cubist article now documents two limits on how closely `tidypredict_fit()` can match `Cubist::predict()`. The instance-based correction that `predict()` applies when `neighbors` is greater than zero is not reproduced, because it adjusts each prediction using training rows that are not part of the fitted model. Separately, Cubist stores its coefficients as 32-bit floats, so the agreement has a relative ceiling near 1e-7 rather than an absolute one, and an outcome on a large scale leaves a proportionally large absolute difference. (#375)

- The glm article now documents the one inverse link `tidypredict_fit()` does not reproduce exactly: `probit`, whose inverse is `pnorm()`, is written as the Bowling et al. logistic approximation to the normal CDF because no SQL backend has a normal CDF. It costs about 1e-4 of probability, which is enough for `tidypredict_test()` to report a probit model as failing at its default threshold. (#355)

- The naive Bayes article now documents the one case where `tidypredict_fit()` does not reproduce `predict()` for `klaR::NaiveBayes()` and `naivebayes::naive_bayes()` models: both replace a normal density that underflowed to zero with their `threshold` argument, which takes a value roughly 38 standard deviations from the class mean, and the log scale used throughout never underflows. (#300)

- `acceptable_formula()` now checks the contrast of every factor predictor. A model that used the treatment contrast for one field and something else for another was accepted and then silently mis-parsed; such a model now aborts with the usual "the treatment contrast is the only one supported" error, which also names the offending field rather than the contrast. (#291)

- `acceptable_formula()` and `parse_model()` now report a model class they do not support, rather than failing with R's "no applicable method" error. (#313)

- `as_parsed_model()` now rejects an object that is not a parsed model. A list without a `general$type` element was given a class of `pm_` that no method matches, so the failure surfaced much later and said nothing about the real problem. (#313)

- `tidypredict_fit()` now works on a LightGBM model whose trees are bare leaves, which is what LightGBM emits when it cannot make a single split, such as with a constant outcome, a single training row, or a lone factor predictor whose splits the categorical guards reject. `lightgbm::lgb.model.dt.tree()` reports no rows at all for such a tree, so the model parsed to no trees and failed with "Model has no trees."; the leaf values are now read from the model's JSON dump. A multiclass model in which only some trees are bare leaves silently assigned trees to the wrong classes, and now matches `predict()`. (#401)

- `tidypredict_fit()` now returns predictions on the response scale for CatBoost models fit with the `Poisson` or `Tweedie` objective, applying `exp()` to the raw score as the other CatBoost objectives already invert their own links. Anyone using such a model will see their predictions change from the log scale to the count or mean scale; they now match `catboost.predict(prediction_type = "Exponent")` instead of the `"RawFormulaVal"` default. (#356)

- `tidypredict_fit()` now sends a split threshold that is not finite, or that overflows the 32-bit float range, down the branch the model does. Such a threshold was moved to a boundary of `NaN`, which makes every comparison `FALSE`, so the model silently mispredicted. (#313)

- `tidypredict_fit()` now works on a parsed LightGBM model fit with `linear_tree = TRUE`. A leaf of a linear tree stores its coefficients separately and leaves its constant prediction empty, which the parsed path never read, so the formula failed with "`..1 (right)` must be a vector, not `NULL`". This also affected such a model saved with `tidypredict_save()` and read back with `tidypredict_load()`. (#346)

- `tidypredict_interval()` now rejects an `interval` that is not a single number strictly between 0 and 1. An `interval` of 1.5 gave a formula beginning with `NaN`, so every prediction bound came back missing. (#313)

- `tidypredict_interval()` now reports a parsed model of a type it does not support with the same message it gives for a fitted model, rather than "Model type not supported.", and reports a list that is not a parsed model rather than failing with "argument is of length zero". (#313)

- `tidypredict_sql()` now returns a single query for an intercept-only model. Such a model's formula is a bare number rather than a call, which was mistaken for the list of formulas a multiclass model produces, so the query came back wrapped in a one element list. (#313)

- `tidypredict_to_column()` now validates `vars`, `add_interval` and `interval`. Passing fewer than three names in `vars` alongside `add_interval = TRUE` produced a data frame with a column literally named `NA`. (#313)

- `tidypredict_fit()` now produces a formula R can evaluate for a `dbarts::bart()` fit at the package default `ntree`. Terms are summed left to right, which nests the `+` calls as deeply as there are terms, and a bart fit sums `ndpost * ntree` leaf values: at the defaults R gave up with "evaluation nested too deeply". A model with 1000 terms or more is now summed in a balanced shape instead, nesting `log2(n)` deep. Only a large ensemble reaches that, so every other model keeps the flat left-to-right sum it had before, along with the exact result and the formula layout that go with it. (#305)

- `.build_case_when_tree()`, which {orbital} calls, now returns the bare prediction of a stump tree whether that prediction is a number or a class label. A classification stump previously produced `case_when(.default = "a")`, which dplyr rejects with "`...` can't be empty". (#310)

- `tidypredict_fit()` no longer returns `NULL` for a parsed model saved by tidypredict 1.0.1 or earlier that came from a `partykit` or `rpart` single tree. The handler for single trees was removed as apparently dead code, leaving those models to fall off the end of a whitelist, so `tidypredict_to_column()` returned the data frame unchanged and `tidypredict_sql()` returned an empty list. Any parsed model type that is still unhandled now raises an error rather than returning `NULL`. (#304)

- `tidypredict_fit()` no longer fails with "`x` must be a formula" on a parsed model saved by tidypredict 1.0.1 or earlier that contains a `ranger::ranger()` or `randomForest::randomForest()` stump, a tree whose root is its only node. Such a tree is now written as its constant prediction. (#310)

- `tidypredict_fit()` now handles three parsed model shapes that no released `parse_model()` writes but that a hand-written or edited parsed model can contain: a path that mixes a `type = "all"` element with real conditions, which aborted with an internal error; a rule whose linear prediction is a single non-intercept term, which produced a garbled formula; and a rule whose terms are all zero, which aborted with "`.x` must not be empty" and is now written as `0`. (#310)

- `tidypredict_fit()` now returns correct predictions for `kernlab::ksvm()` models with a single numeric predictor, which previously produced a bare constant. kernlab leaves the column names of a one-column model matrix empty, so every term was dropped and only the intercept remained. (#289)

- `tidypredict_fit()` now undoes kernlab's predictor scaling when exactly one column was scaled for `kernlab::ksvm()` models. This covers any fit with one numeric predictor plus factor predictors, since kernlab does not scale dummy columns, and the weights were left on the scaled scale because the centers and scales lose their names in that case. (#289)

- `tidypredict_fit()` now assigns rules to the right committee for `Cubist::cubist()` models fitted with more than 20 committees. The committee each rule belonged to was scraped from the printed model, whose "Number of rules per committee" line is truncated at 20 committees, so the rules beyond that point were recycled across the wrong committees and the average was taken over 20 committees instead of the number requested. (#286)

- `tidypredict_fit()` now applies the per-rule extrapolation limits for `Cubist::cubist()` models. Cubist holds each rule to the span of the training outcomes it covers, widened at both ends by `extrap` times that span and never crossing zero; without it a rule's linear model runs away on data outside its range. This engages on rows of the training data too, not only on extrapolation. (#285)

- `tidypredict_fit()` now supports factor predictors for `Cubist::cubist()` models, which previously produced a formula that could not be evaluated (`object '"f"' not found`). Rule conditions are now read from the model text rather than from `model$splits`, which records neither the quoted column name nor a condition naming a single level, so such a rule silently applied to every row. (#322)

- `tidypredict_fit()` now reads the coefficient labels of an `lm()`, `glm()` or `quantreg::rq()` model from the model's own term structure rather than from the spelling of the label. A factor level containing a `:` was taken apart as if it were an interaction, giving a formula that could not be evaluated, and a label that happened to equal another predictor's name was read as that predictor, silently giving wrong predictions. A label that still cannot be resolved to one combination of levels is now reported instead of guessed at. (#308)

- `tidypredict_fit()` now reads coefficient labels from the model's own term structure for `nnet::multinom()`, `nnet::nnet()`, `kernlab::ksvm()`, `MASS::lda()`, `MASS::qda()`, `mda::fda()` and `sda::sda()` models too, extending the fix that landed for `lm()`, `glm()` and `quantreg::rq()`. A dummy column whose name happened to equal another predictor's name was read as that predictor, silently giving wrong predictions: a `y ~ g + gy2` fit where the factor `g` has a level `y2` was out by a full unit of probability. The levels are worked out from how many columns each term expanded into for the models that record no `xlevels`, which also fixes `kernlab::ksvm()` fits whose duplicate model matrix column names were made unique. (#376)

- `set_catboost_categories()` now names every category of a `catboost` model, for any number of factor levels. It used to discover the hash CatBoost stores for a level by training probe models and reading back a split, which only worked reliably for a three-level factor; a factor with four or more levels errored with "No category mapping found for hash", and a two-level one could silently name the levels the wrong way round. Hashes are now taken from CatBoost's own hash function, and a level that cannot be named is reported at once rather than at fit time. This also affects `tidypredict_fit()` on a parsnip or bonsai `catboost` fit. (#297)

- `tidypredict_fit()` now rejects an `earth::earth()` model fit with a contrast other than the treatment one, with the same message the rest of the linear family gives. An ordered factor, which R fits with `contr.poly` by default, previously produced a formula comparing the factor column against contrast values such as `-0.2236`, which could not be evaluated. `earth` records no contrasts, so they are now read back off the names it gave the columns each factor expanded into. (#323)

- `tidypredict_fit()` now rejects an `h2o` model fit with an algorithm other than GBM or RuleFit. Every h2o algorithm returns one of the three model classes tidypredict dispatches on, so nothing had been checking which one was used: `h2o.randomForest()` silently gave predictions that were wrong by a factor of the number of trees, because h2o averages tree predictions where the code summed them, and classification forests use vote proportions rather than a logistic link. The tree-free algorithms, among them `h2o.glm()`, `h2o.deeplearning()` and `h2o.naiveBayes()`, failed with the unhelpful "argument must be coercible to non-negative integer". (#284)

- `tidypredict_fit()` now rejects a `MASS::lda()`, `MASS::qda()` or `mda::fda()` model fit with a contrast other than the treatment one. None of the three records the contrasts it used, so the existing check was a no-op and an ordered factor, which R fits with `contr.poly` by default, silently produced wrong posterior probabilities: the level recovered from a column named `f.L` matches no row, so the term was dropped without complaint. (#343)

- `tidypredict_fit()` now rejects an `nnet::nnet()` model fit with the matrix interface instead of returning an unusable formula. Such a fit keeps neither `terms` nor `coefnames`, so the names of the predictors are lost and every reference to an input unit was written as `NULL`. The formula did not error: it evaluated to a zero length result. Refit the model with the formula interface. (#303)

- `tidypredict_fit()` now routes missing values by each node's `missing_type` for `lightgbm` models, matching `predict()`. LightGBM consults `default_left` only when `missing_type` is `NaN` or `Zero`; a feature with no missing value in the training data gets `None`, where a missing value is coerced to `0` and compared against the threshold like any other. Routing purely by `default_left` was wrong for every model trained without missing data, which is the common case. (#288)

- `tidypredict_fit()` now honours `zero_as_missing` for `lightgbm` models, where an exact zero takes the same branch as a missing value. Predictions were wrong on the training data itself, not only on new zeros. (#288)

- `tidypredict_fit()` no longer sends a missing value down the left branch of a categorical split for `lightgbm` models. LightGBM sends it right whatever `default_left` says. (#288)

- `tidypredict_fit()` no longer returns `NaN` for every class probability of a row whose class scores are large, for any model whose prediction is a softmax: `MASS::lda()`, `MASS::qda()`, `mda::fda()`, `sparsediscrim`, `sda`, `mixOmics`, `nnet::multinom()`, `nnet::nnet()`, multinomial `glmnet`, naive Bayes, `h2o`, `lightgbm` and `catboost`. The probabilities were written as `exp(s) / sum(exp(s))`, which is `Inf / Inf` once a score passes about 710. They are now written as `1 / sum(exp(s_j - s_k))`, which is the same quantity and cannot overflow. (#299)

- `tidypredict_fit()` now rejects a `glmnet` model fit with an `offset` rather than silently dropping it, for both the single-outcome and the multinomial paths. glmnet records only whether an offset was used, never the values, and `predict()` requires them again as `newoffset`, so the prediction cannot be reproduced. Predictions were previously wrong by the size of the offset. (#296)

- `tidypredict_fit()` now rejects a `ranger::ranger()` probability or survival forest rather than producing an unusable formula. Neither records a value per leaf, so a guard that read one let both through and emitted `case_when(x <= 0.0066 ~ NULL, .default = NULL)`, which failed later with an unrelated vctrs error; `parse_model()` returned a parsed model with no predictions and no error at all. The forest type is now read from `treetype`. (#301)

- `tidypredict_fit()` now honours an `mstop` reduced after fitting for `mboost` models, as `model[m]` does. Subsetting a fitted model, which is the standard `cvrisk()` workflow, sets `mstop` but leaves the stored ensemble at its full length, so every boosting iteration was used regardless. (#306)

- `tidypredict_fit()` now sends a value sitting exactly on a split boundary the way the model does, for the backends that compare split thresholds as 32-bit floats: `xgboost`, `lightgbm`, `catboost`, `Cubist::cubist()` and `C50::C5.0()`. The boundary is the midpoint between the stored threshold and the adjacent float, and a value can land precisely on it, where rounding to a float is a tie broken towards the even mantissa. About half of all thresholds resolve that tie towards the neighbour rather than the threshold, and those sent such a value down the wrong branch. (#350)

- `tidypredict_fit()` now handles a `MASS::lda()` or `nnet::nnet()` model whose outcome factor has a level no observation fell in. Both drop the empty group when fitting but keep the full level set in `lev`, which the code used to name the classes, so `MASS::lda()` failed with "subscript out of bounds" and a classification `nnet::nnet()` with "'names' attribute [4] must be the same length as the vector [3]". The classes are now read from the fitted quantities, which is what `predict()` labels its output with. (#302)

- `tidypredict_fit()` now honours `sigmoid` for `lightgbm` models fit with the `binary` or `multiclassova` objective, which apply `1 / (1 + exp(-sigmoid * x))` rather than a plain logistic. Every probability of a model fit with any other value was rescaled. `cross_entropy` accepts the parameter but never applies it, and is left alone. (#288)

- `tidypredict_fit()` now honours `reg_sqrt` for `lightgbm` models, which trains on `sqrt(|y|)` keeping the sign and squares the raw score back onto the response scale. Predictions were left on the square-root scale, which can be further from `predict()` than the response itself. The `huber` objective accepts the parameter but does not act on it, and is left alone. (#288)

- `tidypredict_fit()` and `parse_model()` now work on an `xgboost` booster that has been saved and reloaded with `xgb.save()` / `xgb.load()`. Such a booster was routed to the pre-2.0 code path and failed with `argument "model" is missing, with no default`, because the attribute used to tell the two APIs apart is set by `xgb.train()` but not by `xgb.load()`. The objective is also recovered from the saved model now, which a reloaded booster records nowhere else; without it the raw margin was returned as though it were a probability, behind a warning about custom objectives. (#292)

- `tidypredict_fit()` now combines the trials of a boosted `C50::C5.0()` model with the confidence C5.0 votes with, `(freq + prior) / (n_leaf + 1)`, where `prior` is the class proportion at the root of that trial's own tree. It used the Laplace ratio `(freq + 1) / (n_leaf + 2)` instead, which changed the predicted class for 72 of 720 swept configurations. A tie in the total vote now goes to the default class, as `SelectClass` does. (#287)

- `tidypredict_fit()` no longer reads C5.0's `[ordered]` marker as part of the first level of an ordered predictor. (#287)

- `tidypredict_fit()` now reports a `C50::C5.0()` model that records no tree, rather than failing with "subscript out of bounds". `C5.0()` leaves the tree empty when fitting failed, which a predictor name or level containing `,` or `:` causes. (#287)

- `tidypredict_fit()` now works for rank-deficient `lm()` and `glm()` models, which aborted with "Unable to calculate inverse of QR decomposition" even though it needs no QR decomposition at all. Two everyday shapes hit this: a duplicated predictor column, and a predictor with no variance. The aliased coefficients R leaves as `NA` are now dropped, as `predict()` drops them, and the QR decomposition the prediction interval needs is built from the columns the fit actually identified, so `tidypredict_interval()` keeps working for these models too. (#308)

- `tidypredict_interval()` now reports a parsed model that carries no QR decomposition, instead of failing with "Must supply `.init` when `.x` is empty". (#308)

- `tidypredict_interval()` now works for `glm()` models. It returned `numeric(0)` for every gaussian glm, because the residual variance was read from `summary()$sigma`, which only `summary.lm()` has; `summary.glm()` reports it as `dispersion`. `tidypredict_to_column(add_interval = TRUE)` errored as a result. (#293)

- `tidypredict_sql()` and `tidypredict_sql_interval()` now check that dbplyr is installed before using it, and are no longer marked as internal in the documentation index. (#314)

- `tidypredict_fit()` now supports splits with more than two branches for `partykit` models, such as those from `ctree_control(multiway = TRUE)` or a `partysplit()` with several breaks. Every branch after the second was previously dropped, silently for a factor split and with a warning for a numeric one. (#295)

- `tidypredict_fit()` now honours `partysplit(right = FALSE)` for `partykit` models, where the left branch is `x < break` rather than `x <= break`. A value falling exactly on the break took the wrong branch. (#295)

- `tidypredict_fit()` now handles ordered factor predictors for `partykit` models, which previously errored with "Result must be length 1, not 2". `partykit` splits an ordered factor with a break on the level's integer code rather than with a set of levels. (#295)

- `tidypredict_fit()` no longer swaps the two branches of every `partykit::party` converted from an `rpart` model. `as.party.rpart()` maps the interval below the break to the second child, and the child order was read directly instead of through that mapping. (#295)

- `tidypredict_fit()` now decodes factor splits for `ranger::ranger()` models, in all three `respect.unordered.factors` modes and for ordered factors. The split value names a position in the level order stored on the model, or under `"partition"` lists the level indices going right; it was compared as a numeric threshold against the factor column itself. (#283)

- `tidypredict_fit()` now decodes factor splits for `randomForest::randomForest()` models. An unordered factor's split point is an integer whose bits name the levels going left, and an ordered factor's is compared against the level's integer code; both were read as a numeric threshold on the column itself, which silently produced `NA` or a wrong branch. (#282)

- `tidypredict_fit()` and `parse_model()` now handle a stump, a tree with a single root node and no split, in a `randomForest::randomForest()` forest, instead of aborting with "argument of length 0". `randomForest::getTree()` drops its node table to a vector for such a tree and then fails on its own `1:nrow()`, so the table is now assembled directly. A stump appears whenever the outcome is constant within a bootstrap sample, which a constant outcome or a zero-variance predictor makes routine. (#362)

- `tidypredict_fit()` now skips a feature whose value is missing or whose factor level was not seen while fitting, for `klaR::NaiveBayes()` and `naivebayes::naive_bayes()` models, matching both packages' `predict()` instead of returning `NA` for the whole row. A row missing every predictor falls back on the class prior alone. (#300)

- `tidypredict_fit()` no longer errors with "missing value where TRUE/FALSE needed" for a `naivebayes::naive_bayes()` model with an outcome class of fewer than two observations. Such a class has no standard deviation, and the resulting `NA` probabilities now match `predict()`. (#300)

- `tidypredict_fit()` now substitutes the training mean for a missing predictor in `Cubist::cubist()` models, matching `predict()`. The mean is read from the model text at the precision Cubist itself stores it, and is used in the rule conditions as well as in the linear models. (#294)

- `tidypredict_fit()` now sends a missing predictor down the left branch for `ranger::ranger()` models, matching `predict()`. `ranger` compares as `value > splitval`, which a missing value fails, so it takes the same branch as a value at or below the split point. (#294)

- `tidypredict_fit()` now routes missing values through surrogate splits for `rpart::rpart()` models, and for `baguette::bagger()` models using the `"CART"` base model, matching `predict()` instead of sending every missing value down the right branch. All three `usesurrogate` modes are followed, including stopping at the node when no surrogate resolves the row and there is no majority to go with. (#294)

- `tidypredict_fit()` now returns `NA` for a row that reaches a split on a predictor it is missing, for `partykit::ctree()`, `partykit::cforest()` and `mboost::blackboost()` models. These backends resolve a missing value by randomly sampling the split probabilities, so `predict()` returns a different answer on each call and there is no value to reproduce. A row whose path never reaches a split on the missing column is unaffected. (#294)

- `tidypredict_fit()` now returns `NA` for a row with a missing predictor for `randomForest::randomForest()` and `aorsf::orsf()` models, rather than a confident value the model itself would never produce. `randomForest::predict()` returns `NA` for any incomplete row and `aorsf` refuses to predict from one at all, so there is no value to match. Rows are kept rather than dropped. (#294, #325)

- `tidypredict_test()` now handles missing predictions instead of erroring with "missing value where TRUE/FALSE needed". A row where both the model and tidypredict return `NA` counts as a match, and a row where only one of them does is reported as a mismatch, so the function can be used to check how a model behaves on missing data. (#309)

- `tidypredict_test()` now errors when given data with no rows, rather than reporting that all results are within the difference threshold. (#309)

- `tidypredict_fit()` now returns correct predictions for `C50::C5.0()` models whose predictor values fall on a split cut point. C5.0 compares cut points as 32-bit floats, so values between a cut and its float image were sent down the wrong branch. (#287)

- `tidypredict_fit()` now returns correct predictions for `catboost` models whose predictor values fall on a split border. catboost compares borders as 32-bit floats, so a value a fraction above a border was sent down the wrong branch. (#298)

- `tidypredict_save()` and `tidypredict_load()` write a parsed model to a YAML file and read it back. Use them instead of `yaml::write_yaml()`, which stores only 7 significant digits by default and so rounds split thresholds enough to send rows down a different branch when the model is re-loaded. (#307)

- `tidypredict_fit()` now picks the right factor predictor when three or more variable names are nested prefixes of one another, such as `x`, `xy` and `xyz`. The longest match was selected by indexing with `rank()`, which silently chose the wrong variable and produced wrong predictions for `lm()`, `glm()`, `quantreg::rq()`, `nnet::multinom()`, `nnet::nnet()` and `earth::earth()`. (#290)

- Added support for `baguette::bagger()` bagged tree ensembles fit with the `"CART"` or `"C5.0"` base model, including `bag_tree()` parsnip models fitted with the `"rpart"` or `"C5.0"` engine. Regression predictions average the individual trees, and classification predictions return the class with the largest average class probability. (#232)

- `tidypredict_fit()` now supports `C50::C5.0()` models that split a discrete predictor into one branch per level. (#232)

- `tidypredict_fit()` now uses a strict inequality (`<`) for the continuous splits of `rpart::rpart()` models, matching how `rpart` assigns values that are exactly equal to a cut point. (#232)

- `tidypredict_fit()` now returns correct predictions for `randomForest::randomForest()` models that have been saved and reloaded with `parse_model()` and `as_parsed_model()`. Every split variable after the first leaf in a tree was named incorrectly, so the reloaded model split on the wrong columns. (#232)

- `tidypredict_fit()` now returns correct predictions for `Cubist::cubist()` models whose predictor values fall exactly on a split threshold. Cubist compares split thresholds as 32-bit floats, so a `disp` of 95.1 was sent down the wrong branch when the comparison was made in R's doubles. (#232)

- `tidypredict_test()` now supports `C50::C5.0()` models, including boosted and rule-based ones. (#232)

- `tidypredict_interval()` now honours its `interval` argument. It was hardcoded to 0.95, so `tidypredict_interval()`, `tidypredict_to_column(add_interval = TRUE)`, and `tidypredict_sql_interval()` all returned a 95% interval regardless of what was asked for. (#232)

- `tidypredict_interval()` now reports an unsupported model class with a message naming the class, rather than R's default "no applicable method" error. (#232)

- `tidypredict_to_column()` now explains that a model returning more than one formula is unsupported, instead of incorrectly claiming that tree based models are unsupported. (#232)

- `tidypredict_fit()` now reports an unsupported model class with a message naming the class, rather than R's default "no applicable method" error. (#232)

- `tidypredict_fit()` now keeps small probabilities for models with a logit link, such as `glm()` with `family = binomial` and `LiblineaR::LiblineaR()`. The inverse link was written in a form that rounded to exactly 0 once the linear predictor fell below about -37. (#232)

- Added support for `dbarts::bart()` Bayesian additive regression trees, including `bart()` parsnip models fitted with the `"dbarts"` engine. The model has to be fit with `keeptrees = TRUE`, and only continuous outcomes are supported since binary outcomes are fit with a probit link. (#232)

- Added support for `klaR::NaiveBayes()` naive Bayes models with Gaussian densities (`usekernel = FALSE`), including `naive_Bayes()` parsnip models fitted with the `"klaR"` engine. `tidypredict_fit()` returns a named list of class-probability expressions (softmax of the summed log densities), and `tidypredict_test()` is not supported for these multiclass models. (#232)

- Added support for `naivebayes::naive_bayes()` naive Bayes models fit without kernel density estimates (`usekernel = FALSE`), including Gaussian, categorical, Bernoulli, and Poisson conditional distributions, and `naive_Bayes()` parsnip models fitted with the `"naivebayes"` engine. `tidypredict_fit()` returns a named list of class-probability expressions, and `tidypredict_test()` is not supported for these multiclass models. (#232)

- Added support for `parsnip::nullmodel()` models, including `null_model()` parsnip models fitted with the `"parsnip"` engine. Regression models return the outcome mean as a single expression, and classification models return a named list of constant class-probability expressions, for which `tidypredict_test()` is not supported. (#232)

- Added support for the partial least squares models in `mixOmics` (`pls()`, `spls()`, `plsda()`, and `splsda()`), including `pls()` parsnip models fitted with the `"mixOmics"` engine, for regression and classification. Single-outcome regression models return one expression, multivariate outcomes return a named list of expressions, and the discriminant variants return a named list of class-probability expressions (softmax), for which `tidypredict_test()` is not supported. (#232)

- Added support for multinomial `glmnet::glmnet()` models (`family = "multinomial"`), including `multinom_reg()` parsnip models fitted with the `"glmnet"` engine. `tidypredict_fit()` returns a named list of class-probability expressions (softmax), and `tidypredict_test()` is not supported for these multiclass models. (#198)

- Added support for `nnet::multinom()` multinomial log-linear models, including `multinom_reg()` parsnip models fitted with the `"nnet"` engine. `tidypredict_fit()` returns a named list of class-probability expressions (softmax), and `tidypredict_test()` is not supported for these multiclass models. (#232)

- Added support for `nnet::nnet()` single hidden layer neural networks, including `mlp()` parsnip models fitted with the `"nnet"` engine, for regression and classification. Regression models return a single expression, and classification models return a named list of class-probability expressions, for which `tidypredict_test()` is not supported. (#232)

- Added support for `sda::sda()` shrinkage discriminant analysis models, including `discrim_linear()` parsnip models fitted with the `"sda"` engine. `tidypredict_fit()` returns a named list of class-probability expressions (softmax), and `tidypredict_test()` is not supported for these multiclass models. (#232)

- Added support for the regularized linear discriminant analysis models in `sparsediscrim` (`lda_diag()`, `lda_shrink_mean()`, `lda_shrink_cov()`, and `lda_emp_bayes_eigen()`), including `discrim_linear()` parsnip models fitted with the `"sparsediscrim"` engine. `tidypredict_fit()` returns a named list of class-probability expressions (softmax of the per-class discriminant scores), and `tidypredict_test()` is not supported for these multiclass models. (#232)

- Added support for `mda::fda()` flexible discriminant analysis models fit with a linear regression method (`mda::polyreg()` with `degree = 1` or `mda::gen.ridge()`), including `discrim_linear()` parsnip models fitted with the `"mda"` engine. `tidypredict_fit()` returns a named list of class-probability expressions (softmax of the per-class discriminant scores), and `tidypredict_test()` is not supported for these multiclass models. (#232)

- Added support for `MASS::lda()` linear discriminant analysis models, including `discrim_linear()` parsnip models fitted with the `"MASS"` engine. `tidypredict_fit()` returns a named list of class-probability expressions (softmax of the per-class discriminant scores), and `tidypredict_test()` is not supported for these multiclass models. (#232)

- Added support for `MASS::qda()` quadratic discriminant analysis models, including `discrim_quad()` parsnip models fitted with the `"MASS"` engine. `tidypredict_fit()` returns a named list of class-probability expressions (softmax of the per-class quadratic discriminant scores), and `tidypredict_test()` is not supported for these multiclass models. (#232)

- Added support for `kernlab::ksvm()` linear support vector machine models (`vanilladot` kernel), including `svm_linear()` parsnip models fitted with the `"kernlab"` engine, for regression and binary classification. Non-linear kernels and multiclass classification are not supported, and classification requires a probability model (`prob.model = TRUE`). (#232)

- Added support for H2O gradient boosting models (`H2ORegressionModel`, `H2OBinomialModel`, and `H2OMultinomialModel`), including `boost_tree()` parsnip models fitted with the `"h2o_gbm"` engine, for regression and classification. Only GBM models are supported (not H2O's XGBoost), predictions require a running H2O cluster, and gaussian, bernoulli, and multinomial distributions are supported. (#232)

- Added support for H2O RuleFit models (`h2o::h2o.rulefit()`), including `rule_fit()` parsnip models fitted with the `"h2o"` engine, for regression and binary classification. Predictions require a running H2O cluster, and multiclass models are not supported because `h2o.rule_importance()` does not expose the per-class coefficients. (#232)

- Added support for `mboost::blackboost()` gradient boosting regression models, including `boost_tree()` parsnip models fitted with the `"mboost"` engine. Only the `Gaussian()` family is supported. (#232)

- Added support for `aorsf::orsf()` oblique random forest regression models, including `rand_forest()` parsnip models fitted with the `"aorsf"` engine. Only numeric predictors are supported and classification is not supported. (#232)

- Added support for `C50::C5.0()` classification tree models, including `decision_tree()` and `boost_tree()` parsnip models fitted with the `"C5.0"` engine. Boosted models (`trials > 1`) combine trials by confidence-weighted voting. Fuzzy thresholds (`fuzzyThreshold = TRUE`) and cost matrices (`costs`) are not supported. (#232)

- Added support for rule-based `C50::C5.0()` classification models (`rules = TRUE`), including `C5_rules()` parsnip models fitted with the `"C5.0"` engine. Boosted rule-based models (`trials > 1`) are not supported. (#232)

- Added support for `partykit::cforest()` random forest regression models, including `rand_forest()` parsnip models fitted with the `"partykit"` engine. Classification is not supported. (#232)

- Added support for `LiblineaR::LiblineaR()` binary logistic regression models (`type` 0, 6, 7), including `logistic_reg()` parsnip models fitted with the `"LiblineaR"` engine. Also added support for linear support vector machine models, including `svm_linear()` parsnip models fitted with the `"LiblineaR"` engine, for regression (`type` 11, 12, 13) and binary classification (`type` 1-5). Classification returns the SVM decision value rather than a probability. (#232)

- Added support for `xrf::xrf()` rule-based models (RuleFit), including `rule_fit()` parsnip models fitted with the `"xrf"` engine, for regression (`family = "gaussian"`) and binary classification (`family = "binomial"`). Multinomial models are not supported. (#232)

- Added support for `decision_tree()` parsnip models fitted with the `"rpart"` engine. (#232)

- Added support for `linear_reg()` parsnip models fitted with the `"glm"` engine. (#232)

- Added support for `quantreg::rq()` quantile regression models, including `linear_reg()` parsnip models fitted with the `"quantreg"` engine. Models fitted with multiple quantiles return one fit expression per quantile, named by the quantile level. (#232)

- `tidypredict_fit()` now returns correct predictions for xgboost models whose feature values fall exactly on a split threshold. xgboost compares split thresholds as 32-bit floats, so a value such as a `wt` of 3.19 was sent down the wrong branch when the comparison was made in R's doubles. (#232)

- `tidypredict_fit()` now returns correct predictions for xgboost models that have been saved and reloaded with `parse_model()` and `as_parsed_model()`. Previously every tree collapsed to a single leaf value. (#232)

- `tidypredict_test()` now flags rows where the fitted value is above the model's own prediction for xgboost models. Previously only differences in one direction were reported, so real disagreements could go unnoticed. (#232)

- `tidypredict_test()` now reports the maximum fit, lower, and upper differences under the correct labels, and reports absolute rather than signed differences. Previously the fit and upper values were swapped, and the fit value was omitted entirely when `include_intervals = FALSE`. (#232)

- `tidypredict_test()` now reports a failure message for multiclass CatBoost models when results exceed the threshold. Previously it always claimed that all results were within the threshold, even when `alert` was `TRUE`. (#232)

- `tidypredict_test()` now compares ranger models against `predict()`. Previously the comparison silently measured tidypredict's predictions against themselves and so always reported a difference of zero. (#232)

- `tidypredict_test()` now reports an absolute maximum difference for glmnet models, which could previously be negative. (#232)

- `tidypredict_test()` now names the model's own predictions `fit` in `raw_results` for XGBoost, LightGBM, CatBoost and h2o models, matching every other model type. The column was previously called `base`. (#232)

- `tidypredict_test()` now reports `fit_diff` as a signed difference for LightGBM, CatBoost and h2o models, so the direction of the error is visible. The threshold is applied to its absolute value, as before. (#232)

- `tidypredict_test()` results for classification models are now reported consistently: `fit_diff` is a 0/1 indicator, the threshold is reported as 0 since labels are compared exactly, and the message counts records that do not match rather than quoting a maximum difference. (#232)

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

- Fixed bug where Cubst models incorrectly combined rules and committees. (#134)

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

- Change to with with version 5.1.2 and above of the `earth` package. As a result, `tidypredict` will only parse objects created by this and later versions of `earth`. 

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
