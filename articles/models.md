# Supported models

This is the list of models tidypredict can parse: 43 fitted model
classes from 30 modeling packages.

tidypredict dispatches on the class of the fitted model, so a model is
supported if the “Fit with” column covers how it was fitted. Models
fitted through [parsnip](https://parsnip.tidymodels.org/) are unwrapped
and handed to the same code, so the “parsnip” column is not a separate
list of capabilities: it names the spec and engine that produce each
fitted class. A blank there means tidypredict has no test for that
route, not that it is known to fail.

## Regression

| Model | Fit with | parsnip | Details |
|----|----|----|----|
| Linear regression | [`stats::lm()`](https://rdrr.io/r/stats/lm.html) | `linear_reg(engine = "lm")` | [article](https://tidypredict.tidymodels.org/articles/lm.md) |
| Generalized linear regression | [`stats::glm()`](https://rdrr.io/r/stats/glm.html) | `linear_reg(engine = "glm")`, `logistic_reg(engine = "glm")` | [article](https://tidypredict.tidymodels.org/articles/glm.md) |
| Regularized regression | [`glmnet::glmnet()`](https://glmnet.stanford.edu/reference/glmnet.html) | [`linear_reg()`](https://parsnip.tidymodels.org/reference/linear_reg.html), [`logistic_reg()`](https://parsnip.tidymodels.org/reference/logistic_reg.html), [`multinom_reg()`](https://parsnip.tidymodels.org/reference/multinom_reg.html) with `engine = "glmnet"` | [article](https://tidypredict.tidymodels.org/articles/glmnet.md) |
| Regularized linear models | [`LiblineaR::LiblineaR()`](https://rdrr.io/pkg/LiblineaR/man/LiblineaR.html) | `logistic_reg(engine = "LiblineaR")`, `svm_linear(engine = "LiblineaR")` |  |
| Quantile regression | [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html), `quantreg::rqs()` | `linear_reg(engine = "quantreg")` |  |
| Multinomial regression | [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html) | `multinom_reg(engine = "nnet")` | [article](https://tidypredict.tidymodels.org/articles/multinom.md) |
| Support vector machine | [`kernlab::ksvm()`](https://rdrr.io/pkg/kernlab/man/ksvm.html) | `svm_linear(engine = "kernlab")` |  |
| Neural network | [`nnet::nnet()`](https://rdrr.io/pkg/nnet/man/nnet.html) | `mlp(engine = "nnet")` | [article](https://tidypredict.tidymodels.org/articles/nnet.md) |
| MARS | [`earth::earth()`](https://rdrr.io/pkg/earth/man/earth.html) | `mars(engine = "earth")` | [article](https://tidypredict.tidymodels.org/articles/mars.md) |
| Partial least squares | [`mixOmics::pls()`](https://rdrr.io/pkg/mixOmics/man/pls.html), `spls()`, `plsda()`, `splsda()` | `pls(engine = "mixOmics")` | [article](https://tidypredict.tidymodels.org/articles/mixOmics.md) |
| Null model | [`parsnip::nullmodel()`](https://parsnip.tidymodels.org/reference/nullmodel.html) | [`null_model()`](https://parsnip.tidymodels.org/reference/null_model.html) | [article](https://tidypredict.tidymodels.org/articles/nullmodel.md) |

## Classification and discriminant analysis

| Model | Fit with | parsnip | Details |
|----|----|----|----|
| Naive Bayes | [`naivebayes::naive_bayes()`](https://majkamichal.github.io/naivebayes/reference/naive_bayes.html), [`klaR::NaiveBayes()`](https://rdrr.io/pkg/klaR/man/NaiveBayes.html) | [`naive_Bayes()`](https://parsnip.tidymodels.org/reference/naive_Bayes.html) with `engine = "naivebayes"` or `"klaR"` | [article](https://tidypredict.tidymodels.org/articles/naivebayes.md) |
| Linear discriminant analysis | [`MASS::lda()`](https://rdrr.io/pkg/MASS/man/lda.html) | `discrim_linear(engine = "MASS")` | [article](https://tidypredict.tidymodels.org/articles/lda.md) |
| Quadratic discriminant analysis | [`MASS::qda()`](https://rdrr.io/pkg/MASS/man/qda.html) | `discrim_quad(engine = "MASS")` | [article](https://tidypredict.tidymodels.org/articles/qda.md) |
| Flexible discriminant analysis | [`mda::fda()`](https://rdrr.io/pkg/mda/man/fda.html) | `discrim_linear(engine = "mda")` | [article](https://tidypredict.tidymodels.org/articles/fda.md) |
| Shrinkage discriminant analysis | [`sda::sda()`](https://rdrr.io/pkg/sda/man/sda.html) | `discrim_linear(engine = "sda")` | [article](https://tidypredict.tidymodels.org/articles/sda.md) |
| Regularized discriminant analysis | [`sparsediscrim::lda_diag()`](https://topepo.github.io/sparsediscrim/reference/lda_diag.html), `lda_shrink_mean()`, `lda_shrink_cov()`, `lda_emp_bayes_eigen()` | `discrim_linear(engine = "sparsediscrim")` | [article](https://tidypredict.tidymodels.org/articles/sparsediscrim.md) |

## Trees and forests

| Model | Fit with | parsnip | Details |
|----|----|----|----|
| Decision tree | [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html) | `decision_tree(engine = "rpart")` | [article](https://tidypredict.tidymodels.org/articles/rpart.md) |
| Decision tree | [`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html) | `decision_tree(engine = "C5.0")`, `C5_rules(engine = "C5.0")` | [article](https://tidypredict.tidymodels.org/articles/C5.0.md) |
| Conditional inference tree | [`partykit::ctree()`](https://rdrr.io/pkg/partykit/man/ctree.html) |  | [article](https://tidypredict.tidymodels.org/articles/partykit.md) |
| Random forest | [`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html) | `rand_forest(engine = "randomForest")` | [article](https://tidypredict.tidymodels.org/articles/rf.md) |
| Random forest | [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md) | `rand_forest(engine = "ranger")` | [article](https://tidypredict.tidymodels.org/articles/ranger.md) |
| Conditional inference forest | [`partykit::cforest()`](https://rdrr.io/pkg/partykit/man/cforest.html) | `rand_forest(engine = "partykit")` | [article](https://tidypredict.tidymodels.org/articles/partykit.md) |
| Oblique random forest | [`aorsf::orsf()`](https://docs.ropensci.org/aorsf/reference/orsf.html) | `rand_forest(engine = "aorsf")` | [article](https://tidypredict.tidymodels.org/articles/aorsf.md) |
| Bagged trees | [`baguette::bagger()`](https://baguette.tidymodels.org/reference/bagger.html) | [`bag_tree()`](https://parsnip.tidymodels.org/reference/bag_tree.html) with `engine = "rpart"` or `"C5.0"` | [article](https://tidypredict.tidymodels.org/articles/bagging.md) |
| BART | [`dbarts::bart()`](https://rdrr.io/pkg/dbarts/man/bart.html) | `bart(engine = "dbarts")` | [article](https://tidypredict.tidymodels.org/articles/bart.md) |

## Boosting and rules

| Model | Fit with | parsnip | Details |
|----|----|----|----|
| XGBoost | [`xgboost::xgb.train()`](https://rdrr.io/pkg/xgboost/man/xgb.train.html) | `boost_tree(engine = "xgboost")` | [article](https://tidypredict.tidymodels.org/articles/xgboost.md) |
| LightGBM | [`lightgbm::lgb.train()`](https://rdrr.io/pkg/lightgbm/man/lgb.train.html) | `boost_tree(engine = "lightgbm")`, via bonsai | [article](https://tidypredict.tidymodels.org/articles/lightgbm.md) |
| CatBoost | [`catboost::catboost.train()`](https://rdrr.io/pkg/catboost/man/catboost.train.html) | `boost_tree(engine = "catboost")`, via bonsai | [article](https://tidypredict.tidymodels.org/articles/catboost.md) |
| Boosted C5.0 trees | [`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html) with `trials` | `boost_tree(engine = "C5.0")` | [article](https://tidypredict.tidymodels.org/articles/C5.0.md) |
| Model-based boosting | [`mboost::blackboost()`](https://rdrr.io/pkg/mboost/man/blackboost.html) |  |  |
| Cubist | [`Cubist::cubist()`](http://topepo.github.io/Cubist/reference/cubist.default.md) | `cubist_rules(engine = "Cubist")` | [article](https://tidypredict.tidymodels.org/articles/cubist.md) |
| RuleFit | [`xrf::xrf()`](https://rdrr.io/pkg/xrf/man/xrf.html) | `rule_fit(engine = "xrf")` |  |
| H2O gradient boosting | [`h2o::h2o.gbm()`](https://rdrr.io/pkg/h2o/man/h2o.gbm.html) | `boost_tree(engine = "h2o_gbm")`, via agua | [article](https://tidypredict.tidymodels.org/articles/h2o.md) |
| H2O RuleFit | [`h2o::h2o.rulefit()`](https://rdrr.io/pkg/h2o/man/h2o.rulefit.html) | `rule_fit(engine = "h2o")`, via agua | [article](https://tidypredict.tidymodels.org/articles/h2o.md) |

## Intervals

[`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md)
and
[`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md)
are narrower than
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md):
they only support [`lm()`](https://rdrr.io/r/stats/lm.html) and
[`glm()`](https://rdrr.io/r/stats/glm.html) models.

## Adding a model

If a model you need is missing, [open an
issue](https://github.com/tidymodels/tidypredict/issues).
`CONTRIBUTING.md` describes what a new model needs, and the [non-R
models](https://tidypredict.tidymodels.org/articles/non-r.md) article
covers the other direction: writing a parsed model spec by hand so a
model fitted outside R can be used here.
