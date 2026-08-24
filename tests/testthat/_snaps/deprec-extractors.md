# .extract_xgb_trees() is deprecated

    Code
      x <- .extract_xgb_trees(model)
    Condition
      Warning:
      `.extract_xgb_trees()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_trees()` instead.

# .extract_xgb_trees() errors on the wrong class

    Code
      .extract_xgb_trees(lm(mpg ~ wt, mtcars))
    Condition
      Warning:
      `.extract_xgb_trees()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_trees()` instead.
      Error in `.extract_xgb_trees()`:
      ! `model` must be <xgb.Booster>, not a <lm> object.

# .extract_lgb_trees() is deprecated

    Code
      x <- .extract_lgb_trees(model)
    Condition
      Warning:
      `.extract_lgb_trees()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_trees()` instead.

# .extract_lgb_trees() errors on the wrong class

    Code
      .extract_lgb_trees(lm(mpg ~ wt, mtcars))
    Condition
      Warning:
      `.extract_lgb_trees()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_trees()` instead.
      Error in `.extract_lgb_trees()`:
      ! `model` must be <lgb.Booster>, not a <lm> object.

# .extract_catboost_trees() is deprecated

    Code
      x <- .extract_catboost_trees(model)
    Condition
      Warning:
      `.extract_catboost_trees()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_trees()` instead.

# .extract_catboost_trees() errors on the wrong class

    Code
      .extract_catboost_trees(lm(mpg ~ wt, mtcars))
    Condition
      Warning:
      `.extract_catboost_trees()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_trees()` instead.
      Error in `.extract_catboost_trees()`:
      ! `model` must be <catboost.Model>, not a <lm> object.

# .extract_rf_trees() is deprecated

    Code
      x <- .extract_rf_trees(model)
    Condition
      Warning:
      `.extract_rf_trees()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_trees()` instead.

# .extract_rf_trees() errors on the wrong class

    Code
      .extract_rf_trees(lm(mpg ~ wt, mtcars))
    Condition
      Warning:
      `.extract_rf_trees()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_trees()` instead.
      Error in `.extract_rf_trees()`:
      ! `model` must be <randomForest>, not a <lm> object.

# .extract_ranger_trees() is deprecated

    Code
      x <- .extract_ranger_trees(model)
    Condition
      Warning:
      `.extract_ranger_trees()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_trees()` instead.

# .extract_ranger_trees() errors on the wrong class

    Code
      .extract_ranger_trees(lm(mpg ~ wt, mtcars))
    Condition
      Warning:
      `.extract_ranger_trees()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_trees()` instead.
      Error in `.extract_ranger_trees()`:
      ! `model` must be <ranger>, not a <lm> object.

# .extract_rf_classprob() is deprecated

    Code
      x <- .extract_rf_classprob(model)
    Condition
      Warning:
      `.extract_rf_classprob()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_class_trees()` instead.

# .extract_rf_classprob() errors on the wrong class

    Code
      .extract_rf_classprob(lm(mpg ~ wt, mtcars))
    Condition
      Warning:
      `.extract_rf_classprob()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_class_trees()` instead.
      Error in `.extract_rf_classprob()`:
      ! `model` must be <randomForest>, not a <lm> object.

# .extract_ranger_classprob() is deprecated

    Code
      x <- .extract_ranger_classprob(model)
    Condition
      Warning:
      `.extract_ranger_classprob()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_class_trees()` instead.

# .extract_ranger_classprob() errors on the wrong class

    Code
      .extract_ranger_classprob(lm(mpg ~ wt, mtcars))
    Condition
      Warning:
      `.extract_ranger_classprob()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_class_trees()` instead.
      Error in `.extract_ranger_classprob()`:
      ! `model` must be <ranger>, not a <lm> object.

# .extract_rpart_classprob() is deprecated

    Code
      x <- .extract_rpart_classprob(model)
    Condition
      Warning:
      `.extract_rpart_classprob()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_class_exprs()` instead.

# .extract_rpart_classprob() errors on the wrong class

    Code
      .extract_rpart_classprob(lm(mpg ~ wt, mtcars))
    Condition
      Warning:
      `.extract_rpart_classprob()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_class_exprs()` instead.
      Error in `.extract_rpart_classprob()`:
      ! `model` must be <rpart>, not a <lm> object.

# .extract_partykit_classprob() is deprecated

    Code
      x <- .extract_partykit_classprob(model)
    Condition
      Warning:
      `.extract_partykit_classprob()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_class_exprs()` instead.

# .extract_partykit_classprob() errors on the wrong class

    Code
      .extract_partykit_classprob(lm(mpg ~ wt, mtcars))
    Condition
      Warning:
      `.extract_partykit_classprob()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_class_exprs()` instead.
      Error in `.extract_partykit_classprob()`:
      ! `model` must be <party>, not a <lm> object.

# .extract_earth_multiclass() is deprecated

    Code
      x <- .extract_earth_multiclass(model)
    Condition
      Warning:
      `.extract_earth_multiclass()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_class_exprs()` instead.

# .extract_earth_multiclass() errors on the wrong class

    Code
      .extract_earth_multiclass(lm(mpg ~ wt, mtcars))
    Condition
      Warning:
      `.extract_earth_multiclass()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_class_exprs()` instead.
      Error in `.extract_earth_multiclass()`:
      ! `model` must be <earth>, not a <lm> object.

# .extract_glmnet_multiclass() is deprecated

    Code
      x <- .extract_glmnet_multiclass(model)
    Condition
      Warning:
      `.extract_glmnet_multiclass()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_class_exprs()` instead.

# .extract_glmnet_multiclass() errors on the wrong class

    Code
      .extract_glmnet_multiclass(lm(mpg ~ wt, mtcars))
    Condition
      Warning:
      `.extract_glmnet_multiclass()` was deprecated in tidypredict 1.1.1.
      i Please use `tidypredict_class_exprs()` instead.
      Error in `.extract_glmnet_multiclass()`:
      ! `model` must be <multnet>, not a <lm> object.

