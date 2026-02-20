#' Converts an R model object into a parsed model
#'
#' Parses a fitted R model's structure and extracts the components needed to
#' create a dplyr formula for prediction. The parsed model can be serialized
#' (e.g., saved to YAML) and later used to generate predictions without the
#' original model object.
#'
#' @param model An R model object.
#'
#' @returns A parsed model object with class `parsed_model` and a model-specific
#'   subclass (e.g., `pm_xgb`, `pm_tree`, `pm_regression`). The object contains:
#'
#'   - `$general`: List with model metadata including `model` (model type),
#'     `type` (used for S3 dispatch), `version` (parsed model format version),
#'     and model-specific parameters.
#'   - Model-specific fields containing coefficients, tree structures, etc.
#'
#' @section Parsed model versions:
#'
#' The `$general$version` field indicates the parsed model format:
#'
#' - **Version 1**: Original format. Linear models store coefficients in a
#'   data frame. Tree models use flat `case_when()` expressions where all leaf
#'   conditions are at the same level.
#'
#' - **Version 2**: Improved coefficient storage for linear models (lm, earth).
#'   Tree models still use flat `case_when()`.
#'
#' - **Version 3**: Current format. Tree models (rpart, ranger, randomForest,
#'   xgboost, lightgbm, catboost, partykit, cubist) use nested `case_when()`
#'   expressions that mirror the tree structure. This produces more efficient
#'   SQL and R code because conditions are evaluated hierarchically rather than
#'   checking all leaf paths.
#'
#' When loading a parsed model saved with an older version, tidypredict
#' automatically uses the appropriate formula builder for backwards
#' compatibility.
#'
#' @section Model types:
#'
#' Each parsed model has a type that determines the S3 class used for dispatch:
#'
#' - `pm_regression`: Linear models (lm, glm, earth, glmnet)
#' - `pm_tree`: Single trees and forests (rpart, partykit, ranger, randomForest,
#'   cubist)
#' - `pm_xgb`: XGBoost gradient boosting models
#' - `pm_lgb`: LightGBM gradient boosting models
#' - `pm_catboost`: CatBoost gradient boosting models
#'
#' @examples
#' library(dplyr)
#' df <- mutate(mtcars, cyl = paste0("cyl", cyl))
#' model <- lm(mpg ~ wt + cyl * disp, offset = am, data = df)
#' parse_model(model)
#' @export
parse_model <- function(model) {
  UseMethod("parse_model")
}
