## ----setup, include=FALSE------------------------------------------------
library(dplyr)
library(tidypredict)
library(ranger)
set.seed(100)

## ------------------------------------------------------------------------
library(ranger)
model <- ranger::ranger(Species ~ .,data = iris ,num.trees = 100)

## ------------------------------------------------------------------------
library(tidypredict)

tidypredict_sql(model, dbplyr::simulate_mssql())

## ------------------------------------------------------------------------
iris %>%
  tidypredict_to_column(model) %>%
  head(10)

## ------------------------------------------------------------------------
treeInfo(model)

## ------------------------------------------------------------------------
parse_model(model)

## ------------------------------------------------------------------------
tidypredict_fit(model)

## ------------------------------------------------------------------------
test <- tidypredict_test(model, iris, threshold = 4)

test

test$raw_results %>%
  filter(predict != tidypredict)

