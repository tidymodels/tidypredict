## ----setup, include=FALSE------------------------------------------------
library(dplyr)
library(tidypredict)
library(randomForest)
set.seed(100)

## ------------------------------------------------------------------------
library(randomForest)
model <- randomForest(Species ~ .,data = iris ,ntree = 100, proximity = TRUE)

## ------------------------------------------------------------------------
library(tidypredict)

tidypredict_sql(model, dbplyr::simulate_mssql())

## ------------------------------------------------------------------------
iris %>%
  tidypredict_to_column(model) %>%
  head(10)

## ------------------------------------------------------------------------
getTree(model, labelVar = TRUE) %>%
  head()

## ------------------------------------------------------------------------
parse_model(model)

## ------------------------------------------------------------------------
tidypredict_fit(model)

## ------------------------------------------------------------------------
test <- tidypredict_test(model, iris, threshold = 5)

test

test$raw_results %>%
  filter(predict != tidypredict)

