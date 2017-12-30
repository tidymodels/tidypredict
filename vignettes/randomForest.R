## ----setup, include=FALSE------------------------------------------------
library(dplyr)
library(tidypredict)
library(randomForest)

## ------------------------------------------------------------------------
library(tidypredict)
library(randomForest)

set.seed(100)

model <- randomForest(Species ~ .,data = iris ,ntree = 100, proximity = TRUE)

model

## ------------------------------------------------------------------------
iris %>%
  predict_to_column(model) %>%
  head(10)

## ------------------------------------------------------------------------
getTree(model, labelVar = TRUE) %>% 
  as.tibble() 

## ------------------------------------------------------------------------
parsemodel(model)

## ------------------------------------------------------------------------
predict_fit(model)

## ------------------------------------------------------------------------
fit_to_sql(model, dbplyr::simulate_dbi())

## ------------------------------------------------------------------------
test <- test_predictions(model, iris, threshold = 5)

test

test$raw_results %>%
  filter(predict != tidypredict)

