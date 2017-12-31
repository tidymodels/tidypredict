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
  tidypredict_to_column(model) %>%
  head(10)

## ------------------------------------------------------------------------
getTree(model, labelVar = TRUE) %>% 
  as.tibble() 

## ------------------------------------------------------------------------
parse_model(model)

## ------------------------------------------------------------------------
tidypredict_fit(model)

## ------------------------------------------------------------------------
tidypredict_sql(model, dbplyr::simulate_dbi())

## ------------------------------------------------------------------------
test <- tidypredict_test(model, iris, threshold = 5)

test

test$raw_results %>%
  filter(predict != tidypredict)

