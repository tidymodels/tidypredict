## ----setup, include=FALSE------------------------------------------------
library(dplyr)
library(tidypredict)

## ------------------------------------------------------------------------
model <- glm(am ~ mpg + wt, data = mtcars, family = "binomial")

## ------------------------------------------------------------------------
library(tidypredict)

mtcars %>%
  head(10) %>%
  tidypredict_to_column(model) %>%
  select(mpg, wt, am, fit)

## ------------------------------------------------------------------------
tidypredict_fit(model)

## ------------------------------------------------------------------------
tidypredict_sql(model, dbplyr::simulate_dbi())

## ------------------------------------------------------------------------
test <- tidypredict_test(model)

test

## ------------------------------------------------------------------------
test$raw_results %>%
  head(10)

