## ----setup, include=FALSE------------------------------------------------
library(dplyr)
library(tidypredict)

## ------------------------------------------------------------------------
df <- mtcars %>%
  mutate(char_cyl = paste0("cyl", cyl)) %>%
  select(wt, char_cyl, am) 

model <- glm(am ~ wt + char_cyl, data = df, family = "binomial")

## ------------------------------------------------------------------------
library(tidypredict)

tidypredict_sql(model, dbplyr::simulate_mssql())

## ------------------------------------------------------------------------
df %>%
  tidypredict_to_column(model) %>%
  head(10) 

## ------------------------------------------------------------------------
parse_model(model)

## ------------------------------------------------------------------------
tidypredict_fit(model)

## ------------------------------------------------------------------------
tidypredict_test(model)

