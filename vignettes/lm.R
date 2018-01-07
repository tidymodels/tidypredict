## ----setup, include=FALSE------------------------------------------------
library(dplyr)
library(tidypredict)

## ------------------------------------------------------------------------
df <- mtcars %>%
  mutate(char_cyl = paste0("cyl", cyl)) %>%
  select(mpg, wt, char_cyl, am) 

model <- lm(mpg ~ wt + char_cyl, offset = am, data = df)

## ------------------------------------------------------------------------
library(tidypredict)

tidypredict_sql(model, dbplyr::simulate_mssql())

## ------------------------------------------------------------------------
df %>%
  tidypredict_to_column(model) %>%
  head(10) 

## ------------------------------------------------------------------------
tidypredict_sql_interval(model, dbplyr::simulate_mssql())

## ------------------------------------------------------------------------
df %>%
  tidypredict_to_column(model, add_interval = TRUE) %>%
  head(10)

## ------------------------------------------------------------------------
parse_model(model)

## ------------------------------------------------------------------------
tidypredict_fit(model)

## ------------------------------------------------------------------------
tidypredict_interval(model)

## ------------------------------------------------------------------------
tidypredict_test(model)

## ------------------------------------------------------------------------
tidypredict_test(model, include_intervals = TRUE)

