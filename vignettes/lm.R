## ----setup, include=FALSE------------------------------------------------
library(dplyr)
library(tidypredict)

## ------------------------------------------------------------------------
library(dplyr)
library(tidypredict)

df <- mtcars %>%
  mutate(cyl = paste0("cyl", cyl))

model <- lm(mpg ~ wt + am + cyl, data = df)

model

## ------------------------------------------------------------------------
df %>%
  head(10) %>%
  tidypredict_to_column(model)

## ------------------------------------------------------------------------
predict(model, head(df,10))

