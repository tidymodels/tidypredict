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
  predict_to_column(model)

## ------------------------------------------------------------------------
predict(model, head(df,10))

## ------------------------------------------------------------------------
df %>%
  head(10) %>%
  predict_to_column(model, add_interval = TRUE) %>%
  select(fit, lower, upper)

## ------------------------------------------------------------------------
predict(model, head(df,10), interval = "prediction")

## ------------------------------------------------------------------------
predict_fit(model)

## ------------------------------------------------------------------------
df %>%
  head(10) %>%
  mutate(fit = !! predict_fit(model))

## ------------------------------------------------------------------------
df %>%
  head(10) %>%
  mutate(fit = !! predict_fit(model),
         lwr = fit + !! predict_interval(model),
         upr = fit - !! predict_interval(model)) %>%
  select(fit, lwr, upr)

## ------------------------------------------------------------------------
predict_interval(model, interval = 0.99)

## ------------------------------------------------------------------------
parsemodel(model)

## ------------------------------------------------------------------------
write.csv(parsemodel(model), "model.csv")


reloaded_model <- read.csv("model.csv")

df %>%
  head(10) %>%
  predict_to_column(reloaded_model, add_interval = TRUE, vars = c("ft", "up", "lw")) %>%
  select(ft, lw, up)

