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

## ------------------------------------------------------------------------
df %>%
  head(10) %>%
  tidypredict_to_column(model, add_interval = TRUE) %>%
  select(fit, lower, upper)

## ------------------------------------------------------------------------
predict(model, head(df,10), interval = "prediction")

## ------------------------------------------------------------------------
tidypredict_fit(model)

## ------------------------------------------------------------------------
df %>%
  head(10) %>%
  mutate(fit = !! tidypredict_fit(model))

## ------------------------------------------------------------------------
df %>%
  head(10) %>%
  mutate(fit = !! tidypredict_fit(model),
         lwr = fit + !! tidypredict_interval(model),
         upr = fit - !! tidypredict_interval(model)) %>%
  select(fit, lwr, upr)

## ------------------------------------------------------------------------
tidypredict_interval(model, interval = 0.99)

## ------------------------------------------------------------------------
parse_model(model)

## ------------------------------------------------------------------------
write.csv(parse_model(model), "model.csv")


reloaded_model <- read.csv("model.csv")

df %>%
  head(10) %>%
  tidypredict_to_column(reloaded_model, add_interval = TRUE, vars = c("ft", "up", "lw")) %>%
  select(ft, lw, up)

## ------------------------------------------------------------------------
model <- lm(mpg ~ wt + am + cyl, data = df)

tidypredict_test(model, include_intervals = TRUE)

## ------------------------------------------------------------------------
tidypredict_test(model, include_intervals = TRUE, threshold = 0.0000000000000001)

## ------------------------------------------------------------------------
results <- tidypredict_test(model, include_intervals = TRUE, threshold = 0.0000000000000001)

results

## ------------------------------------------------------------------------
head(results$raw_results)

## ------------------------------------------------------------------------
results$alert

