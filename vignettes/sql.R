## ----setup, include=FALSE------------------------------------------------
library(dplyr)
library(tidypredict)
library(randomForest)
library(dbplyr)
library(tibble)

set.seed(100)

## ------------------------------------------------------------------------
library(dplyr)
library(tibble)

flights_table <- nycflights13::flights %>%
  mutate(current_score = 0) %>%
  rowid_to_column("flight_id")

## ------------------------------------------------------------------------
library(DBI)

con <- dbConnect(RSQLite::SQLite(), path = ":memory:")
db_fligths <- copy_to(con,flights_table )

## ------------------------------------------------------------------------
df <- db_fligths %>%
  select(dep_delay, hour, distance) %>%
  head(1000) %>%
  collect() 

## ------------------------------------------------------------------------
model <- lm(dep_delay ~ ., data = df)

## ------------------------------------------------------------------------
tidypredict_test(model)

## ---- eval = FALSE-------------------------------------------------------
#  if(tidypredict_test(model)$alert) stop("Threshold exceeded!")

## ------------------------------------------------------------------------
library(dbplyr)

update_statement <- build_sql("UPDATE flights_table SET current_score  = ", tidypredict_sql(model, con = con), con = con)

update_statement

## ------------------------------------------------------------------------
dbSendQuery(con, update_statement)

## ------------------------------------------------------------------------
db_fligths %>%
  select(current_score) %>%
  head(10) 
  

## ------------------------------------------------------------------------
dbWriteTable(con, "daily_scores", 
             tibble(
               flight_id = 0,
               score = 0,
               date = ""
             ))

## ------------------------------------------------------------------------
new_predictions <- db_fligths %>%
  filter(month == 12) %>% 
  tidypredict_to_column(model, vars = "score") %>%
  select(
    flight_id,
    score) %>%
  mutate(date = "01/01/2018")

## ------------------------------------------------------------------------
insert_scores <- build_sql("INSERT INTO daily_scores ", sql_render(new_predictions, con = con), con = con)
insert_scores

## ------------------------------------------------------------------------
dbSendQuery(con, insert_scores)

## ------------------------------------------------------------------------
tbl(con, "daily_scores") %>%
  inner_join(tbl(con, "flights_table"), by = "flight_id") %>%
  filter(date == "01/01/2018") %>%
  select(dep_delay, hour, distance, score, date)

## ---- include = FALSE----------------------------------------------------
dbDisconnect(con)

