# Database write-back

This article reviews two possible scenarios for “writing back”
predictions to the database, and without importing the data into memory
first. Both scenarios share a common model preparation method.

## Example setup

To keep the example reproducible, a SQLite database will be used to
simulate a larger scale deployment.

First the data is prepared in memory. The article will use the
[`nycflights13::flights`](https://rdrr.io/pkg/nycflights13/man/flights.html)
data, with a couple of modifications

``` r
library(dplyr)
library(tidypredict)
library(randomForest)
library(dbplyr)

flights_table <- nycflights13::flights %>%
  mutate(
    current_score = 0,
    flight_id = row_number()
  )
```

A new database is created using `RSQLite`.

``` r
library(DBI)

con <- dbConnect(RSQLite::SQLite(), path = ":memory:")
db_fligths <- copy_to(con, flights_table)
```

## Model preparation

A sample is downloaded from the database for modeling. This example
already selects the needed variables.

``` r
df <- db_fligths %>%
  select(dep_delay, hour, distance) %>%
  head(1000) %>%
  collect()
```

A linear model is fitted using [`lm()`](https://rdrr.io/r/stats/lm.html)

``` r
model <- lm(dep_delay ~ ., data = df)
```

It is highly recommendable to always run
[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
to make sure that the predictions are in line with what the
[`predict()`](https://rdrr.io/r/stats/predict.html) command returns.

The `lm` and `glm` models contain the data that they were fitted with,
so it’s easy to run a test.
[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
uses the model’s internal data set by default

``` r
tidypredict_test(model)
#> tidypredict test results
#> Difference threshold: 1e-12
#> 
#>  All results are within the difference threshold
```

In cases where the model re-fitting is automated, the
[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
function returns an `alert` in case the threshold is exceeded, which can
be used to fail the automated script.

``` r
if (tidypredict_test(model)$alert) stop("Threshold exceeded!")
```

## Scenario 1 - Update scores

In this scenario, The table that supplies the term values is also the
recipient of the new score. This is done by updating a specific field in
the table. For this example, the field is called `current_score`. The
following SQL UPDATE statement should work in most databases:

``` r
library(dbplyr)

update_statement <- build_sql("UPDATE flights_table SET current_score  = ", tidypredict_sql(model, con = con), con = con)

update_statement
#> <SQL> UPDATE flights_table SET current_score  = (-3.59844229187029 + (`hour` * 1.38710560882252)) + (`distance` * -0.00307606912118568)
```

This statement can be then passed on to the database team, via
documentation or an automated process. In cases where the analyst has
the responsibility to run the new SQL statement, or if R is being used
to automate the scoring, the next line can be used:

``` r
dbSendQuery(con, update_statement)
#> <SQLiteResult>
#>   SQL  UPDATE flights_table SET current_score  = (-3.59844229187029 + (`hour` * 1.38710560882252)) + (`distance` * -0.00307606912118568)
#>   ROWS Fetched: 0 [complete]
#>        Changed: 336776
```

Here is a sample of the newly populated field:

``` r
db_fligths %>%
  select(current_score) %>%
  head(10)
#> Warning: Closing open result set, pending rows
#> # Source:   SQL [?? x 1]
#> # Database: sqlite 3.50.4 []
#>    current_score
#>            <dbl>
#>  1       -0.969 
#>  2       -1.02  
#>  3       -0.0128
#>  4       -1.51  
#>  5        2.38  
#>  6        1.13  
#>  7        1.45  
#>  8        4.02  
#>  9        1.82  
#> 10        2.47
```

## Scenario 2- Append new scores

There may be the need to retain not only the new score, but when it was
determined and its history. This is usually possible because the source
of record possesses a unique key identifier per entity, such as
transaction ID or customer ID. In the example, `flights_id` is the
unique identifier.

In this example, the new scores will be stored in a new table called
`daily_scores`. The following code is part of the example preparation,
it creates the table and seeds it with a single row. This is not the
best way to create an empty table, but it’ll do for the purposes of this
example.

``` r
dbWriteTable(
  con, "daily_scores",
  tibble(
    flight_id = 0,
    score = 0,
    date = ""
  )
)
```

The plan is to use a SQL statement that most vendors support, is called
INSERT INTO SELECT. The idea is to use `dplyr` laziness to prepare the
data transformation and predictions, but it’s not going to be executed
until is parsed into SQL and sent as part of a another statement. The
INSERT INTO SELECT statement allows for the results of a query to be
saved in a table, and without leaving the database.

In this example, predictions are going to be executed for just the
records in the month of December. The data is filtered, and then
[`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md)
is used to create the new fit field. The results are then transformed to
match to the structure of the new `daily_scores` table.

``` r
new_predictions <- db_fligths %>%
  filter(month == 12) %>%
  tidypredict_to_column(model, vars = "score") %>%
  select(
    flight_id,
    score
  ) %>%
  mutate(date = "01/01/2018")
```

``` r
insert_scores <- build_sql("INSERT INTO daily_scores ", sql_render(new_predictions, con = con), con = con)
insert_scores
#> <SQL> INSERT INTO daily_scores SELECT `q01`.*, '01/01/2018' AS `date`
#> FROM (
#>   SELECT
#>     `flight_id`,
#>     (-3.59844229187029 + (`hour` * 1.38710560882252)) + (`distance` * -0.00307606912118568) AS `score`
#>   FROM `flights_table`
#>   WHERE (`month` = 12.0)
#> ) AS `q01`
```

As in the first scenario, this statement can be then passed on to the
database team, via documentation or an automated process. In cases where
the analyst has the responsibility to run the new SQL statement, or if R
is being used to automate the scoring, the next line can be used:

``` r
dbSendQuery(con, insert_scores)
#> <SQLiteResult>
#>   SQL  INSERT INTO daily_scores SELECT `q01`.*, '01/01/2018' AS `date`
#> FROM (
#>   SELECT
#>     `flight_id`,
#>     (-3.59844229187029 + (`hour` * 1.38710560882252)) + (`distance` * -0.00307606912118568) AS `score`
#>   FROM `flights_table`
#>   WHERE (`month` = 12.0)
#> ) AS `q01`
#>   ROWS Fetched: 0 [complete]
#>        Changed: 28135
```

A simple table join can be used to confirm that the new update worked.
For real life scenarios, a more sophisticated query should be performed
in order to only get the latest score. For this example, we simple
filter on the same date we inserted

``` r
tbl(con, "daily_scores") %>%
  inner_join(tbl(con, "flights_table"), by = "flight_id") %>%
  filter(date == "01/01/2018") %>%
  select(dep_delay, hour, distance, score, date)
#> Warning: Closing open result set, pending rows
#> # Source:   SQL [?? x 5]
#> # Database: sqlite 3.50.4 []
#>    dep_delay  hour distance   score date      
#>        <dbl> <dbl>    <dbl>   <dbl> <chr>     
#>  1        14    23     1617 23.3    01/01/2018
#>  2        18    23     1576 23.5    01/01/2018
#>  3        -7     5      529  1.71   01/01/2018
#>  4         5     5     1400 -0.969  01/01/2018
#>  5        -4     5     1089 -0.0128 01/01/2018
#>  6       -10     5     1576 -1.51   01/01/2018
#>  7        -4     5      569  1.59   01/01/2018
#>  8         1     5     1416 -1.02   01/01/2018
#>  9       -11     6      214  4.07   01/01/2018
#> 10       -10     6     1065  1.45   01/01/2018
#> # ℹ more rows
```
