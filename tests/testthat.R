library(testthat)
library(tidypredict)
library(purrr)
library(dplyr)
library(Cubist)
library(mlbench)
library(rlang)
library(partykit)
library(yaml)

if (requireNamespace("xgboost", quietly = TRUE)) library(xgboost)

test_check("tidypredict")
