# Cubist models

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✔ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✗ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✗ |

## `tidypredict_` functions

``` r

library(Cubist)
data("BostonHousing", package = "mlbench")

model <- Cubist::cubist(
  x = BostonHousing[, -14],
  y = BostonHousing$medv,
  committees = 3
)
```

- Create the R formula

  ``` r

  tidypredict_fit(model)
  #> ((ifelse(ifelse(is.na(nox), 0.55469, nox) > 0.668000012636185, 
  #>     pmin(pmax(-1.11 + ifelse(is.na(crim), 3.613523, crim) * -0.02 + 
  #>         ifelse(is.na(nox), 0.55469, nox) * 21.4 + ifelse(is.na(rm), 
  #>         6.2846, rm) * 0.1 + ifelse(is.na(age), 68.57, age) * 
  #>         -0.003 + ifelse(is.na(dis), 3.79504, dis) * 2.93 + ifelse(is.na(ptratio), 
  #>         18.45, ptratio) * -0.13 + ifelse(is.na(b), 356.674, b) * 
  #>         0.008 + ifelse(is.na(lstat), 12.653, lstat) * -0.33, 
  #>         0, na.rm = TRUE), 50, na.rm = TRUE), 0) + ifelse(ifelse(is.na(lstat), 
  #>     12.653, lstat) > 9.59000062942505 & ifelse(is.na(nox), 0.55469, 
  #>     nox) <= 0.668000012636185, pmin(pmax(23.57 + ifelse(is.na(crim), 
  #>     3.613523, crim) * 0.05 + ifelse(is.na(nox), 0.55469, nox) * 
  #>     -5.2 + ifelse(is.na(rm), 6.2846, rm) * 3.1 + ifelse(is.na(age), 
  #>     68.57, age) * -0.048 + ifelse(is.na(dis), 3.79504, dis) * 
  #>     -0.81 + ifelse(is.na(rad), 9.5, rad) * 0.02 + ifelse(is.na(tax), 
  #>     408.2, tax) * -0.0041 + ifelse(is.na(ptratio), 18.45, ptratio) * 
  #>     -0.71 + ifelse(is.na(b), 356.674, b) * 0.01 + ifelse(is.na(lstat), 
  #>     12.653, lstat) * -0.15, 0, na.rm = TRUE), 55, na.rm = TRUE), 
  #>     0) + ifelse(ifelse(is.na(lstat), 12.653, lstat) <= 9.59000062942505 & 
  #>     ifelse(is.na(rm), 6.2846, rm) <= 6.2260000705719, pmin(pmax(1.18 + 
  #>     ifelse(is.na(crim), 3.613523, crim) * 3.83 + ifelse(is.na(rm), 
  #>     6.2846, rm) * 4.3 + ifelse(is.na(age), 68.57, age) * -0.06 + 
  #>     ifelse(is.na(dis), 3.79504, dis) * -0.09 + ifelse(is.na(tax), 
  #>     408.2, tax) * -0.003 + ifelse(is.na(ptratio), 18.45, ptratio) * 
  #>     -0.08 + ifelse(is.na(lstat), 12.653, lstat) * -0.11, 0, na.rm = TRUE), 
  #>     88.1, na.rm = TRUE), 0) + ifelse(ifelse(is.na(lstat), 12.653, 
  #>     lstat) <= 9.59000062942505 & ifelse(is.na(rm), 6.2846, rm) > 
  #>     6.2260000705719, pmin(pmax(-4.71 + ifelse(is.na(crim), 3.613523, 
  #>     crim) * 2.22 + ifelse(is.na(zn), 11.36, zn) * 0.008 + ifelse(is.na(nox), 
  #>     0.55469, nox) * -1.7 + ifelse(is.na(rm), 6.2846, rm) * 9.2 + 
  #>     ifelse(is.na(age), 68.57, age) * -0.04 + ifelse(is.na(dis), 
  #>     3.79504, dis) * -0.71 + ifelse(is.na(rad), 9.5, rad) * 0.03 + 
  #>     ifelse(is.na(tax), 408.2, tax) * -0.0182 + ifelse(is.na(ptratio), 
  #>     18.45, ptratio) * -0.72 + ifelse(is.na(lstat), 12.653, lstat) * 
  #>     -0.83, 0, na.rm = TRUE), 83.5, na.rm = TRUE), 0))/((ifelse(is.na(nox), 
  #>     0.55469, nox) > 0.668000012636185) + (ifelse(is.na(lstat), 
  #>     12.653, lstat) > 9.59000062942505 & ifelse(is.na(nox), 0.55469, 
  #>     nox) <= 0.668000012636185) + (ifelse(is.na(lstat), 12.653, 
  #>     lstat) <= 9.59000062942505 & ifelse(is.na(rm), 6.2846, rm) <= 
  #>     6.2260000705719) + (ifelse(is.na(lstat), 12.653, lstat) <= 
  #>     9.59000062942505 & ifelse(is.na(rm), 6.2846, rm) > 6.2260000705719)) + 
  #>     (ifelse(ifelse(is.na(dis), 3.79504, dis) <= 1.75540000200272 & 
  #>         ifelse(is.na(lstat), 12.653, lstat) > 5.12000012397766, 
  #>         pmin(pmax(122.32 + ifelse(is.na(crim), 3.613523, crim) * 
  #>             -0.29 + ifelse(is.na(nox), 0.55469, nox) * -21.6 + 
  #>             ifelse(is.na(rm), 6.2846, rm) * -3 + ifelse(is.na(dis), 
  #>             3.79504, dis) * -30.88 + ifelse(is.na(rad), 9.5, 
  #>             rad) * 0.02 + ifelse(is.na(tax), 408.2, tax) * -0.001 + 
  #>             ifelse(is.na(b), 356.674, b) * -0.023 + ifelse(is.na(lstat), 
  #>             12.653, lstat) * -0.73, 0, na.rm = TRUE), 95, na.rm = TRUE), 
  #>         0) + ifelse(ifelse(is.na(rm), 6.2846, rm) <= 6.54500031471252 & 
  #>         ifelse(is.na(lstat), 12.653, lstat) > 5.12000012397766, 
  #>         pmin(pmax(27.8 + ifelse(is.na(crim), 3.613523, crim) * 
  #>             -0.16 + ifelse(is.na(zn), 11.36, zn) * 0.007 + ifelse(is.na(nox), 
  #>             0.55469, nox) * -3.9 + ifelse(is.na(rm), 6.2846, 
  #>             rm) * 2 + ifelse(is.na(age), 68.57, age) * -0.035 + 
  #>             ifelse(is.na(dis), 3.79504, dis) * -0.7 + ifelse(is.na(rad), 
  #>             9.5, rad) * 0.28 + ifelse(is.na(tax), 408.2, tax) * 
  #>             -0.0135 + ifelse(is.na(ptratio), 18.45, ptratio) * 
  #>             -0.6 + ifelse(is.na(b), 356.674, b) * 0.013 + ifelse(is.na(lstat), 
  #>             12.653, lstat) * -0.25, 0, na.rm = TRUE), 95, na.rm = TRUE), 
  #>         0) + ifelse(ifelse(is.na(rm), 6.2846, rm) > 6.54500031471252 & 
  #>         ifelse(is.na(lstat), 12.653, lstat) > 5.12000012397766, 
  #>         pmin(pmax(22.21 + ifelse(is.na(crim), 3.613523, crim) * 
  #>             -0.04 + ifelse(is.na(zn), 11.36, zn) * 0.01 + ifelse(is.na(indus), 
  #>             11.136, indus) * -0.02 + ifelse(is.na(nox), 0.55469, 
  #>             nox) * -4 + ifelse(is.na(rm), 6.2846, rm) * 4.7 + 
  #>             ifelse(is.na(dis), 3.79504, dis) * -0.34 + ifelse(is.na(rad), 
  #>             9.5, rad) * 0.11 + ifelse(is.na(tax), 408.2, tax) * 
  #>             -0.0248 + ifelse(is.na(ptratio), 18.45, ptratio) * 
  #>             -0.9 + ifelse(is.na(b), 356.674, b) * 0.002 + ifelse(is.na(lstat), 
  #>             12.653, lstat) * -0.1, 0, na.rm = TRUE), 92.5, na.rm = TRUE), 
  #>         0) + ifelse(ifelse(is.na(lstat), 12.653, lstat) <= 5.12000012397766 & 
  #>         ifelse(is.na(rm), 6.2846, rm) <= 8.03400087356567, pmin(pmax(-71.95 + 
  #>         ifelse(is.na(rm), 6.2846, rm) * 17 + ifelse(is.na(age), 
  #>         68.57, age) * -0.06 + ifelse(is.na(tax), 408.2, tax) * 
  #>         -0.0112 + ifelse(is.na(ptratio), 18.45, ptratio) * -0.48 + 
  #>         ifelse(is.na(lstat), 12.653, lstat) * -0.03, 0, na.rm = TRUE), 
  #>         77.5, na.rm = TRUE), 0) + ifelse(ifelse(is.na(rm), 6.2846, 
  #>         rm) > 8.03400087356567 & ifelse(is.na(dis), 3.79504, 
  #>         dis) > 3.19920003414154, pmin(pmax(-32.79 + ifelse(is.na(crim), 
  #>         3.613523, crim) * -0.01 + ifelse(is.na(zn), 11.36, zn) * 
  #>         0.005 + ifelse(is.na(nox), 0.55469, nox) * -1.8 + ifelse(is.na(rm), 
  #>         6.2846, rm) * 12.9 + ifelse(is.na(age), 68.57, age) * 
  #>         -0.117 + ifelse(is.na(dis), 3.79504, dis) * -0.15 + ifelse(is.na(rad), 
  #>         9.5, rad) * 0.04 + ifelse(is.na(tax), 408.2, tax) * -0.0246 + 
  #>         ifelse(is.na(ptratio), 18.45, ptratio) * -1.05 + ifelse(is.na(lstat), 
  #>         12.653, lstat) * -0.04, 26.9, na.rm = TRUE), 59, na.rm = TRUE), 
  #>         0) + ifelse(ifelse(is.na(lstat), 12.653, lstat) <= 5.12000012397766 & 
  #>         ifelse(is.na(dis), 3.79504, dis) <= 3.19920003414154, 
  #>         pmin(pmax(53.41 + ifelse(is.na(rm), 6.2846, rm) * 1.6 + 
  #>             ifelse(is.na(dis), 3.79504, dis) * -7.16 + ifelse(is.na(tax), 
  #>             408.2, tax) * 0.0088 + ifelse(is.na(lstat), 12.653, 
  #>             lstat) * -0.68, 24.4, na.rm = TRUE), 62.8, na.rm = TRUE), 
  #>         0))/((ifelse(is.na(dis), 3.79504, dis) <= 1.75540000200272 & 
  #>         ifelse(is.na(lstat), 12.653, lstat) > 5.12000012397766) + 
  #>         (ifelse(is.na(rm), 6.2846, rm) <= 6.54500031471252 & 
  #>             ifelse(is.na(lstat), 12.653, lstat) > 5.12000012397766) + 
  #>         (ifelse(is.na(rm), 6.2846, rm) > 6.54500031471252 & ifelse(is.na(lstat), 
  #>             12.653, lstat) > 5.12000012397766) + (ifelse(is.na(lstat), 
  #>         12.653, lstat) <= 5.12000012397766 & ifelse(is.na(rm), 
  #>         6.2846, rm) <= 8.03400087356567) + (ifelse(is.na(rm), 
  #>         6.2846, rm) > 8.03400087356567 & ifelse(is.na(dis), 3.79504, 
  #>         dis) > 3.19920003414154) + (ifelse(is.na(lstat), 12.653, 
  #>         lstat) <= 5.12000012397766 & ifelse(is.na(dis), 3.79504, 
  #>         dis) <= 3.19920003414154)) + (ifelse(ifelse(is.na(nox), 
  #>     0.55469, nox) > 0.668000012636185, pmin(pmax(-36.31 + ifelse(is.na(crim), 
  #>     3.613523, crim) * 0.08 + ifelse(is.na(nox), 0.55469, nox) * 
  #>     48.4 + ifelse(is.na(dis), 3.79504, dis) * 7.52 + ifelse(is.na(b), 
  #>     356.674, b) * 0.01 + ifelse(is.na(lstat), 12.653, lstat) * 
  #>     -0.24, 0, na.rm = TRUE), 50, na.rm = TRUE), 0) + ifelse(ifelse(is.na(lstat), 
  #>     12.653, lstat) > 9.53000020980835 & ifelse(is.na(nox), 0.55469, 
  #>     nox) <= 0.668000012636185, pmin(pmax(28.04 + ifelse(is.na(nox), 
  #>     0.55469, nox) * -4.8 + ifelse(is.na(rm), 6.2846, rm) * 2.9 + 
  #>     ifelse(is.na(age), 68.57, age) * -0.051 + ifelse(is.na(dis), 
  #>     3.79504, dis) * -0.86 + ifelse(is.na(rad), 9.5, rad) * 0.01 + 
  #>     ifelse(is.na(tax), 408.2, tax) * -0.0019 + ifelse(is.na(ptratio), 
  #>     18.45, ptratio) * -0.72 + ifelse(is.na(lstat), 12.653, lstat) * 
  #>     -0.12, 0, na.rm = TRUE), 60.6, na.rm = TRUE), 0) + ifelse(ifelse(is.na(lstat), 
  #>     12.653, lstat) <= 9.53000020980835, pmin(pmax(-26.05 + ifelse(is.na(crim), 
  #>     3.613523, crim) * 0.89 + ifelse(is.na(nox), 0.55469, nox) * 
  #>     -2.3 + ifelse(is.na(rm), 6.2846, rm) * 9.6 + ifelse(is.na(dis), 
  #>     3.79504, dis) * -0.17 + ifelse(is.na(rad), 9.5, rad) * 0.02 + 
  #>     ifelse(is.na(tax), 408.2, tax) * -0.0055 + ifelse(is.na(ptratio), 
  #>     18.45, ptratio) * -0.12 + ifelse(is.na(b), 356.674, b) * 
  #>     0.001 + ifelse(is.na(lstat), 12.653, lstat) * -0.74, 0, na.rm = TRUE), 
  #>     88.1, na.rm = TRUE), 0) + ifelse(ifelse(is.na(lstat), 12.653, 
  #>     lstat) <= 9.53000020980835 & ifelse(is.na(dis), 3.79504, 
  #>     dis) <= 2.64030015468597, pmin(pmax(136.67 + ifelse(is.na(crim), 
  #>     3.613523, crim) * 7.2 + ifelse(is.na(nox), 0.55469, nox) * 
  #>     -96.6 + ifelse(is.na(rm), 6.2846, rm) * 1.1 + ifelse(is.na(tax), 
  #>     408.2, tax) * -0.0033 + ifelse(is.na(ptratio), 18.45, ptratio) * 
  #>     -3.31 + ifelse(is.na(lstat), 12.653, lstat) * -0.1, 0, na.rm = TRUE), 
  #>     88.1, na.rm = TRUE), 0))/((ifelse(is.na(nox), 0.55469, nox) > 
  #>     0.668000012636185) + (ifelse(is.na(lstat), 12.653, lstat) > 
  #>     9.53000020980835 & ifelse(is.na(nox), 0.55469, nox) <= 0.668000012636185) + 
  #>     (ifelse(is.na(lstat), 12.653, lstat) <= 9.53000020980835) + 
  #>     (ifelse(is.na(lstat), 12.653, lstat) <= 9.53000020980835 & 
  #>         ifelse(is.na(dis), 3.79504, dis) <= 2.64030015468597)))/3
  ```

- SQL output example

  ``` r

  tidypredict_sql(model, dbplyr::simulate_odbc())
  #> <SQL> ((((((CASE WHEN (CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END > 0.668000012636185) THEN (LEAST(GREATEST((((((((-1.11 + CASE WHEN (("crim" IS NULL)) THEN 3.613523 WHEN NOT (("crim" IS NULL)) THEN "crim" END * -0.02) + CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END * 21.4) + CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END * 0.1) + CASE WHEN (("age" IS NULL)) THEN 68.57 WHEN NOT (("age" IS NULL)) THEN "age" END * -0.003) + CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END * 2.93) + CASE WHEN (("ptratio" IS NULL)) THEN 18.45 WHEN NOT (("ptratio" IS NULL)) THEN "ptratio" END * -0.13) + CASE WHEN (("b" IS NULL)) THEN 356.674 WHEN NOT (("b" IS NULL)) THEN "b" END * 0.008) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END * -0.33, 0.0), 50.0)) WHEN NOT (CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END > 0.668000012636185) THEN 0.0 END + CASE WHEN (CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END > 9.59000062942505 AND CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END <= 0.668000012636185) THEN (LEAST(GREATEST((((((((((23.57 + CASE WHEN (("crim" IS NULL)) THEN 3.613523 WHEN NOT (("crim" IS NULL)) THEN "crim" END * 0.05) + CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END * -5.2) + CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END * 3.1) + CASE WHEN (("age" IS NULL)) THEN 68.57 WHEN NOT (("age" IS NULL)) THEN "age" END * -0.048) + CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END * -0.81) + CASE WHEN (("rad" IS NULL)) THEN 9.5 WHEN NOT (("rad" IS NULL)) THEN "rad" END * 0.02) + CASE WHEN (("tax" IS NULL)) THEN 408.2 WHEN NOT (("tax" IS NULL)) THEN "tax" END * -0.0041) + CASE WHEN (("ptratio" IS NULL)) THEN 18.45 WHEN NOT (("ptratio" IS NULL)) THEN "ptratio" END * -0.71) + CASE WHEN (("b" IS NULL)) THEN 356.674 WHEN NOT (("b" IS NULL)) THEN "b" END * 0.01) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END * -0.15, 0.0), 55.0)) WHEN NOT (CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END > 9.59000062942505 AND CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END <= 0.668000012636185) THEN 0.0 END) + CASE WHEN (CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 9.59000062942505 AND CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END <= 6.2260000705719) THEN (LEAST(GREATEST(((((((1.18 + CASE WHEN (("crim" IS NULL)) THEN 3.613523 WHEN NOT (("crim" IS NULL)) THEN "crim" END * 3.83) + CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END * 4.3) + CASE WHEN (("age" IS NULL)) THEN 68.57 WHEN NOT (("age" IS NULL)) THEN "age" END * -0.06) + CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END * -0.09) + CASE WHEN (("tax" IS NULL)) THEN 408.2 WHEN NOT (("tax" IS NULL)) THEN "tax" END * -0.003) + CASE WHEN (("ptratio" IS NULL)) THEN 18.45 WHEN NOT (("ptratio" IS NULL)) THEN "ptratio" END * -0.08) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END * -0.11, 0.0), 88.1)) WHEN NOT (CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 9.59000062942505 AND CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END <= 6.2260000705719) THEN 0.0 END) + CASE WHEN (CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 9.59000062942505 AND CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END > 6.2260000705719) THEN (LEAST(GREATEST((((((((((-4.71 + CASE WHEN (("crim" IS NULL)) THEN 3.613523 WHEN NOT (("crim" IS NULL)) THEN "crim" END * 2.22) + CASE WHEN (("zn" IS NULL)) THEN 11.36 WHEN NOT (("zn" IS NULL)) THEN "zn" END * 0.008) + CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END * -1.7) + CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END * 9.2) + CASE WHEN (("age" IS NULL)) THEN 68.57 WHEN NOT (("age" IS NULL)) THEN "age" END * -0.04) + CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END * -0.71) + CASE WHEN (("rad" IS NULL)) THEN 9.5 WHEN NOT (("rad" IS NULL)) THEN "rad" END * 0.03) + CASE WHEN (("tax" IS NULL)) THEN 408.2 WHEN NOT (("tax" IS NULL)) THEN "tax" END * -0.0182) + CASE WHEN (("ptratio" IS NULL)) THEN 18.45 WHEN NOT (("ptratio" IS NULL)) THEN "ptratio" END * -0.72) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END * -0.83, 0.0), 83.5)) WHEN NOT (CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 9.59000062942505 AND CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END > 6.2260000705719) THEN 0.0 END) / (((CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END > 0.668000012636185 + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END > 9.59000062942505 AND CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END <= 0.668000012636185) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 9.59000062942505 AND CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END <= 6.2260000705719) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 9.59000062942505 AND CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END > 6.2260000705719)) + (((((CASE WHEN (CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END <= 1.75540000200272 AND CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END > 5.12000012397766) THEN (LEAST(GREATEST((((((((122.32 + CASE WHEN (("crim" IS NULL)) THEN 3.613523 WHEN NOT (("crim" IS NULL)) THEN "crim" END * -0.29) + CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END * -21.6) + CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END * -3.0) + CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END * -30.88) + CASE WHEN (("rad" IS NULL)) THEN 9.5 WHEN NOT (("rad" IS NULL)) THEN "rad" END * 0.02) + CASE WHEN (("tax" IS NULL)) THEN 408.2 WHEN NOT (("tax" IS NULL)) THEN "tax" END * -0.001) + CASE WHEN (("b" IS NULL)) THEN 356.674 WHEN NOT (("b" IS NULL)) THEN "b" END * -0.023) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END * -0.73, 0.0), 95.0)) WHEN NOT (CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END <= 1.75540000200272 AND CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END > 5.12000012397766) THEN 0.0 END + CASE WHEN (CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END <= 6.54500031471252 AND CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END > 5.12000012397766) THEN (LEAST(GREATEST(((((((((((27.8 + CASE WHEN (("crim" IS NULL)) THEN 3.613523 WHEN NOT (("crim" IS NULL)) THEN "crim" END * -0.16) + CASE WHEN (("zn" IS NULL)) THEN 11.36 WHEN NOT (("zn" IS NULL)) THEN "zn" END * 0.007) + CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END * -3.9) + CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END * 2.0) + CASE WHEN (("age" IS NULL)) THEN 68.57 WHEN NOT (("age" IS NULL)) THEN "age" END * -0.035) + CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END * -0.7) + CASE WHEN (("rad" IS NULL)) THEN 9.5 WHEN NOT (("rad" IS NULL)) THEN "rad" END * 0.28) + CASE WHEN (("tax" IS NULL)) THEN 408.2 WHEN NOT (("tax" IS NULL)) THEN "tax" END * -0.0135) + CASE WHEN (("ptratio" IS NULL)) THEN 18.45 WHEN NOT (("ptratio" IS NULL)) THEN "ptratio" END * -0.6) + CASE WHEN (("b" IS NULL)) THEN 356.674 WHEN NOT (("b" IS NULL)) THEN "b" END * 0.013) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END * -0.25, 0.0), 95.0)) WHEN NOT (CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END <= 6.54500031471252 AND CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END > 5.12000012397766) THEN 0.0 END) + CASE WHEN (CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END > 6.54500031471252 AND CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END > 5.12000012397766) THEN (LEAST(GREATEST(((((((((((22.21 + CASE WHEN (("crim" IS NULL)) THEN 3.613523 WHEN NOT (("crim" IS NULL)) THEN "crim" END * -0.04) + CASE WHEN (("zn" IS NULL)) THEN 11.36 WHEN NOT (("zn" IS NULL)) THEN "zn" END * 0.01) + CASE WHEN (("indus" IS NULL)) THEN 11.136 WHEN NOT (("indus" IS NULL)) THEN "indus" END * -0.02) + CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END * -4.0) + CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END * 4.7) + CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END * -0.34) + CASE WHEN (("rad" IS NULL)) THEN 9.5 WHEN NOT (("rad" IS NULL)) THEN "rad" END * 0.11) + CASE WHEN (("tax" IS NULL)) THEN 408.2 WHEN NOT (("tax" IS NULL)) THEN "tax" END * -0.0248) + CASE WHEN (("ptratio" IS NULL)) THEN 18.45 WHEN NOT (("ptratio" IS NULL)) THEN "ptratio" END * -0.9) + CASE WHEN (("b" IS NULL)) THEN 356.674 WHEN NOT (("b" IS NULL)) THEN "b" END * 0.002) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END * -0.1, 0.0), 92.5)) WHEN NOT (CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END > 6.54500031471252 AND CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END > 5.12000012397766) THEN 0.0 END) + CASE WHEN (CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 5.12000012397766 AND CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END <= 8.03400087356567) THEN (LEAST(GREATEST(((((-71.95 + CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END * 17.0) + CASE WHEN (("age" IS NULL)) THEN 68.57 WHEN NOT (("age" IS NULL)) THEN "age" END * -0.06) + CASE WHEN (("tax" IS NULL)) THEN 408.2 WHEN NOT (("tax" IS NULL)) THEN "tax" END * -0.0112) + CASE WHEN (("ptratio" IS NULL)) THEN 18.45 WHEN NOT (("ptratio" IS NULL)) THEN "ptratio" END * -0.48) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END * -0.03, 0.0), 77.5)) WHEN NOT (CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 5.12000012397766 AND CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END <= 8.03400087356567) THEN 0.0 END) + CASE WHEN (CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END > 8.03400087356567 AND CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END > 3.19920003414154) THEN (LEAST(GREATEST((((((((((-32.79 + CASE WHEN (("crim" IS NULL)) THEN 3.613523 WHEN NOT (("crim" IS NULL)) THEN "crim" END * -0.01) + CASE WHEN (("zn" IS NULL)) THEN 11.36 WHEN NOT (("zn" IS NULL)) THEN "zn" END * 0.005) + CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END * -1.8) + CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END * 12.9) + CASE WHEN (("age" IS NULL)) THEN 68.57 WHEN NOT (("age" IS NULL)) THEN "age" END * -0.117) + CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END * -0.15) + CASE WHEN (("rad" IS NULL)) THEN 9.5 WHEN NOT (("rad" IS NULL)) THEN "rad" END * 0.04) + CASE WHEN (("tax" IS NULL)) THEN 408.2 WHEN NOT (("tax" IS NULL)) THEN "tax" END * -0.0246) + CASE WHEN (("ptratio" IS NULL)) THEN 18.45 WHEN NOT (("ptratio" IS NULL)) THEN "ptratio" END * -1.05) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END * -0.04, 26.9), 59)) WHEN NOT (CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END > 8.03400087356567 AND CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END > 3.19920003414154) THEN 0.0 END) + CASE WHEN (CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 5.12000012397766 AND CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END <= 3.19920003414154) THEN (LEAST(GREATEST((((53.41 + CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END * 1.6) + CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END * -7.16) + CASE WHEN (("tax" IS NULL)) THEN 408.2 WHEN NOT (("tax" IS NULL)) THEN "tax" END * 0.0088) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END * -0.68, 24.4), 62.8)) WHEN NOT (CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 5.12000012397766 AND CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END <= 3.19920003414154) THEN 0.0 END) / (((((CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END <= 1.75540000200272 AND CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END > 5.12000012397766 + CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END <= 6.54500031471252 AND CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END > 5.12000012397766) + CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END > 6.54500031471252 AND CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END > 5.12000012397766) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 5.12000012397766 AND CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END <= 8.03400087356567) + CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END > 8.03400087356567 AND CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END > 3.19920003414154) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 5.12000012397766 AND CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END <= 3.19920003414154)) + (((CASE WHEN (CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END > 0.668000012636185) THEN (LEAST(GREATEST(((((-36.31 + CASE WHEN (("crim" IS NULL)) THEN 3.613523 WHEN NOT (("crim" IS NULL)) THEN "crim" END * 0.08) + CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END * 48.4) + CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END * 7.52) + CASE WHEN (("b" IS NULL)) THEN 356.674 WHEN NOT (("b" IS NULL)) THEN "b" END * 0.01) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END * -0.24, 0.0), 50.0)) WHEN NOT (CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END > 0.668000012636185) THEN 0.0 END + CASE WHEN (CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END > 9.53000020980835 AND CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END <= 0.668000012636185) THEN (LEAST(GREATEST((((((((28.04 + CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END * -4.8) + CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END * 2.9) + CASE WHEN (("age" IS NULL)) THEN 68.57 WHEN NOT (("age" IS NULL)) THEN "age" END * -0.051) + CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END * -0.86) + CASE WHEN (("rad" IS NULL)) THEN 9.5 WHEN NOT (("rad" IS NULL)) THEN "rad" END * 0.01) + CASE WHEN (("tax" IS NULL)) THEN 408.2 WHEN NOT (("tax" IS NULL)) THEN "tax" END * -0.0019) + CASE WHEN (("ptratio" IS NULL)) THEN 18.45 WHEN NOT (("ptratio" IS NULL)) THEN "ptratio" END * -0.72) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END * -0.12, 0.0), 60.6)) WHEN NOT (CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END > 9.53000020980835 AND CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END <= 0.668000012636185) THEN 0.0 END) + CASE WHEN (CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 9.53000020980835) THEN (LEAST(GREATEST(((((((((-26.05 + CASE WHEN (("crim" IS NULL)) THEN 3.613523 WHEN NOT (("crim" IS NULL)) THEN "crim" END * 0.89) + CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END * -2.3) + CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END * 9.6) + CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END * -0.17) + CASE WHEN (("rad" IS NULL)) THEN 9.5 WHEN NOT (("rad" IS NULL)) THEN "rad" END * 0.02) + CASE WHEN (("tax" IS NULL)) THEN 408.2 WHEN NOT (("tax" IS NULL)) THEN "tax" END * -0.0055) + CASE WHEN (("ptratio" IS NULL)) THEN 18.45 WHEN NOT (("ptratio" IS NULL)) THEN "ptratio" END * -0.12) + CASE WHEN (("b" IS NULL)) THEN 356.674 WHEN NOT (("b" IS NULL)) THEN "b" END * 0.001) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END * -0.74, 0.0), 88.1)) WHEN NOT (CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 9.53000020980835) THEN 0.0 END) + CASE WHEN (CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 9.53000020980835 AND CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END <= 2.64030015468597) THEN (LEAST(GREATEST((((((136.67 + CASE WHEN (("crim" IS NULL)) THEN 3.613523 WHEN NOT (("crim" IS NULL)) THEN "crim" END * 7.2) + CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END * -96.6) + CASE WHEN (("rm" IS NULL)) THEN 6.2846 WHEN NOT (("rm" IS NULL)) THEN "rm" END * 1.1) + CASE WHEN (("tax" IS NULL)) THEN 408.2 WHEN NOT (("tax" IS NULL)) THEN "tax" END * -0.0033) + CASE WHEN (("ptratio" IS NULL)) THEN 18.45 WHEN NOT (("ptratio" IS NULL)) THEN "ptratio" END * -3.31) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END * -0.1, 0.0), 88.1)) WHEN NOT (CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 9.53000020980835 AND CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END <= 2.64030015468597) THEN 0.0 END) / (((CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END > 0.668000012636185 + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END > 9.53000020980835 AND CASE WHEN (("nox" IS NULL)) THEN 0.55469 WHEN NOT (("nox" IS NULL)) THEN "nox" END <= 0.668000012636185) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 9.53000020980835) + CASE WHEN (("lstat" IS NULL)) THEN 12.653 WHEN NOT (("lstat" IS NULL)) THEN "lstat" END <= 9.53000020980835 AND CASE WHEN (("dis" IS NULL)) THEN 3.79504 WHEN NOT (("dis" IS NULL)) THEN "dis" END <= 2.64030015468597)) / 3.0
  ```

- Add the prediction to the original table

  ``` r

  library(dplyr)

  BostonHousing %>%
    tidypredict_to_column(model) %>%
    glimpse()
  #> Rows: 506
  #> Columns: 15
  #> $ crim    <dbl> 0.00632, 0.02731, 0.02729, 0.03237, 0.06905, 0.02985,…
  #> $ zn      <dbl> 18.0, 0.0, 0.0, 0.0, 0.0, 0.0, 12.5, 12.5, 12.5, 12.5…
  #> $ indus   <dbl> 2.31, 7.07, 7.07, 2.18, 2.18, 2.18, 7.87, 7.87, 7.87,…
  #> $ chas    <fct> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
  #> $ nox     <dbl> 0.538, 0.469, 0.469, 0.458, 0.458, 0.458, 0.524, 0.52…
  #> $ rm      <dbl> 6.575, 6.421, 7.185, 6.998, 7.147, 6.430, 6.012, 6.17…
  #> $ age     <dbl> 65.2, 78.9, 61.1, 45.8, 54.2, 58.7, 66.6, 96.1, 100.0…
  #> $ dis     <dbl> 4.0900, 4.9671, 4.9671, 6.0622, 6.0622, 6.0622, 5.560…
  #> $ rad     <dbl> 1, 2, 2, 3, 3, 3, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4,…
  #> $ tax     <dbl> 296, 242, 242, 222, 222, 222, 311, 311, 311, 311, 311…
  #> $ ptratio <dbl> 15.3, 17.8, 17.8, 18.7, 18.7, 18.7, 15.2, 15.2, 15.2,…
  #> $ b       <dbl> 396.90, 396.90, 392.83, 394.63, 396.90, 394.12, 395.6…
  #> $ lstat   <dbl> 4.98, 9.14, 4.03, 2.94, 5.33, 5.21, 12.43, 19.15, 29.…
  #> $ medv    <dbl> 24.0, 21.6, 34.7, 33.4, 36.2, 28.7, 22.9, 27.1, 16.5,…
  #> $ fit     <dbl> 27.50665, 22.71805, 34.78128, 33.19372, 31.93653, 25.…
  ```

We are not able to give an exact match of the original predictions [due
to a minor bug](https://github.com/topepo/Cubist/issues/62) in Cubist.

## Parse model spec

Here is an example of the model spec:

``` r

pm <- parse_model(model)
str(pm, 2)
#> List of 2
#>  $ general:List of 7
#>   ..$ model       : chr "cubist"
#>   ..$ type        : chr "tree"
#>   ..$ version     : num 3
#>   ..$ mode        : chr "ifelse"
#>   ..$ n_committees: num 3
#>   ..$ ommittee_id : int [1:14] 1 1 1 1 2 2 2 2 2 2 ...
#>   ..$ means       :List of 12
#>  $ trees  :List of 1
#>   ..$ :List of 14
#>  - attr(*, "class")= chr [1:3] "parsed_model" "pm_tree" "list"
```

``` r

str(pm$terms[1:2])
#>  NULL
```

## Limitations

- [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  is not supported
- Prediction intervals are not supported
- Cubist uses 32-bit floats internally, which may cause prediction
  discrepancies at exact split boundaries. See the [float
  precision](https://tidypredict.tidymodels.org/articles/float-precision.md)
  article for details. The same 32-bit storage puts a *relative* ceiling
  of roughly `1e-7` on the agreement with
  [`predict()`](https://rdrr.io/r/stats/predict.html), so an outcome on
  a large scale leaves a correspondingly large absolute difference.
- The instance-based correction that
  [`predict()`](https://rdrr.io/r/stats/predict.html) applies when
  `neighbors` is greater than zero is not reproduced. It adjusts each
  prediction using the nearest training rows, which are not part of the
  fitted model, so no formula can stand in for it. Formulas from
  [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
  match `predict(model, newdata)` with its default `neighbors = 0` only.
