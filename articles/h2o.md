# Gradient boosting, using H2O

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md) |  |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) |  |
| `parsnip` |  |

## How it works

`tidypredict` supports H2O gradient boosting models (GBM) fitted through
the `parsnip` package with the `"h2o_gbm"` engine of
[`boost_tree()`](https://parsnip.tidymodels.org/reference/boost_tree.html),
which is provided by the `agua` package. Both regression and
classification are supported.

``` r

library(parsnip)
library(agua)

model <- boost_tree(mode = "regression", trees = 10) |>
  set_engine("h2o_gbm") |>
  fit(mpg ~ wt + cyl + hp, data = mtcars)
```

## Under the hood

An H2O model is a handle into a running H2O cluster. `tidypredict` pulls
each boosted tree from the cluster with
[`h2o::h2o.getModelTree()`](https://rdrr.io/pkg/h2o/man/h2o.getModelTree.html)
and turns it into a nested
[`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
statement. The individual tree contributions are added together (along
with the model’s initial value) to form the Tidy Eval formula. Because
the trees live in the cluster, an active `h2o.init()` connection is
required.

``` r

tidypredict_fit(model)
#> 20.090625 + (case_when(wt < 2.2596803 ~ case_when(wt < 1.8848817 ~ 
#>     case_when(wt < 1.7250888 ~ 3.0928125, .default = 4.1428123), 
#>     .default = case_when(wt < 2.1698422 ~ case_when(wt < 2.0377436 ~ 
#>         2.1628125, .default = 1.7728126), .default = 3.6928124)), 
#>     .default = case_when(cyl < 7 ~ case_when(wt < 3.326375 | 
#>         is.na(wt) ~ case_when(hp < 102.5 ~ case_when(wt < 3.1730468 | 
#>         is.na(wt) ~ case_when(hp < 96.5 ~ 0.81281245, .default = 0.4228125), 
#>         .default = 1.2928125), .default = case_when(hp < 141.5 | 
#>         is.na(hp) ~ case_when(cyl < 5 ~ 0.39281252, .default = 0.3128125), 
#>         .default = -0.1171875)), .default = case_when(wt < 3.4495606 ~ 
#>         -0.4771875, .default = -0.5971875)), .default = case_when(wt < 
#>         4.660125 | is.na(wt) ~ case_when(hp < 211 | is.na(hp) ~ 
#>         case_when(hp < 164 ~ case_when(wt < 3.4846094 ~ -1.4671875, 
#>             .default = -1.3771875), .default = case_when(hp < 
#>             177.5 ~ -0.3421875, .default = -1.1371875)), .default = case_when(hp < 
#>         254.5 ~ case_when(wt < 3.7039063 ~ -1.7371875, .default = -2.0371876), 
#>         .default = case_when(wt < 3.379375 ~ -1.2871876, .default = -1.5271875))), 
#>         .default = case_when(hp < 224.5 | is.na(hp) ~ -2.9071875, 
#>             .default = -1.6171875)))) + case_when(wt < 2.2596803 ~ 
#>     case_when(wt < 1.8848817 ~ case_when(wt < 1.7250888 ~ 2.1649687, 
#>         .default = 2.8999689), .default = case_when(wt < 2.1698422 ~ 
#>         case_when(wt < 2.0377436 ~ 1.513969, .default = 1.2409687), 
#>         .default = 2.5849686)), .default = case_when(cyl < 7 ~ 
#>     case_when(wt < 3.326375 | is.na(wt) ~ case_when(hp < 102.5 ~ 
#>         case_when(wt < 3.1730468 | is.na(wt) ~ case_when(hp < 
#>             96.5 ~ 0.568969, .default = 0.29596883), .default = 0.9049686), 
#>         .default = case_when(hp < 141.5 | is.na(hp) ~ case_when(wt < 
#>             3.0383594 | is.na(wt) ~ 0.210969, .default = 0.298969), 
#>             .default = -0.08203148)), .default = case_when(wt < 
#>         3.4495606 ~ -0.3340313, .default = -0.41803104)), .default = case_when(wt < 
#>     4.660125 | is.na(wt) ~ case_when(hp < 211 | is.na(hp) ~ case_when(hp < 
#>     164 ~ case_when(wt < 3.4846094 ~ -1.0270313, .default = -0.9640314), 
#>     .default = case_when(hp < 177.5 ~ -0.23953135, .default = -0.79603136)), 
#>     .default = case_when(hp < 254.5 ~ case_when(wt < 3.7039063 ~ 
#>         -1.2160312, .default = -1.4260314), .default = case_when(wt < 
#>         3.379375 ~ -0.9010315, .default = -1.0690315))), .default = case_when(hp < 
#>     224.5 | is.na(hp) ~ -2.035031, .default = -1.1320314)))) + 
#>     case_when(wt < 2.2596803 ~ case_when(wt < 1.8848817 ~ case_when(wt < 
#>         1.7250888 ~ 1.5154783, .default = 2.0299783), .default = case_when(wt < 
#>         2.1698422 ~ case_when(wt < 2.0377436 ~ 1.0597781, .default = 0.8686781), 
#>         .default = 1.8094782)), .default = case_when(cyl < 7 ~ 
#>         case_when(wt < 3.326375 | is.na(wt) ~ case_when(hp < 
#>             102.5 ~ case_when(wt < 3.1730468 | is.na(wt) ~ case_when(hp < 
#>             96.5 ~ 0.39827806, .default = 0.20717812), .default = 0.6334781), 
#>             .default = case_when(hp < 141.5 | is.na(hp) ~ case_when(cyl < 
#>                 5 ~ 0.21167804, .default = 0.1468781), .default = -0.0574221)), 
#>             .default = case_when(wt < 3.4495606 ~ -0.23382169, 
#>                 .default = -0.29262152)), .default = case_when(wt < 
#>         4.660125 | is.na(wt) ~ case_when(hp < 211 | is.na(hp) ~ 
#>         case_when(hp < 164 ~ case_when(wt < 3.4846094 ~ -0.7189221, 
#>             .default = -0.67482203), .default = case_when(hp < 
#>             177.5 ~ -0.1676722, .default = -0.5572218)), .default = case_when(hp < 
#>         254.5 ~ case_when(wt < 3.7039063 ~ -0.8512216, .default = -0.9982221), 
#>         .default = case_when(wt < 3.379375 ~ -0.63072205, .default = -0.7483223))), 
#>         .default = case_when(hp < 224.5 | is.na(hp) ~ -1.4245218, 
#>             .default = -0.7924223)))) + case_when(wt < 2.2596803 ~ 
#>     case_when(wt < 1.8848817 ~ case_when(wt < 1.7250888 ~ 1.0608349, 
#>         .default = 1.420985), .default = case_when(wt < 2.1698422 ~ 
#>         case_when(wt < 2.0377436 ~ 0.7418446, .default = 0.60807496), 
#>         .default = 1.2666345)), .default = case_when(cyl < 7 ~ 
#>     case_when(wt < 3.326375 | is.na(wt) ~ case_when(hp < 102.5 ~ 
#>         case_when(wt < 3.1730468 | is.na(wt) ~ case_when(hp < 
#>             96.5 ~ 0.2787949, .default = 0.14502469), .default = 0.44343463), 
#>         .default = case_when(hp < 141.5 | is.na(hp) ~ case_when(wt < 
#>             3.0383594 | is.na(wt) ~ 0.09713482, .default = 0.16521503), 
#>             .default = -0.040195312)), .default = case_when(wt < 
#>         3.4495606 ~ -0.16367513, .default = -0.20483504)), .default = case_when(wt < 
#>     4.660125 | is.na(wt) ~ case_when(hp < 211 | is.na(hp) ~ case_when(hp < 
#>     164 ~ case_when(wt < 3.4846094 ~ -0.5032456, .default = -0.47237548), 
#>     .default = case_when(hp < 177.5 ~ -0.11737026, .default = -0.39005503)), 
#>     .default = case_when(hp < 254.5 ~ case_when(wt < 3.7039063 ~ 
#>         -0.5958553, .default = -0.69875544), .default = case_when(wt < 
#>         3.379375 ~ -0.4415054, .default = -0.5238258))), .default = case_when(hp < 
#>     224.5 | is.na(hp) ~ -0.9971653, .default = -0.55469537)))) + 
#>     case_when(wt < 2.2596803 ~ case_when(wt < 1.8848817 ~ case_when(wt < 
#>         1.7250888 ~ 0.74258435, .default = 0.9946897), .default = case_when(wt < 
#>         2.1698422 ~ case_when(wt < 2.0377436 ~ 0.51929134, .default = 0.42565268), 
#>         .default = 0.8866443)), .default = case_when(cyl < 7 ~ 
#>         case_when(wt < 3.326375 | is.na(wt) ~ case_when(hp < 
#>             102.5 ~ case_when(wt < 3.1730468 | is.na(wt) ~ case_when(hp < 
#>             96.5 ~ 0.19515634, .default = 0.1015171), .default = 0.3104045), 
#>             .default = case_when(hp < 141.5 | is.na(hp) ~ case_when(cyl < 
#>                 5 ~ 0.11903412, .default = 0.06686634), .default = -0.028136672)), 
#>             .default = case_when(wt < 3.4495606 ~ -0.11457253, 
#>                 .default = -0.1433843)), .default = case_when(wt < 
#>         4.660125 | is.na(wt) ~ case_when(hp < 211 | is.na(hp) ~ 
#>         case_when(wt < 3.8098438 | is.na(wt) ~ case_when(wt < 
#>             3.7571313 | is.na(wt) ~ -0.20328303, .default = -0.6030386), 
#>             .default = case_when(wt < 3.9561818 ~ -0.007159081, 
#>                 .default = -0.24303864)), .default = case_when(hp < 
#>         254.5 ~ case_when(wt < 3.7039063 ~ -0.41709858, .default = -0.48912886), 
#>         .default = case_when(wt < 3.379375 ~ -0.30905378, .default = -0.36667785))), 
#>         .default = case_when(hp < 224.5 | is.na(hp) ~ -0.69801563, 
#>             .default = -0.3882868)))) + case_when(wt < 2.2596803 ~ 
#>     case_when(wt < 1.8848817 ~ case_when(wt < 1.7250888 ~ 0.51980907, 
#>         .default = 0.69628274), .default = case_when(wt < 2.1698422 ~ 
#>         case_when(wt < 2.0377436 ~ 0.36350405, .default = 0.29795665), 
#>         .default = 0.620651)), .default = case_when(cyl < 7 ~ 
#>     case_when(wt < 3.326375 | is.na(wt) ~ case_when(hp < 102.5 ~ 
#>         case_when(wt < 3.1730468 | is.na(wt) ~ case_when(hp < 
#>             96.5 ~ 0.1366095, .default = 0.07106209), .default = 0.21728335), 
#>         .default = case_when(hp < 141.5 | is.na(hp) ~ case_when(wt < 
#>             3.0383594 | is.na(wt) ~ 0.04271759, .default = 0.0955909), 
#>             .default = -0.01969551)), .default = case_when(wt < 
#>         3.4495606 ~ -0.08020077, .default = -0.100368805)), .default = case_when(wt < 
#>     4.660125 | is.na(wt) ~ case_when(hp < 211 | is.na(hp) ~ case_when(hp < 
#>     164 ~ case_when(wt < 3.4846094 ~ -0.2912867, .default = -0.26967773), 
#>     .default = case_when(wt < 3.7525 ~ -0.004113865, .default = -0.19908859)), 
#>     .default = case_when(hp < 254.5 ~ case_when(wt < 3.7039063 ~ 
#>         -0.29196888, .default = -0.34239012), .default = case_when(wt < 
#>         3.379375 ~ -0.21633774, .default = -0.25667438))), .default = case_when(hp < 
#>     224.5 | is.na(hp) ~ -0.4886111, .default = -0.27180085)))) + 
#>     case_when(cyl < 5 ~ case_when(wt < 2.2577705 | is.na(wt) ~ 
#>         case_when(wt < 1.8853853 ~ case_when(wt < 1.725376 ~ 
#>             0.36386615, .default = 0.48739767), .default = case_when(wt < 
#>             2.169276 | is.na(wt) ~ case_when(wt < 2.039326 ~ 
#>             0.25445274, .default = 0.20856972), .default = 0.4344556)), 
#>         .default = case_when(wt < 3.1699717 ~ case_when(hp < 
#>             96.5 ~ 0.09562649, .default = case_when(wt < 2.6247656 ~ 
#>             0.049743462, .default = 0.07050888)), .default = 0.15209839)), 
#>         .default = case_when(hp < 193 | is.na(hp) ~ case_when(wt < 
#>             3.3264766 ~ case_when(wt < 3.0449898 ~ case_when(hp < 
#>             143 | is.na(hp) ~ 0.009599304, .default = -0.013786927), 
#>             .default = 0.066913724), .default = case_when(wt < 
#>             3.8144138 | is.na(wt) ~ case_when(wt < 3.7551303 | 
#>             is.na(wt) ~ case_when(wt < 3.6250772 | is.na(wt) ~ 
#>             -0.11169243, .default = 0.0891806), .default = -0.36240026), 
#>             .default = case_when(wt < 3.9561841 ~ 0.05471512, 
#>                 .default = -0.11040047))), .default = case_when(hp < 
#>             222.5 ~ -0.34202775, .default = case_when(hp < 253.5 ~ 
#>             case_when(hp < 238 ~ -0.19026054, .default = case_when(wt < 
#>                 3.7086718 ~ -0.20437808, .default = -0.2396731)), 
#>             .default = case_when(wt < 3.3739061 ~ -0.15143657, 
#>                 .default = -0.17967196))))) + case_when(cyl < 
#>     5 ~ case_when(wt < 2.2577705 | is.na(wt) ~ case_when(wt < 
#>     1.8853853 ~ case_when(wt < 1.725376 ~ 0.2547061, .default = 0.3411788), 
#>     .default = case_when(wt < 2.169276 | is.na(wt) ~ case_when(wt < 
#>         2.039326 ~ 0.17811665, .default = 0.14599857), .default = 0.30411884)), 
#>     .default = case_when(wt < 3.1699717 ~ case_when(hp < 96.5 ~ 
#>         0.06693844, .default = case_when(wt < 2.6247656 ~ 0.034820367, 
#>         .default = 0.049356196)), .default = 0.10646908)), .default = case_when(cyl < 
#>     7 ~ case_when(wt < 3.3264766 | is.na(wt) ~ case_when(wt < 
#>     3.0449898 | is.na(wt) ~ case_when(hp < 143 | is.na(hp) ~ 
#>     0.0067193983, .default = -0.009651031), .default = 0.04683964), 
#>     .default = case_when(wt < 3.4495685 ~ -0.02263298, .default = -0.036750525)), 
#>     .default = case_when(wt < 4.662758 | is.na(wt) ~ case_when(hp < 
#>         163.5 ~ case_when(wt < 3.4723437 ~ -0.17039303, .default = -0.15526657), 
#>         .default = case_when(hp < 211.5 | is.na(hp) ~ case_when(wt < 
#>             3.7465625 ~ 0.0004972458, .default = -0.09755318), 
#>             .default = case_when(hp < 255.5 ~ -0.15541798, .default = -0.11588805))), 
#>         .default = case_when(hp < 224.5 | is.na(hp) ~ -0.23941945, 
#>             .default = -0.13318229)))) + case_when(cyl < 5 ~ 
#>     case_when(wt < 2.2577705 | is.na(wt) ~ case_when(wt < 1.8853853 ~ 
#>         case_when(wt < 1.725376 ~ 0.17829448, .default = 0.23882514), 
#>         .default = case_when(wt < 2.169276 | is.na(wt) ~ case_when(wt < 
#>             2.039326 ~ 0.1246819, .default = 0.102199174), .default = 0.2128831)), 
#>         .default = case_when(wt < 3.1699717 ~ case_when(hp < 
#>             96.5 ~ 0.046856917, .default = case_when(wt < 2.6247656 ~ 
#>             0.024374198, .default = 0.034549255)), .default = 0.07452862)), 
#>     .default = case_when(cyl < 7 ~ case_when(wt < 3.3264766 | 
#>         is.na(wt) ~ case_when(wt < 3.0449898 | is.na(wt) ~ case_when(hp < 
#>         143 | is.na(hp) ~ 0.0047035217, .default = -0.0067556766), 
#>         .default = 0.03278801), .default = case_when(wt < 3.4495685 ~ 
#>         -0.015843201, .default = -0.025725288)), .default = case_when(wt < 
#>         4.662758 | is.na(wt) ~ case_when(wt < 3.8114195 | is.na(wt) ~ 
#>         case_when(wt < 3.7512865 | is.na(wt) ~ case_when(wt < 
#>             3.651378 | is.na(wt) ~ -0.09137092, .default = 0.062277257), 
#>             .default = -0.22441429), .default = case_when(hp < 
#>         177.5 ~ 0.067566834, .default = case_when(wt < 3.955 ~ 
#>         -0.12114578, .default = -0.04801414))), .default = case_when(hp < 
#>         224.5 | is.na(hp) ~ -0.16759375, .default = -0.09322752)))) + 
#>     case_when(cyl < 5 ~ case_when(wt < 2.2577705 | is.na(wt) ~ 
#>         case_when(wt < 1.8853853 ~ case_when(wt < 1.725376 ~ 
#>             0.12480595, .default = 0.16717711), .default = case_when(wt < 
#>             2.169276 | is.na(wt) ~ case_when(wt < 2.039326 ~ 
#>             0.08727746, .default = 0.071539305), .default = 0.14901821)), 
#>         .default = case_when(wt < 3.1699717 ~ case_when(hp < 
#>             96.5 ~ 0.032799568, .default = case_when(wt < 2.6247656 ~ 
#>             0.017061997, .default = 0.02418434)), .default = 0.0521703)), 
#>         .default = case_when(cyl < 7 ~ case_when(wt < 3.3264766 | 
#>             is.na(wt) ~ case_when(wt < 3.0449898 | is.na(wt) ~ 
#>             case_when(hp < 143 | is.na(hp) ~ 0.0032924651, .default = -0.0047289277), 
#>             .default = 0.022951812), .default = case_when(wt < 
#>             3.4495685 ~ -0.01109047, .default = -0.018007964)), 
#>             .default = case_when(wt < 4.662758 | is.na(wt) ~ 
#>                 case_when(wt < 3.8114195 | is.na(wt) ~ case_when(wt < 
#>                   3.7512865 | is.na(wt) ~ case_when(wt < 3.651378 | 
#>                   is.na(wt) ~ -0.063959695, .default = 0.043594208), 
#>                   .default = -0.15709013), .default = case_when(hp < 
#>                   177.5 ~ 0.04729706, .default = case_when(wt < 
#>                   3.955 ~ -0.08480192, .default = -0.03361004))), 
#>                 .default = case_when(hp < 224.5 | is.na(hp) ~ 
#>                   -0.11731556, .default = -0.0652593)))))
```

From there, the Tidy Eval formula can be used anywhere it can be
operated. `tidypredict` provides these paths:

- Use directly inside `dplyr`,
  `mutate(mtcars, !! tidypredict_fit(model))`
- Use `tidypredict_sql(model)` to retrieve the SQL statement

``` r

tidypredict_sql(model, dbplyr::simulate_dbi())
#> <SQL> 20.090625 + ((((((((CASE
#> WHEN ("wt" < 2.2596803) THEN (CASE
#> WHEN ("wt" < 1.8848817) THEN (CASE WHEN ("wt" < 1.7250888) THEN 3.0928125 ELSE 4.1428123 END)
#> ELSE CASE
#> WHEN ("wt" < 2.1698422) THEN (CASE WHEN ("wt" < 2.0377436) THEN 2.1628125 ELSE 1.7728126 END)
#> ELSE 3.6928124
#> END
#> END)
#> ELSE CASE
#> WHEN ("cyl" < 7.0) THEN (CASE
#> WHEN ("wt" < 3.326375 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("hp" < 102.5) THEN (CASE
#> WHEN ("wt" < 3.1730468 OR ("wt" IS NULL)) THEN (CASE WHEN ("hp" < 96.5) THEN 0.81281245 ELSE 0.4228125 END)
#> ELSE 1.2928125
#> END)
#> ELSE CASE
#> WHEN ("hp" < 141.5 OR ("hp" IS NULL)) THEN (CASE WHEN ("cyl" < 5.0) THEN 0.39281252 ELSE 0.3128125 END)
#> ELSE -0.1171875
#> END
#> END)
#> ELSE CASE WHEN ("wt" < 3.4495606) THEN -0.4771875 ELSE -0.5971875 END
#> END)
#> ELSE CASE
#> WHEN ("wt" < 4.660125 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("hp" < 211.0 OR ("hp" IS NULL)) THEN (CASE
#> WHEN ("hp" < 164.0) THEN (CASE WHEN ("wt" < 3.4846094) THEN -1.4671875 ELSE -1.3771875 END)
#> ELSE CASE WHEN ("hp" < 177.5) THEN -0.3421875 ELSE -1.1371875 END
#> END)
#> ELSE CASE
#> WHEN ("hp" < 254.5) THEN (CASE WHEN ("wt" < 3.7039063) THEN -1.7371875 ELSE -2.0371876 END)
#> ELSE CASE WHEN ("wt" < 3.379375) THEN -1.2871876 ELSE -1.5271875 END
#> END
#> END)
#> ELSE CASE WHEN ("hp" < 224.5 OR ("hp" IS NULL)) THEN -2.9071875 ELSE -1.6171875 END
#> END
#> END
#> END + CASE
#> WHEN ("wt" < 2.2596803) THEN (CASE
#> WHEN ("wt" < 1.8848817) THEN (CASE WHEN ("wt" < 1.7250888) THEN 2.1649687 ELSE 2.8999689 END)
#> ELSE CASE
#> WHEN ("wt" < 2.1698422) THEN (CASE WHEN ("wt" < 2.0377436) THEN 1.513969 ELSE 1.2409687 END)
#> ELSE 2.5849686
#> END
#> END)
#> ELSE CASE
#> WHEN ("cyl" < 7.0) THEN (CASE
#> WHEN ("wt" < 3.326375 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("hp" < 102.5) THEN (CASE
#> WHEN ("wt" < 3.1730468 OR ("wt" IS NULL)) THEN (CASE WHEN ("hp" < 96.5) THEN 0.568969 ELSE 0.29596883 END)
#> ELSE 0.9049686
#> END)
#> ELSE CASE
#> WHEN ("hp" < 141.5 OR ("hp" IS NULL)) THEN (CASE WHEN ("wt" < 3.0383594 OR ("wt" IS NULL)) THEN 0.210969 ELSE 0.298969 END)
#> ELSE -0.08203148
#> END
#> END)
#> ELSE CASE WHEN ("wt" < 3.4495606) THEN -0.3340313 ELSE -0.41803104 END
#> END)
#> ELSE CASE
#> WHEN ("wt" < 4.660125 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("hp" < 211.0 OR ("hp" IS NULL)) THEN (CASE
#> WHEN ("hp" < 164.0) THEN (CASE WHEN ("wt" < 3.4846094) THEN -1.0270313 ELSE -0.9640314 END)
#> ELSE CASE WHEN ("hp" < 177.5) THEN -0.23953135 ELSE -0.79603136 END
#> END)
#> ELSE CASE
#> WHEN ("hp" < 254.5) THEN (CASE WHEN ("wt" < 3.7039063) THEN -1.2160312 ELSE -1.4260314 END)
#> ELSE CASE WHEN ("wt" < 3.379375) THEN -0.9010315 ELSE -1.0690315 END
#> END
#> END)
#> ELSE CASE WHEN ("hp" < 224.5 OR ("hp" IS NULL)) THEN -2.035031 ELSE -1.1320314 END
#> END
#> END
#> END) + CASE
#> WHEN ("wt" < 2.2596803) THEN (CASE
#> WHEN ("wt" < 1.8848817) THEN (CASE WHEN ("wt" < 1.7250888) THEN 1.5154783 ELSE 2.0299783 END)
#> ELSE CASE
#> WHEN ("wt" < 2.1698422) THEN (CASE WHEN ("wt" < 2.0377436) THEN 1.0597781 ELSE 0.8686781 END)
#> ELSE 1.8094782
#> END
#> END)
#> ELSE CASE
#> WHEN ("cyl" < 7.0) THEN (CASE
#> WHEN ("wt" < 3.326375 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("hp" < 102.5) THEN (CASE
#> WHEN ("wt" < 3.1730468 OR ("wt" IS NULL)) THEN (CASE WHEN ("hp" < 96.5) THEN 0.39827806 ELSE 0.20717812 END)
#> ELSE 0.6334781
#> END)
#> ELSE CASE
#> WHEN ("hp" < 141.5 OR ("hp" IS NULL)) THEN (CASE WHEN ("cyl" < 5.0) THEN 0.21167804 ELSE 0.1468781 END)
#> ELSE -0.0574221
#> END
#> END)
#> ELSE CASE WHEN ("wt" < 3.4495606) THEN -0.23382169 ELSE -0.29262152 END
#> END)
#> ELSE CASE
#> WHEN ("wt" < 4.660125 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("hp" < 211.0 OR ("hp" IS NULL)) THEN (CASE
#> WHEN ("hp" < 164.0) THEN (CASE WHEN ("wt" < 3.4846094) THEN -0.7189221 ELSE -0.67482203 END)
#> ELSE CASE WHEN ("hp" < 177.5) THEN -0.1676722 ELSE -0.5572218 END
#> END)
#> ELSE CASE
#> WHEN ("hp" < 254.5) THEN (CASE WHEN ("wt" < 3.7039063) THEN -0.8512216 ELSE -0.9982221 END)
#> ELSE CASE WHEN ("wt" < 3.379375) THEN -0.63072205 ELSE -0.7483223 END
#> END
#> END)
#> ELSE CASE WHEN ("hp" < 224.5 OR ("hp" IS NULL)) THEN -1.4245218 ELSE -0.7924223 END
#> END
#> END
#> END) + CASE
#> WHEN ("wt" < 2.2596803) THEN (CASE
#> WHEN ("wt" < 1.8848817) THEN (CASE WHEN ("wt" < 1.7250888) THEN 1.0608349 ELSE 1.420985 END)
#> ELSE CASE
#> WHEN ("wt" < 2.1698422) THEN (CASE WHEN ("wt" < 2.0377436) THEN 0.7418446 ELSE 0.60807496 END)
#> ELSE 1.2666345
#> END
#> END)
#> ELSE CASE
#> WHEN ("cyl" < 7.0) THEN (CASE
#> WHEN ("wt" < 3.326375 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("hp" < 102.5) THEN (CASE
#> WHEN ("wt" < 3.1730468 OR ("wt" IS NULL)) THEN (CASE WHEN ("hp" < 96.5) THEN 0.2787949 ELSE 0.14502469 END)
#> ELSE 0.44343463
#> END)
#> ELSE CASE
#> WHEN ("hp" < 141.5 OR ("hp" IS NULL)) THEN (CASE
#> WHEN ("wt" < 3.0383594 OR ("wt" IS NULL)) THEN 0.09713482
#> ELSE 0.16521503
#> END)
#> ELSE -0.040195312
#> END
#> END)
#> ELSE CASE WHEN ("wt" < 3.4495606) THEN -0.16367513 ELSE -0.20483504 END
#> END)
#> ELSE CASE
#> WHEN ("wt" < 4.660125 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("hp" < 211.0 OR ("hp" IS NULL)) THEN (CASE
#> WHEN ("hp" < 164.0) THEN (CASE WHEN ("wt" < 3.4846094) THEN -0.5032456 ELSE -0.47237548 END)
#> ELSE CASE WHEN ("hp" < 177.5) THEN -0.11737026 ELSE -0.39005503 END
#> END)
#> ELSE CASE
#> WHEN ("hp" < 254.5) THEN (CASE WHEN ("wt" < 3.7039063) THEN -0.5958553 ELSE -0.69875544 END)
#> ELSE CASE WHEN ("wt" < 3.379375) THEN -0.4415054 ELSE -0.5238258 END
#> END
#> END)
#> ELSE CASE WHEN ("hp" < 224.5 OR ("hp" IS NULL)) THEN -0.9971653 ELSE -0.55469537 END
#> END
#> END
#> END) + CASE
#> WHEN ("wt" < 2.2596803) THEN (CASE
#> WHEN ("wt" < 1.8848817) THEN (CASE WHEN ("wt" < 1.7250888) THEN 0.74258435 ELSE 0.9946897 END)
#> ELSE CASE
#> WHEN ("wt" < 2.1698422) THEN (CASE WHEN ("wt" < 2.0377436) THEN 0.51929134 ELSE 0.42565268 END)
#> ELSE 0.8866443
#> END
#> END)
#> ELSE CASE
#> WHEN ("cyl" < 7.0) THEN (CASE
#> WHEN ("wt" < 3.326375 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("hp" < 102.5) THEN (CASE
#> WHEN ("wt" < 3.1730468 OR ("wt" IS NULL)) THEN (CASE WHEN ("hp" < 96.5) THEN 0.19515634 ELSE 0.1015171 END)
#> ELSE 0.3104045
#> END)
#> ELSE CASE
#> WHEN ("hp" < 141.5 OR ("hp" IS NULL)) THEN (CASE WHEN ("cyl" < 5.0) THEN 0.11903412 ELSE 0.06686634 END)
#> ELSE -0.028136672
#> END
#> END)
#> ELSE CASE WHEN ("wt" < 3.4495606) THEN -0.11457253 ELSE -0.1433843 END
#> END)
#> ELSE CASE
#> WHEN ("wt" < 4.660125 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("hp" < 211.0 OR ("hp" IS NULL)) THEN (CASE
#> WHEN ("wt" < 3.8098438 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("wt" < 3.7571313 OR ("wt" IS NULL)) THEN -0.20328303
#> ELSE -0.6030386
#> END)
#> ELSE CASE WHEN ("wt" < 3.9561818) THEN -0.007159081 ELSE -0.24303864 END
#> END)
#> ELSE CASE
#> WHEN ("hp" < 254.5) THEN (CASE WHEN ("wt" < 3.7039063) THEN -0.41709858 ELSE -0.48912886 END)
#> ELSE CASE WHEN ("wt" < 3.379375) THEN -0.30905378 ELSE -0.36667785 END
#> END
#> END)
#> ELSE CASE WHEN ("hp" < 224.5 OR ("hp" IS NULL)) THEN -0.69801563 ELSE -0.3882868 END
#> END
#> END
#> END) + CASE
#> WHEN ("wt" < 2.2596803) THEN (CASE
#> WHEN ("wt" < 1.8848817) THEN (CASE WHEN ("wt" < 1.7250888) THEN 0.51980907 ELSE 0.69628274 END)
#> ELSE CASE
#> WHEN ("wt" < 2.1698422) THEN (CASE WHEN ("wt" < 2.0377436) THEN 0.36350405 ELSE 0.29795665 END)
#> ELSE 0.620651
#> END
#> END)
#> ELSE CASE
#> WHEN ("cyl" < 7.0) THEN (CASE
#> WHEN ("wt" < 3.326375 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("hp" < 102.5) THEN (CASE
#> WHEN ("wt" < 3.1730468 OR ("wt" IS NULL)) THEN (CASE WHEN ("hp" < 96.5) THEN 0.1366095 ELSE 0.07106209 END)
#> ELSE 0.21728335
#> END)
#> ELSE CASE
#> WHEN ("hp" < 141.5 OR ("hp" IS NULL)) THEN (CASE
#> WHEN ("wt" < 3.0383594 OR ("wt" IS NULL)) THEN 0.04271759
#> ELSE 0.0955909
#> END)
#> ELSE -0.01969551
#> END
#> END)
#> ELSE CASE WHEN ("wt" < 3.4495606) THEN -0.08020077 ELSE -0.100368805 END
#> END)
#> ELSE CASE
#> WHEN ("wt" < 4.660125 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("hp" < 211.0 OR ("hp" IS NULL)) THEN (CASE
#> WHEN ("hp" < 164.0) THEN (CASE WHEN ("wt" < 3.4846094) THEN -0.2912867 ELSE -0.26967773 END)
#> ELSE CASE WHEN ("wt" < 3.7525) THEN -0.004113865 ELSE -0.19908859 END
#> END)
#> ELSE CASE
#> WHEN ("hp" < 254.5) THEN (CASE WHEN ("wt" < 3.7039063) THEN -0.29196888 ELSE -0.34239012 END)
#> ELSE CASE WHEN ("wt" < 3.379375) THEN -0.21633774 ELSE -0.25667438 END
#> END
#> END)
#> ELSE CASE WHEN ("hp" < 224.5 OR ("hp" IS NULL)) THEN -0.4886111 ELSE -0.27180085 END
#> END
#> END
#> END) + CASE
#> WHEN ("cyl" < 5.0) THEN (CASE
#> WHEN ("wt" < 2.2577705 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("wt" < 1.8853853) THEN (CASE WHEN ("wt" < 1.725376) THEN 0.36386615 ELSE 0.48739767 END)
#> ELSE CASE
#> WHEN ("wt" < 2.169276 OR ("wt" IS NULL)) THEN (CASE WHEN ("wt" < 2.039326) THEN 0.25445274 ELSE 0.20856972 END)
#> ELSE 0.4344556
#> END
#> END)
#> ELSE CASE
#> WHEN ("wt" < 3.1699717) THEN (CASE
#> WHEN ("hp" < 96.5) THEN 0.09562649
#> ELSE CASE WHEN ("wt" < 2.6247656) THEN 0.049743462 ELSE 0.07050888 END
#> END)
#> ELSE 0.15209839
#> END
#> END)
#> ELSE CASE
#> WHEN ("hp" < 193.0 OR ("hp" IS NULL)) THEN (CASE
#> WHEN ("wt" < 3.3264766) THEN (CASE
#> WHEN ("wt" < 3.0449898) THEN (CASE
#> WHEN ("hp" < 143.0 OR ("hp" IS NULL)) THEN 0.009599304
#> ELSE -0.013786927
#> END)
#> ELSE 0.066913724
#> END)
#> ELSE CASE
#> WHEN ("wt" < 3.8144138 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("wt" < 3.7551303 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("wt" < 3.6250772 OR ("wt" IS NULL)) THEN -0.11169243
#> ELSE 0.0891806
#> END)
#> ELSE -0.36240026
#> END)
#> ELSE CASE WHEN ("wt" < 3.9561841) THEN 0.05471512 ELSE -0.11040047 END
#> END
#> END)
#> ELSE CASE
#> WHEN ("hp" < 222.5) THEN -0.34202775
#> ELSE CASE
#> WHEN ("hp" < 253.5) THEN (CASE
#> WHEN ("hp" < 238.0) THEN -0.19026054
#> ELSE CASE WHEN ("wt" < 3.7086718) THEN -0.20437808 ELSE -0.2396731 END
#> END)
#> ELSE CASE WHEN ("wt" < 3.3739061) THEN -0.15143657 ELSE -0.17967196 END
#> END
#> END
#> END
#> END) + CASE
#> WHEN ("cyl" < 5.0) THEN (CASE
#> WHEN ("wt" < 2.2577705 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("wt" < 1.8853853) THEN (CASE WHEN ("wt" < 1.725376) THEN 0.2547061 ELSE 0.3411788 END)
#> ELSE CASE
#> WHEN ("wt" < 2.169276 OR ("wt" IS NULL)) THEN (CASE WHEN ("wt" < 2.039326) THEN 0.17811665 ELSE 0.14599857 END)
#> ELSE 0.30411884
#> END
#> END)
#> ELSE CASE
#> WHEN ("wt" < 3.1699717) THEN (CASE
#> WHEN ("hp" < 96.5) THEN 0.06693844
#> ELSE CASE WHEN ("wt" < 2.6247656) THEN 0.034820367 ELSE 0.049356196 END
#> END)
#> ELSE 0.10646908
#> END
#> END)
#> ELSE CASE
#> WHEN ("cyl" < 7.0) THEN (CASE
#> WHEN ("wt" < 3.3264766 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("wt" < 3.0449898 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("hp" < 143.0 OR ("hp" IS NULL)) THEN 0.0067193983
#> ELSE -0.009651031
#> END)
#> ELSE 0.04683964
#> END)
#> ELSE CASE WHEN ("wt" < 3.4495685) THEN -0.02263298 ELSE -0.036750525 END
#> END)
#> ELSE CASE
#> WHEN ("wt" < 4.662758 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("hp" < 163.5) THEN (CASE WHEN ("wt" < 3.4723437) THEN -0.17039303 ELSE -0.15526657 END)
#> ELSE CASE
#> WHEN ("hp" < 211.5 OR ("hp" IS NULL)) THEN (CASE WHEN ("wt" < 3.7465625) THEN 0.0004972458 ELSE -0.09755318 END)
#> ELSE CASE WHEN ("hp" < 255.5) THEN -0.15541798 ELSE -0.11588805 END
#> END
#> END)
#> ELSE CASE WHEN ("hp" < 224.5 OR ("hp" IS NULL)) THEN -0.23941945 ELSE -0.13318229 END
#> END
#> END
#> END) + CASE
#> WHEN ("cyl" < 5.0) THEN (CASE
#> WHEN ("wt" < 2.2577705 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("wt" < 1.8853853) THEN (CASE WHEN ("wt" < 1.725376) THEN 0.17829448 ELSE 0.23882514 END)
#> ELSE CASE
#> WHEN ("wt" < 2.169276 OR ("wt" IS NULL)) THEN (CASE WHEN ("wt" < 2.039326) THEN 0.1246819 ELSE 0.102199174 END)
#> ELSE 0.2128831
#> END
#> END)
#> ELSE CASE
#> WHEN ("wt" < 3.1699717) THEN (CASE
#> WHEN ("hp" < 96.5) THEN 0.046856917
#> ELSE CASE WHEN ("wt" < 2.6247656) THEN 0.024374198 ELSE 0.034549255 END
#> END)
#> ELSE 0.07452862
#> END
#> END)
#> ELSE CASE
#> WHEN ("cyl" < 7.0) THEN (CASE
#> WHEN ("wt" < 3.3264766 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("wt" < 3.0449898 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("hp" < 143.0 OR ("hp" IS NULL)) THEN 0.0047035217
#> ELSE -0.0067556766
#> END)
#> ELSE 0.03278801
#> END)
#> ELSE CASE WHEN ("wt" < 3.4495685) THEN -0.015843201 ELSE -0.025725288 END
#> END)
#> ELSE CASE
#> WHEN ("wt" < 4.662758 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("wt" < 3.8114195 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("wt" < 3.7512865 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("wt" < 3.651378 OR ("wt" IS NULL)) THEN -0.09137092
#> ELSE 0.062277257
#> END)
#> ELSE -0.22441429
#> END)
#> ELSE CASE
#> WHEN ("hp" < 177.5) THEN 0.067566834
#> ELSE CASE WHEN ("wt" < 3.955) THEN -0.12114578 ELSE -0.04801414 END
#> END
#> END)
#> ELSE CASE WHEN ("hp" < 224.5 OR ("hp" IS NULL)) THEN -0.16759375 ELSE -0.09322752 END
#> END
#> END
#> END) + CASE
#> WHEN ("cyl" < 5.0) THEN (CASE
#> WHEN ("wt" < 2.2577705 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("wt" < 1.8853853) THEN (CASE WHEN ("wt" < 1.725376) THEN 0.12480595 ELSE 0.16717711 END)
#> ELSE CASE
#> WHEN ("wt" < 2.169276 OR ("wt" IS NULL)) THEN (CASE WHEN ("wt" < 2.039326) THEN 0.08727746 ELSE 0.071539305 END)
#> ELSE 0.14901821
#> END
#> END)
#> ELSE CASE
#> WHEN ("wt" < 3.1699717) THEN (CASE
#> WHEN ("hp" < 96.5) THEN 0.032799568
#> ELSE CASE WHEN ("wt" < 2.6247656) THEN 0.017061997 ELSE 0.02418434 END
#> END)
#> ELSE 0.0521703
#> END
#> END)
#> ELSE CASE
#> WHEN ("cyl" < 7.0) THEN (CASE
#> WHEN ("wt" < 3.3264766 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("wt" < 3.0449898 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("hp" < 143.0 OR ("hp" IS NULL)) THEN 0.0032924651
#> ELSE -0.0047289277
#> END)
#> ELSE 0.022951812
#> END)
#> ELSE CASE WHEN ("wt" < 3.4495685) THEN -0.01109047 ELSE -0.018007964 END
#> END)
#> ELSE CASE
#> WHEN ("wt" < 4.662758 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("wt" < 3.8114195 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("wt" < 3.7512865 OR ("wt" IS NULL)) THEN (CASE
#> WHEN ("wt" < 3.651378 OR ("wt" IS NULL)) THEN -0.063959695
#> ELSE 0.043594208
#> END)
#> ELSE -0.15709013
#> END)
#> ELSE CASE
#> WHEN ("hp" < 177.5) THEN 0.04729706
#> ELSE CASE WHEN ("wt" < 3.955) THEN -0.08480192 ELSE -0.03361004 END
#> END
#> END)
#> ELSE CASE WHEN ("hp" < 224.5 OR ("hp" IS NULL)) THEN -0.11731556 ELSE -0.0652593 END
#> END
#> END
#> END
```

## Categorical predictors

H2O handles categorical predictors natively. The generated formula uses
`%in%` for categorical splits.

``` r

mtcars2 <- mtcars
mtcars2$cyl <- factor(mtcars2$cyl)
mtcars2$gear <- factor(mtcars2$gear)

model_cat <- boost_tree(mode = "regression", trees = 10) |>
  set_engine("h2o_gbm") |>
  fit(mpg ~ cyl + gear + wt, data = mtcars2)

tidypredict_fit(model_cat)
#> 20.090625 + (case_when(wt < 2.2596803 ~ case_when(wt < 1.8848817 ~ 
#>     case_when(wt < 1.7250888 ~ 3.0928125, .default = 4.1428123), 
#>     .default = case_when(wt < 2.1698422 ~ case_when(gear %in% 
#>         c("3", "5") ~ 1.7728126, .default = 2.1628125), .default = 3.6928124)), 
#>     .default = case_when(cyl %in% "8" | is.na(cyl) ~ case_when(wt < 
#>         4.6576414 | is.na(wt) ~ case_when(wt < 3.8440876 | is.na(wt) ~ 
#>         case_when(wt < 3.8072233 | is.na(wt) ~ case_when(wt < 
#>             3.478655 ~ -1.0571876, .default = -1.3891875), .default = -2.0371876), 
#>         .default = case_when(wt < 3.957044 ~ -0.2671875, .default = -1.1071875)), 
#>         .default = case_when(wt < 5.2952757 ~ -2.9071875, .default = case_when(wt < 
#>             5.383774 ~ -1.6171875, .default = -2.9071875))), 
#>         .default = case_when(wt < 3.3288743 | is.na(wt) ~ case_when(cyl %in% 
#>             "6" ~ case_when(gear %in% "5" ~ -0.1171875, .default = case_when(gear %in% 
#>             "4" | is.na(gear) ~ 0.27281252, .default = 0.39281252)), 
#>             .default = case_when(wt < 3.1712377 | is.na(wt) ~ 
#>                 case_when(wt < 2.3998036 ~ 0.81281245, .default = 0.54281247), 
#>                 .default = 1.2928125)), .default = case_when(gear %in% 
#>             c("3", "5") ~ -0.5971875, .default = -0.4771875)))) + 
#>     case_when(wt < 2.2596803 ~ case_when(wt < 1.8848817 ~ case_when(wt < 
#>         1.7250888 ~ 2.1649687, .default = 2.8999689), .default = case_when(wt < 
#>         2.1698422 ~ case_when(gear %in% c("3", "5") ~ 1.2409687, 
#>         .default = 1.513969), .default = 2.5849686)), .default = case_when(cyl %in% 
#>         "8" | is.na(cyl) ~ case_when(wt < 4.6576414 | is.na(wt) ~ 
#>         case_when(wt < 3.8440876 | is.na(wt) ~ case_when(wt < 
#>             3.8072233 | is.na(wt) ~ case_when(wt < 3.478655 ~ 
#>             -0.7400313, .default = -0.97243106), .default = -1.4260314), 
#>             .default = case_when(wt < 3.957044 ~ -0.18703103, 
#>                 .default = -0.775031)), .default = case_when(wt < 
#>         5.2952757 ~ -2.035031, .default = case_when(wt < 5.383774 ~ 
#>         -1.1320314, .default = -2.035031))), .default = case_when(wt < 
#>         3.3288743 | is.na(wt) ~ case_when(cyl %in% "6" ~ case_when(gear %in% 
#>         "5" ~ -0.08203148, .default = case_when(gear %in% "4" | 
#>         is.na(gear) ~ 0.1909687, .default = 0.274969)), .default = case_when(wt < 
#>         2.96631 | is.na(wt) ~ case_when(wt < 2.4007888 ~ 0.568969, 
#>         .default = 0.24496856), .default = case_when(wt < 3.169029 ~ 
#>         0.64996856, .default = 0.9049686))), .default = case_when(gear %in% 
#>         c("3", "5") ~ -0.41803104, .default = -0.3340313)))) + 
#>     case_when(wt < 2.2596803 ~ case_when(wt < 1.8848817 ~ case_when(wt < 
#>         1.7250888 ~ 1.5154783, .default = 2.0299783), .default = case_when(wt < 
#>         2.1698422 ~ case_when(gear %in% c("3", "5") ~ 0.8686781, 
#>         .default = 1.0597781), .default = 1.8094782)), .default = case_when(cyl %in% 
#>         "8" | is.na(cyl) ~ case_when(wt < 4.6576414 | is.na(wt) ~ 
#>         case_when(wt < 3.8440876 | is.na(wt) ~ case_when(wt < 
#>             3.8072233 | is.na(wt) ~ case_when(wt < 3.657874 | 
#>             is.na(wt) ~ -0.6783617, .default = -0.44370174), 
#>             .default = -0.9982221), .default = case_when(wt < 
#>             3.957044 ~ -0.13092178, .default = -0.54252195)), 
#>         .default = case_when(wt < 5.2952757 ~ -1.4245218, .default = case_when(wt < 
#>             5.383774 ~ -0.7924223, .default = -1.4245218))), 
#>         .default = case_when(wt < 3.3288743 | is.na(wt) ~ case_when(cyl %in% 
#>             "6" ~ case_when(gear %in% "5" ~ -0.0574221, .default = case_when(gear %in% 
#>             "4" | is.na(gear) ~ 0.13367787, .default = 0.19247828)), 
#>             .default = case_when(wt < 2.96631 | is.na(wt) ~ case_when(wt < 
#>                 2.4007888 ~ 0.39827806, .default = 0.17147803), 
#>                 .default = case_when(wt < 3.169029 ~ 0.4549778, 
#>                   .default = 0.6334781))), .default = case_when(gear %in% 
#>             c("3", "5") ~ -0.29262152, .default = -0.23382169)))) + 
#>     case_when(wt < 2.2596803 ~ case_when(wt < 1.8848817 ~ case_when(wt < 
#>         1.7250888 ~ 1.0608349, .default = 1.420985), .default = case_when(wt < 
#>         2.1698422 ~ case_when(gear %in% c("3", "5") ~ 0.60807496, 
#>         .default = 0.7418446), .default = 1.2666345)), .default = case_when(cyl %in% 
#>         "8" | is.na(cyl) ~ case_when(wt < 4.6576414 | is.na(wt) ~ 
#>         case_when(wt < 3.4373105 ~ case_when(gear %in% c("3", 
#>         "4") ~ -0.72451305, .default = -0.5445131), .default = case_when(wt < 
#>             3.4817965 ~ 0.32548693, .default = case_when(wt < 
#>             3.6472287 ~ -0.63519317, .default = -0.35826975))), 
#>         .default = case_when(wt < 5.2952757 ~ -0.9971653, .default = case_when(wt < 
#>             5.383774 ~ -0.55469537, .default = -0.9971653))), 
#>         .default = case_when(wt < 3.3288743 | is.na(wt) ~ case_when(cyl %in% 
#>             "6" ~ case_when(gear %in% "5" ~ -0.040195312, .default = case_when(gear %in% 
#>             "4" | is.na(gear) ~ 0.09357433, .default = 0.13473484)), 
#>             .default = case_when(wt < 2.96631 | is.na(wt) ~ case_when(wt < 
#>                 2.4007888 ~ 0.2787949, .default = 0.12003456), 
#>                 .default = case_when(wt < 3.169029 ~ 0.31848472, 
#>                   .default = 0.44343463))), .default = case_when(gear %in% 
#>             c("3", "5") ~ -0.20483504, .default = -0.16367513)))) + 
#>     case_when(wt < 2.2596803 ~ case_when(wt < 1.8848817 ~ case_when(wt < 
#>         1.7250888 ~ 0.74258435, .default = 0.9946897), .default = case_when(wt < 
#>         2.1698422 ~ case_when(gear %in% c("3", "5") ~ 0.42565268, 
#>         .default = 0.51929134), .default = 0.8866443)), .default = case_when(cyl %in% 
#>         "8" | is.na(cyl) ~ case_when(wt < 4.6576414 | is.na(wt) ~ 
#>         case_when(wt < 3.8440876 | is.na(wt) ~ case_when(wt < 
#>             3.7545602 | is.na(wt) ~ case_when(wt < 3.6449552 | 
#>             is.na(wt) ~ -0.33239716, .default = 0.11188969), 
#>             .default = case_when(wt < 3.8092952 ~ -0.5181103, 
#>                 .default = -0.5912745)), .default = case_when(wt < 
#>             3.957044 ~ 0.015836105, .default = -0.27228403)), 
#>         .default = case_when(wt < 5.2952757 ~ -0.69801563, .default = case_when(wt < 
#>             5.383774 ~ -0.3882868, .default = -0.69801563))), 
#>         .default = case_when(wt < 3.3288743 | is.na(wt) ~ case_when(cyl %in% 
#>             "6" ~ case_when(gear %in% "5" ~ -0.028136672, .default = case_when(gear %in% 
#>             "4" | is.na(gear) ~ 0.06550197, .default = 0.09431431)), 
#>             .default = case_when(wt < 2.96631 | is.na(wt) ~ case_when(wt < 
#>                 2.4007888 ~ 0.19515634, .default = 0.08402401), 
#>                 .default = case_when(wt < 3.169029 ~ 0.22293915, 
#>                   .default = 0.3104045))), .default = case_when(gear %in% 
#>             c("3", "5") ~ -0.1433843, .default = -0.11457253)))) + 
#>     case_when(wt < 2.2596803 ~ case_when(wt < 1.8848817 ~ case_when(wt < 
#>         1.7250888 ~ 0.51980907, .default = 0.69628274), .default = case_when(wt < 
#>         2.1698422 ~ case_when(gear %in% c("3", "5") ~ 0.29795665, 
#>         .default = 0.36350405), .default = 0.620651)), .default = case_when(wt < 
#>         3.4895623 | is.na(wt) ~ case_when(cyl %in% c("6", "8"
#>     ) | is.na(cyl) ~ case_when(gear %in% "5" ~ case_when(cyl %in% 
#>         "8" ~ -0.28143975, .default = -0.01969551), .default = case_when(wt < 
#>         3.3250926 ~ case_when(gear %in% "4" | is.na(gear) ~ 0.045851327, 
#>         .default = 0.06601994), .default = case_when(wt < 3.4389207 ~ 
#>         -0.40743965, .default = 0.01669742))), .default = case_when(wt < 
#>         2.964173 | is.na(wt) ~ case_when(wt < 2.395489 ~ 0.1366095, 
#>         .default = case_when(gear %in% c("4", "5") ~ 0.043816682, 
#>             .default = 0.07381668)), .default = case_when(wt < 
#>         3.1688287 ~ 0.1560576, .default = 0.21728335))), .default = case_when(wt < 
#>         4.657026 ~ case_when(wt < 3.6488037 ~ case_when(wt < 
#>         3.545157 ~ -0.17491607, .default = case_when(gear %in% 
#>         c("3", "4") ~ -0.5349161, .default = -0.32491606)), .default = case_when(wt < 
#>         3.7541027 ~ 0.07832302, .default = case_when(wt < 3.842949 ~ 
#>         -0.38828474, .default = -0.08975693))), .default = case_when(wt < 
#>         5.295172 ~ -0.4886111, .default = case_when(wt < 5.3837414 ~ 
#>         -0.27180085, .default = -0.4886111))))) + case_when(wt < 
#>     2.2596803 ~ case_when(wt < 1.8848817 ~ case_when(wt < 1.7250888 ~ 
#>     0.36386615, .default = 0.48739767), .default = case_when(wt < 
#>     2.1698422 ~ case_when(gear %in% c("3", "5") ~ 0.20856972, 
#>     .default = 0.25445274), .default = 0.4344556)), .default = case_when(wt < 
#>     3.4493904 | is.na(wt) ~ case_when(cyl %in% c("6", "8") | 
#>     is.na(cyl) ~ case_when(gear %in% "5" ~ case_when(cyl %in% 
#>     "8" ~ -0.19700809, .default = -0.013786927), .default = case_when(wt < 
#>     3.3258634 ~ case_when(gear %in% "4" | is.na(gear) ~ 0.0320961, 
#>     .default = 0.04621422), .default = case_when(wt < 3.4364333 ~ 
#>     -0.2852076, .default = 0.050710417))), .default = case_when(wt < 
#>     2.968517 | is.na(wt) ~ case_when(wt < 2.395998 ~ 0.09562649, 
#>     .default = case_when(gear %in% c("4", "5") ~ 0.030671425, 
#>         .default = 0.05167179)), .default = case_when(wt < 3.169236 ~ 
#>     0.10924038, .default = 0.15209839))), .default = case_when(wt < 
#>     4.6603813 ~ case_when(wt < 3.8444972 ~ case_when(wt < 3.7543807 ~ 
#>     case_when(wt < 3.6485877 ~ -0.20742543, .default = 0.05482601), 
#>     .default = case_when(wt < 3.8092306 ~ -0.24619183, .default = -0.29740682)), 
#>     .default = case_when(wt < 3.9572487 ~ 0.038011894, .default = -0.16367215)), 
#>     .default = case_when(wt < 5.295736 ~ -0.34202775, .default = case_when(wt < 
#>         5.3839173 ~ -0.19026054, .default = -0.34202775))))) + 
#>     case_when(wt < 2.2596803 ~ case_when(wt < 1.8848817 ~ case_when(wt < 
#>         1.7250888 ~ 0.2547061, .default = 0.3411788), .default = case_when(wt < 
#>         2.1698422 ~ case_when(gear %in% c("3", "5") ~ 0.14599857, 
#>         .default = 0.17811665), .default = 0.30411884)), .default = case_when(wt < 
#>         3.545185 | is.na(wt) ~ case_when(cyl %in% c("6", "8") | 
#>         is.na(cyl) ~ case_when(gear %in% c("4", "5") | is.na(gear) ~ 
#>         case_when(wt < 3.0187383 ~ case_when(gear %in% "5" ~ 
#>             -0.009651031, .default = 0.022467041), .default = case_when(cyl %in% 
#>             "8" ~ -0.13790566, .default = -0.10042306)), .default = case_when(wt < 
#>         3.4494674 | is.na(wt) ~ case_when(cyl %in% "6" ~ 0.032349702, 
#>         .default = 0.053846233), .default = case_when(cyl %in% 
#>         "8" ~ -0.060213663, .default = -0.043150064))), .default = case_when(wt < 
#>         2.9660938 | is.na(wt) ~ case_when(wt < 2.395714 ~ 0.06693844, 
#>         .default = case_when(gear %in% c("4", "5") ~ 0.021469802, 
#>             .default = 0.036170196)), .default = case_when(wt < 
#>         3.1690087 ~ 0.07646851, .default = 0.10646908))), .default = case_when(wt < 
#>         3.6516023 ~ case_when(gear %in% c("3", "4") ~ -0.31221378, 
#>         .default = -0.16521378), .default = case_when(wt < 3.7554536 ~ 
#>         0.03837799, .default = case_when(wt < 4.66794 ~ case_when(wt < 
#>         3.8216221 ~ -0.17233424, .default = -0.09871565), .default = case_when(wt < 
#>         5.294052 ~ -0.23941945, .default = -0.18630086)))))) + 
#>     case_when(wt < 2.2596803 ~ case_when(wt < 1.8848817 ~ case_when(wt < 
#>         1.7250888 ~ 0.17829448, .default = 0.23882514), .default = case_when(wt < 
#>         2.1698422 ~ case_when(gear %in% c("3", "5") ~ 0.102199174, 
#>         .default = 0.1246819), .default = 0.2128831)), .default = case_when(wt < 
#>         3.545185 | is.na(wt) ~ case_when(cyl %in% c("6", "8") | 
#>         is.na(cyl) ~ case_when(wt < 3.4398956 | is.na(wt) ~ case_when(cyl %in% 
#>         "8" ~ case_when(gear %in% c("3", "4") ~ -0.21579918, 
#>         .default = -0.09653412), .default = case_when(gear %in% 
#>         "5" ~ -0.0067556766, .default = 0.018032875)), .default = case_when(cyl %in% 
#>         "6" | is.na(cyl) ~ case_when(gear %in% c("4", "5") | 
#>         is.na(gear) ~ -0.07029591, .default = -0.030205078), 
#>         .default = case_when(wt < 3.4799478 ~ 0.2911838, .default = -0.042149447))), 
#>         .default = case_when(wt < 2.9660938 | is.na(wt) ~ case_when(wt < 
#>             2.395714 ~ 0.046856917, .default = case_when(gear %in% 
#>             c("4", "5") ~ 0.015029067, .default = 0.02531891)), 
#>             .default = case_when(wt < 3.1690087 ~ 0.05352768, 
#>                 .default = 0.07452862))), .default = case_when(wt < 
#>         3.6516023 ~ case_when(gear %in% c("3", "4") ~ -0.2185496, 
#>         .default = -0.1156497), .default = case_when(wt < 3.7554536 ~ 
#>         0.02686466, .default = case_when(wt < 5.371858 ~ case_when(wt < 
#>         4.6505475 ~ -0.08198429, .default = -0.122442834), .default = -0.18352908))))) + 
#>     case_when(wt < 2.2596803 ~ case_when(wt < 1.8848817 ~ case_when(wt < 
#>         1.7250888 ~ 0.12480595, .default = 0.16717711), .default = case_when(wt < 
#>         2.1698422 ~ case_when(gear %in% c("3", "5") ~ 0.071539305, 
#>         .default = 0.08727746), .default = 0.14901821)), .default = case_when(wt < 
#>         3.545185 | is.na(wt) ~ case_when(cyl %in% c("6", "8") | 
#>         is.na(cyl) ~ case_when(wt < 3.4398956 | is.na(wt) ~ case_when(cyl %in% 
#>         "8" ~ case_when(gear %in% c("3", "4") ~ -0.15105939, 
#>         .default = -0.067573704), .default = case_when(gear %in% 
#>         "5" ~ -0.0047289277, .default = 0.012623252)), .default = case_when(cyl %in% 
#>         "6" | is.na(cyl) ~ case_when(gear %in% c("4", "5") | 
#>         is.na(gear) ~ -0.049207307, .default = -0.021143645), 
#>         .default = case_when(wt < 3.4799478 ~ 0.20382877, .default = -0.029504586))), 
#>         .default = case_when(wt < 2.9660938 | is.na(wt) ~ case_when(wt < 
#>             2.395714 ~ 0.032799568, .default = case_when(gear %in% 
#>             c("4", "5") ~ 0.010520095, .default = 0.017723465)), 
#>             .default = case_when(wt < 3.1690087 ~ 0.037469327, 
#>                 .default = 0.0521703))), .default = case_when(wt < 
#>         3.6516023 ~ case_when(gear %in% c("3", "4") ~ -0.15298468, 
#>         .default = -0.08095493), .default = case_when(wt < 3.7554536 ~ 
#>         0.01880516, .default = case_when(wt < 5.371858 ~ case_when(wt < 
#>         4.6505475 ~ -0.057389073, .default = -0.08570987), .default = -0.1284704))))))
```

## Classification

For binary classification,
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
returns the probability of the second (positive) outcome level via the
logistic link.

``` r

mtcars3 <- mtcars
mtcars3$vs <- factor(mtcars3$vs)

model_bin <- boost_tree(mode = "classification", trees = 10) |>
  set_engine("h2o_gbm") |>
  fit(vs ~ wt + cyl + hp, data = mtcars3)

tidypredict_fit(model_bin)
#> 1/(1 + exp(-(-0.251314428280906 + (case_when(hp < 136.5 | is.na(hp) ~ 
#>     case_when(wt < 3.013999 | is.na(wt) ~ case_when(cyl < 5 | 
#>         is.na(cyl) ~ case_when(wt < 2.1673985 | is.na(wt) ~ case_when(wt < 
#>         2.0395863 | is.na(wt) ~ 0.6857143, .default = -0.53333336), 
#>         .default = 0.6857143), .default = -0.53333336), .default = 0.6857143), 
#>     .default = -0.53333336) + case_when(hp < 136.5 | is.na(hp) ~ 
#>     case_when(wt < 3.013999 | is.na(wt) ~ case_when(cyl < 5 | 
#>         is.na(cyl) ~ case_when(hp < 78.5 ~ 0.49429598, .default = case_when(hp < 
#>         92 ~ -0.43688414, .default = 0.49429598)), .default = -0.43688414), 
#>         .default = 0.49429598), .default = -0.43688414) + case_when(hp < 
#>     136.5 | is.na(hp) ~ case_when(wt < 3.013999 | is.na(wt) ~ 
#>     case_when(cyl < 5 | is.na(cyl) ~ case_when(wt < 2.1673985 | 
#>         is.na(wt) ~ case_when(wt < 2.0395863 | is.na(wt) ~ 0.4185206, 
#>         .default = -0.3884335), .default = 0.4185206), .default = -0.3884335), 
#>     .default = 0.4185206), .default = -0.3884335) + case_when(hp < 
#>     136.5 | is.na(hp) ~ case_when(wt < 3.013999 | is.na(wt) ~ 
#>     case_when(cyl < 5 | is.na(cyl) ~ case_when(wt < 2.1673985 | 
#>         is.na(wt) ~ case_when(wt < 2.0395863 | is.na(wt) ~ 0.37798885, 
#>         .default = -0.35996836), .default = 0.37798885), .default = -0.35996836), 
#>     .default = 0.37798885), .default = -0.35996836) + case_when(hp < 
#>     136.5 | is.na(hp) ~ case_when(wt < 3.013999 | is.na(wt) ~ 
#>     case_when(cyl < 5 | is.na(cyl) ~ case_when(hp < 78.5 ~ 0.35344094, 
#>         .default = case_when(hp < 92 ~ -0.34183982, .default = 0.35344094)), 
#>         .default = -0.34183982), .default = 0.35344094), .default = -0.34183982) + 
#>     case_when(hp < 136.5 | is.na(hp) ~ case_when(wt < 3.013999 | 
#>         is.na(wt) ~ case_when(cyl < 5 | is.na(cyl) ~ case_when(wt < 
#>         2.1673985 | is.na(wt) ~ case_when(wt < 2.0395863 | is.na(wt) ~ 
#>         0.33752984, .default = -0.3297256), .default = 0.33752984), 
#>         .default = -0.3297256), .default = 0.33752984), .default = -0.3297256) + 
#>     case_when(hp < 136.5 | is.na(hp) ~ case_when(wt < 3.013999 | 
#>         is.na(wt) ~ case_when(cyl < 5 | is.na(cyl) ~ case_when(wt < 
#>         2.1673985 | is.na(wt) ~ case_when(wt < 2.0395863 | is.na(wt) ~ 
#>         0.32677868, .default = -0.32137632), .default = 0.32677868), 
#>         .default = -0.32137632), .default = 0.32677868), .default = -0.32137632) + 
#>     case_when(hp < 136.5 | is.na(hp) ~ case_when(wt < 3.013999 | 
#>         is.na(wt) ~ case_when(cyl < 5 | is.na(cyl) ~ case_when(wt < 
#>         2.1673985 | is.na(wt) ~ case_when(wt < 2.0395863 | is.na(wt) ~ 
#>         0.31931394, .default = -0.31550103), .default = 0.31931394), 
#>         .default = -0.31550103), .default = 0.31931394), .default = -0.31550103) + 
#>     case_when(hp < 136.5 | is.na(hp) ~ case_when(wt < 3.013999 | 
#>         is.na(wt) ~ case_when(cyl < 5 | is.na(cyl) ~ case_when(wt < 
#>         2.1673985 | is.na(wt) ~ case_when(wt < 2.0395863 | is.na(wt) ~ 
#>         0.31403443, .default = -0.31130683), .default = 0.31403443), 
#>         .default = -0.31130683), .default = 0.31403443), .default = -0.31130683) + 
#>     case_when(hp < 136.5 | is.na(hp) ~ case_when(wt < 3.013999 | 
#>         is.na(wt) ~ case_when(cyl < 5 | is.na(cyl) ~ case_when(hp < 
#>         78.5 ~ 0.31025207, .default = case_when(hp < 92 ~ -0.30828214, 
#>         .default = 0.31025207)), .default = -0.30828214), .default = 0.31025207), 
#>         .default = -0.30828214)))))
```

For multiclass classification,
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
returns a named list with one softmax probability formula per class.

``` r

model_multi <- boost_tree(mode = "classification", trees = 10) |>
  set_engine("h2o_gbm") |>
  fit(Species ~ ., data = iris)

names(tidypredict_fit(model_multi))
#> [1] "setosa"     "versicolor" "virginica"
```

## Limitations

Only H2O’s GBM models are supported, not H2O’s XGBoost. The gaussian,
bernoulli, and multinomial distributions are supported. Because
predictions require a live H2O cluster, the parsed formula cannot be
reused after the cluster is shut down.
