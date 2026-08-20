# bart models

| Function | Works |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md), [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md), [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | ✔ |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | ✔ |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | ✔ |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md), [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | ✗ |
| `parsnip` | ✔ |

[`dbarts::bart()`](https://rdrr.io/pkg/dbarts/man/bart.html) fits a
Bayesian additive regression trees (BART) model. Each draw of the
posterior sampler holds a full ensemble of trees, and the fitted value
is the average, over every draw, of the summed leaf values of that
draw’s trees.
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
returns one nested
[`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
per tree per draw, so the size of the returned expression grows with
both the number of trees and the number of posterior samples that were
kept. Small values of `ntree` and `ndpost` keep the formula manageable.

Two things are needed for the model to be parsed:

- The model has to be fit with `keeptrees = TRUE`, otherwise the trees
  are discarded and cannot be recovered.
- The outcome has to be continuous. Binary outcomes are fit with a
  probit link, which cannot be translated to SQL.

## `tidypredict_` functions

``` r

set.seed(100)
model <- dbarts::bart(
  mtcars[c("wt", "cyl", "disp")],
  mtcars$mpg,
  ntree = 5,
  ndpost = 5,
  keeptrees = TRUE,
  verbose = FALSE
)
```

- Create the R formula

  ``` r

  tidypredict_fit(model)
  #> (case_when(disp <= 468.030693069307 ~ 0.0388237369617747, .default = -0.23775684832047) + 
  #>     case_when(cyl <= 6.73267326732673 ~ case_when(cyl <= 5.94059405940594 ~ 
  #>         0.0566109358696729, .default = -0.0104096098780389), 
  #>         .default = -0.151028877096314) + case_when(wt <= 1.90022772277228 ~ 
  #>     0.063821849213624, .default = -0.017588928858414) + case_when(wt <= 
  #>     1.66789108910891 ~ 0.0220891384775673, .default = case_when(wt <= 
  #>     2.28745544554455 ~ 0.102347409914292, .default = -0.240170731594466)) + 
  #>     case_when(disp <= 206.056435643564 ~ 0.170519142067219, .default = case_when(wt <= 
  #>         5.26910891089109 ~ 0.109851140353609, .default = 0.0703308840296107)) + 
  #>     case_when(wt <= 4.26231683168317 ~ 0.0209084766145231, .default = -0.127255636100085) + 
  #>     case_when(cyl <= 6.73267326732673 ~ case_when(cyl <= 5.94059405940594 ~ 
  #>         0.087322333226358, .default = 0.0231118535912801), .default = -0.134454972160386) + 
  #>     case_when(wt <= 1.90022772277228 ~ 0.0168629074757081, .default = -0.034472043686233) + 
  #>     case_when(wt <= 1.66789108910891 ~ 0.100724361869511, .default = case_when(wt <= 
  #>         2.28745544554455 ~ 0.0288614155532398, .default = -0.271551184814163)) + 
  #>     case_when(disp <= 206.056435643564 ~ 0.154066146195713, .default = 0.143388261109583) + 
  #>     case_when(wt <= 4.26231683168317 ~ 0.0312269499895216, .default = -0.1020496658973) + 
  #>     case_when(cyl <= 6.73267326732673 ~ case_when(cyl <= 5.94059405940594 ~ 
  #>         0.131214189976264, .default = 0.0269003998760029), .default = -0.169167717280167) + 
  #>     case_when(wt <= 1.90022772277228 ~ -0.0384834759479691, .default = -0.0109836500983765) + 
  #>     case_when(wt <= 1.66789108910891 ~ 0.134820173276589, .default = case_when(wt <= 
  #>         2.28745544554455 ~ -0.0037701517450396, .default = -0.264801417154789)) + 
  #>     case_when(disp <= 206.056435643564 ~ 0.156417402284849, .default = 0.0983617263389225) + 
  #>     case_when(wt <= 4.26231683168317 ~ 0.0454050194494695, .default = -0.0576085063856489) + 
  #>     case_when(cyl <= 6.73267326732673 ~ case_when(cyl <= 5.94059405940594 ~ 
  #>         0.0838485525920718, .default = -0.0251188327464236), 
  #>         .default = -0.130066986185129) + case_when(wt <= 1.90022772277228 ~ 
  #>     0.0857154093842334, .default = 0.0160148949007423) + case_when(wt <= 
  #>     1.66789108910891 ~ 0.0299985697865336, .default = case_when(wt <= 
  #>     2.28745544554455 ~ 0.052570578033697, .default = -0.306118475801609)) + 
  #>     case_when(disp <= 206.056435643564 ~ 0.15333254364241, .default = 0.109243084268014) + 
  #>     case_when(wt <= 4.26231683168317 ~ 0.0696646411370065, .default = -0.133900392130145) + 
  #>     case_when(cyl <= 6.73267326732673 ~ case_when(cyl <= 5.94059405940594 ~ 
  #>         0.0651369620580563, .default = -0.00298440166446769), 
  #>         .default = -0.149791417046493) + case_when(wt <= 1.90022772277228 ~ 
  #>     0.0800207591246698, .default = -0.0110037437390987) + case_when(wt <= 
  #>     1.66789108910891 ~ 0.00340975857993721, .default = case_when(wt <= 
  #>     2.28745544554455 ~ -0.0170822487624541, .default = -0.290317244542404)) + 
  #>     case_when(disp <= 206.056435643564 ~ 0.152868952126194, .default = 0.152581935068088))/5 * 
  #>     23.5 + 22.15
  ```

- Add the predictions to the original table

  ``` r

  mtcars %>%
    tidypredict_to_column(model) %>%
    glimpse()
  #> Rows: 32
  #> Columns: 12
  #> $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7, 18.1, 14.3, 24.4, 22.8, 19…
  #> $ cyl  <dbl> 6, 6, 4, 6, 8, 6, 8, 4, 4, 6, 6, 8, 8, 8, 8, 8, 8, 4, 4,…
  #> $ disp <dbl> 160.0, 160.0, 108.0, 258.0, 360.0, 225.0, 360.0, 146.7, …
  #> $ hp   <dbl> 110, 110, 93, 110, 175, 105, 245, 62, 95, 123, 123, 180,…
  #> $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15, 2.76, 3.21, 3.69, 3.92, 3.…
  #> $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440, 3.460, 3.570, 3.190, …
  #> $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02, 20.22, 15.84, 20.00, …
  #> $ vs   <dbl> 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1,…
  #> $ am   <dbl> 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1,…
  #> $ gear <dbl> 4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4,…
  #> $ carb <dbl> 4, 4, 1, 1, 2, 1, 4, 2, 2, 4, 4, 3, 3, 3, 4, 4, 4, 1, 2,…
  #> $ fit  <dbl> 20.14658, 20.14658, 22.08596, 19.32982, 15.82358, 19.329…
  ```

- Confirm that the results match the model’s
  [`predict()`](https://rdrr.io/r/stats/predict.html) results

  ``` r

  tidypredict_test(model, mtcars)
  #> tidypredict test results
  #> Difference threshold: 1e-12
  #> 
  #>  All results are within the difference threshold
  ```

- Get the SQL translation

  ``` r

  tidypredict_sql(model, dbplyr::simulate_mssql())
  #> <SQL> ((((((((((((((((((((((((((CASE
  #> WHEN ([disp] <= 468.030693069307) THEN 0.0388237369617747
  #> ELSE -0.23775684832047
  #> END + CASE
  #> WHEN ([cyl] <= 6.73267326732673) THEN (CASE
  #> WHEN ([cyl] <= 5.94059405940594) THEN 0.0566109358696729
  #> ELSE -0.0104096098780389
  #> END)
  #> ELSE -0.151028877096314
  #> END) + CASE
  #> WHEN ([wt] <= 1.90022772277228) THEN 0.063821849213624
  #> ELSE -0.017588928858414
  #> END) + CASE
  #> WHEN ([wt] <= 1.66789108910891) THEN 0.0220891384775673
  #> ELSE CASE
  #> WHEN ([wt] <= 2.28745544554455) THEN 0.102347409914292
  #> ELSE -0.240170731594466
  #> END
  #> END) + CASE
  #> WHEN ([disp] <= 206.056435643564) THEN 0.170519142067219
  #> ELSE CASE
  #> WHEN ([wt] <= 5.26910891089109) THEN 0.109851140353609
  #> ELSE 0.0703308840296107
  #> END
  #> END) + CASE
  #> WHEN ([wt] <= 4.26231683168317) THEN 0.0209084766145231
  #> ELSE -0.127255636100085
  #> END) + CASE
  #> WHEN ([cyl] <= 6.73267326732673) THEN (CASE
  #> WHEN ([cyl] <= 5.94059405940594) THEN 0.087322333226358
  #> ELSE 0.0231118535912801
  #> END)
  #> ELSE -0.134454972160386
  #> END) + CASE
  #> WHEN ([wt] <= 1.90022772277228) THEN 0.0168629074757081
  #> ELSE -0.034472043686233
  #> END) + CASE
  #> WHEN ([wt] <= 1.66789108910891) THEN 0.100724361869511
  #> ELSE CASE
  #> WHEN ([wt] <= 2.28745544554455) THEN 0.0288614155532398
  #> ELSE -0.271551184814163
  #> END
  #> END) + CASE
  #> WHEN ([disp] <= 206.056435643564) THEN 0.154066146195713
  #> ELSE 0.143388261109583
  #> END) + CASE
  #> WHEN ([wt] <= 4.26231683168317) THEN 0.0312269499895216
  #> ELSE -0.1020496658973
  #> END) + CASE
  #> WHEN ([cyl] <= 6.73267326732673) THEN (CASE
  #> WHEN ([cyl] <= 5.94059405940594) THEN 0.131214189976264
  #> ELSE 0.0269003998760029
  #> END)
  #> ELSE -0.169167717280167
  #> END) + CASE
  #> WHEN ([wt] <= 1.90022772277228) THEN -0.0384834759479691
  #> ELSE -0.0109836500983765
  #> END) + CASE
  #> WHEN ([wt] <= 1.66789108910891) THEN 0.134820173276589
  #> ELSE CASE
  #> WHEN ([wt] <= 2.28745544554455) THEN -0.0037701517450396
  #> ELSE -0.264801417154789
  #> END
  #> END) + CASE
  #> WHEN ([disp] <= 206.056435643564) THEN 0.156417402284849
  #> ELSE 0.0983617263389225
  #> END) + CASE
  #> WHEN ([wt] <= 4.26231683168317) THEN 0.0454050194494695
  #> ELSE -0.0576085063856489
  #> END) + CASE
  #> WHEN ([cyl] <= 6.73267326732673) THEN (CASE
  #> WHEN ([cyl] <= 5.94059405940594) THEN 0.0838485525920718
  #> ELSE -0.0251188327464236
  #> END)
  #> ELSE -0.130066986185129
  #> END) + CASE
  #> WHEN ([wt] <= 1.90022772277228) THEN 0.0857154093842334
  #> ELSE 0.0160148949007423
  #> END) + CASE
  #> WHEN ([wt] <= 1.66789108910891) THEN 0.0299985697865336
  #> ELSE CASE
  #> WHEN ([wt] <= 2.28745544554455) THEN 0.052570578033697
  #> ELSE -0.306118475801609
  #> END
  #> END) + CASE
  #> WHEN ([disp] <= 206.056435643564) THEN 0.15333254364241
  #> ELSE 0.109243084268014
  #> END) + CASE
  #> WHEN ([wt] <= 4.26231683168317) THEN 0.0696646411370065
  #> ELSE -0.133900392130145
  #> END) + CASE
  #> WHEN ([cyl] <= 6.73267326732673) THEN (CASE
  #> WHEN ([cyl] <= 5.94059405940594) THEN 0.0651369620580563
  #> ELSE -0.00298440166446769
  #> END)
  #> ELSE -0.149791417046493
  #> END) + CASE
  #> WHEN ([wt] <= 1.90022772277228) THEN 0.0800207591246698
  #> ELSE -0.0110037437390987
  #> END) + CASE
  #> WHEN ([wt] <= 1.66789108910891) THEN 0.00340975857993721
  #> ELSE CASE
  #> WHEN ([wt] <= 2.28745544554455) THEN -0.0170822487624541
  #> ELSE -0.290317244542404
  #> END
  #> END) + CASE
  #> WHEN ([disp] <= 206.056435643564) THEN 0.152868952126194
  #> ELSE 0.152581935068088
  #> END) / 5.0) * 23.5) + 22.15
  ```

## parsnip

Models fit with
[`parsnip::bart()`](https://parsnip.tidymodels.org/reference/bart.html)
and the `"dbarts"` engine are supported as well. Note that
[`predict()`](https://rdrr.io/r/stats/predict.html) on a parsnip
[`bart()`](https://parsnip.tidymodels.org/reference/bart.html) model
draws from the posterior *predictive* distribution, which adds residual
noise, so its results are only equal to the ones of
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
in expectation.

``` r

library(parsnip)

set.seed(100)
model <- bart(mode = "regression", trees = 5) %>%
  set_engine("dbarts", ndpost = 5, verbose = FALSE) %>%
  fit(mpg ~ wt + cyl + disp, data = mtcars)

tidypredict_fit(model)
#> (case_when(disp <= 468.030693069307 ~ 0.0388237369617747, .default = -0.23775684832047) + 
#>     case_when(cyl <= 6.73267326732673 ~ case_when(cyl <= 5.94059405940594 ~ 
#>         0.0566109358696729, .default = -0.0104096098780389), 
#>         .default = -0.151028877096314) + case_when(wt <= 1.90022772277228 ~ 
#>     0.063821849213624, .default = -0.017588928858414) + case_when(wt <= 
#>     1.66789108910891 ~ 0.0220891384775673, .default = case_when(wt <= 
#>     2.28745544554455 ~ 0.102347409914292, .default = -0.240170731594466)) + 
#>     case_when(disp <= 206.056435643564 ~ 0.170519142067219, .default = case_when(wt <= 
#>         5.26910891089109 ~ 0.109851140353609, .default = 0.0703308840296107)) + 
#>     case_when(wt <= 4.26231683168317 ~ 0.0209084766145231, .default = -0.127255636100085) + 
#>     case_when(cyl <= 6.73267326732673 ~ case_when(cyl <= 5.94059405940594 ~ 
#>         0.087322333226358, .default = 0.0231118535912801), .default = -0.134454972160386) + 
#>     case_when(wt <= 1.90022772277228 ~ 0.0168629074757081, .default = -0.034472043686233) + 
#>     case_when(wt <= 1.66789108910891 ~ 0.100724361869511, .default = case_when(wt <= 
#>         2.28745544554455 ~ 0.0288614155532398, .default = -0.271551184814163)) + 
#>     case_when(disp <= 206.056435643564 ~ 0.154066146195713, .default = 0.143388261109583) + 
#>     case_when(wt <= 4.26231683168317 ~ 0.0312269499895216, .default = -0.1020496658973) + 
#>     case_when(cyl <= 6.73267326732673 ~ case_when(cyl <= 5.94059405940594 ~ 
#>         0.131214189976264, .default = 0.0269003998760029), .default = -0.169167717280167) + 
#>     case_when(wt <= 1.90022772277228 ~ -0.0384834759479691, .default = -0.0109836500983765) + 
#>     case_when(wt <= 1.66789108910891 ~ 0.134820173276589, .default = case_when(wt <= 
#>         2.28745544554455 ~ -0.0037701517450396, .default = -0.264801417154789)) + 
#>     case_when(disp <= 206.056435643564 ~ 0.156417402284849, .default = 0.0983617263389225) + 
#>     case_when(wt <= 4.26231683168317 ~ 0.0454050194494695, .default = -0.0576085063856489) + 
#>     case_when(cyl <= 6.73267326732673 ~ case_when(cyl <= 5.94059405940594 ~ 
#>         0.0838485525920718, .default = -0.0251188327464236), 
#>         .default = -0.130066986185129) + case_when(wt <= 1.90022772277228 ~ 
#>     0.0857154093842334, .default = 0.0160148949007423) + case_when(wt <= 
#>     1.66789108910891 ~ 0.0299985697865336, .default = case_when(wt <= 
#>     2.28745544554455 ~ 0.052570578033697, .default = -0.306118475801609)) + 
#>     case_when(disp <= 206.056435643564 ~ 0.15333254364241, .default = 0.109243084268014) + 
#>     case_when(wt <= 4.26231683168317 ~ 0.0696646411370065, .default = -0.133900392130145) + 
#>     case_when(cyl <= 6.73267326732673 ~ case_when(cyl <= 5.94059405940594 ~ 
#>         0.0651369620580563, .default = -0.00298440166446769), 
#>         .default = -0.149791417046493) + case_when(wt <= 1.90022772277228 ~ 
#>     0.0800207591246698, .default = -0.0110037437390987) + case_when(wt <= 
#>     1.66789108910891 ~ 0.00340975857993721, .default = case_when(wt <= 
#>     2.28745544554455 ~ -0.0170822487624541, .default = -0.290317244542404)) + 
#>     case_when(disp <= 206.056435643564 ~ 0.152868952126194, .default = 0.152581935068088))/5 * 
#>     23.5 + 22.15
```

## Factor predictors

Factor and character predictors are expanded into indicator columns
before the model is fit.
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
maps those columns back onto the original columns, so the returned
formula can be used with the same data that the model was fit on.

``` r

set.seed(100)
model <- dbarts::bart(
  data.frame(wt = mtcars$wt, cyl = factor(mtcars$cyl)),
  mtcars$mpg,
  ntree = 2,
  ndpost = 2,
  keeptrees = TRUE,
  verbose = FALSE
)

tidypredict_fit(model)
#> (case_when(cyl != "8" ~ 0.0209929673364753, .default = -0.23137072210149) + 
#>     case_when(wt <= 4.06870297029703 ~ case_when(wt <= 2.24873267326733 ~ 
#>         0.2729255967499, .default = -0.0575567928789224), .default = -0.155546894767129) + 
#>     case_when(cyl != "8" ~ 0.0239618128499716, .default = -0.207627487534535) + 
#>     case_when(wt <= 4.06870297029703 ~ case_when(wt <= 2.28745544554455 ~ 
#>         0.311929961318821, .default = -0.0549142789129821), .default = -0.169591479476645))/2 * 
#>     23.5 + 22.15
```
