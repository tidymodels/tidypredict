# tidypredict

[![R-CMD-check](https://github.com/tidymodels/tidypredict/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidymodels/tidypredict/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/tidypredict)](https://CRAN.R-project.org/package=tidypredict)
[![Downloads](https://cranlogs.r-pkg.org/badges/tidypredict)](https://CRAN.R-project.org/package=tidypredict)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/tidypredict/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/tidypredict)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)

The main goal of `tidypredict` is to enable running predictions inside
databases. It reads the model, extracts the components needed to
calculate the prediction, and then creates an R formula that can be
translated into SQL. In other words, it is able to parse a model such as
this one:

``` r

model <- lm(mpg ~ wt + cyl, data = mtcars)
```

`tidypredict` can return a SQL statement that is ready to run inside the
database. Because it uses `dplyr`’s database interface, it works with
several databases back-ends, such as MS SQL:

``` r

tidypredict_sql(model, dbplyr::simulate_mssql())
```

``` R
## <SQL> (39.686261480253 + ([wt] * -3.19097213898374)) + ([cyl] * -1.5077949682598)
```

## Installation

Install `tidypredict` from CRAN using:

``` r

install.packages("tidypredict")
```

Or install the **development version** using `devtools` as follows:

``` r

install.packages("remotes")
remotes::install_github("tidymodels/tidypredict")
```

## Functions

`tidypredict` has only a few functions, and it is not expected that
number to grow much. The main focus at this time is to add more models
to support.

| Function | Description |
|----|----|
| [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md) | Returns an R formula that calculates the prediction |
| [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md) | Returns a SQL query based on the formula from [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md) |
| [`tidypredict_to_column()`](https://tidypredict.tidymodels.org/reference/tidypredict_to_column.md) | Adds a new column using the formula from [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md) |
| [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md) | Tests `tidypredict` predictions against the model’s native [`predict()`](https://rdrr.io/r/stats/predict.html) function |
| [`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md) | Same as [`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md) but for intervals (only works with `lm` and `glm`) |
| [`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md) | Same as [`tidypredict_sql()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql.md) but for intervals (only works with `lm` and `glm`) |
| [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md) | Creates a list spec based on the R model |
| [`as_parsed_model()`](https://tidypredict.tidymodels.org/reference/as_parsed_model.md) | Prepares an object to be recognized as a parsed model |

## How it works

![](reference/figures/howitworks.png)

Instead of translating directly to a SQL statement, `tidypredict`
creates an R formula. That formula can then be used inside `dplyr`. The
overall workflow would be as illustrated in the image above, and
described here:

1.  Fit the model using a base R model, or one from the packages listed
    in [Supported Models](#supported-models)
2.  `tidypredict` reads model, and creates a list object with the
    necessary components to run predictions
3.  `tidypredict` builds an R formula based on the list object
4.  `dplyr` evaluates the formula created by `tidypredict`
5.  `dplyr` translates the formula into a SQL statement, or any other
    interfaces.
6.  The database executes the SQL statement(s) created by `dplyr`

### Parsed model spec

`tidypredict` writes and reads a spec based on a model. Instead of
simply writing the R formula directly, splitting the spec from the
formula adds the following capabilities:

1.  No more saving models as `.rds` - Specifically for cases when the
    model needs to be used for predictions in a Shiny app.
2.  Beyond R models - Technically, anything that can write a proper
    spec, can be read into `tidypredict`. It also means, that the parsed
    model spec can become a good alternative to using *PMML.*

## Supported models

`tidypredict` parses 43 fitted model classes from 30 modeling packages.
[Supported
models](https://tidypredict.tidymodels.org/articles/models.html) has the
full list, with the `parsnip` spec and engine for each and a link to a
worked example. In brief:

- Regression: [`lm()`](https://rdrr.io/r/stats/lm.html),
  [`glm()`](https://rdrr.io/r/stats/glm.html),
  [`glmnet::glmnet()`](https://rdrr.io/pkg/glmnet/man/glmnet.html),
  [`LiblineaR::LiblineaR()`](https://rdrr.io/pkg/LiblineaR/man/LiblineaR.html),
  [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html),
  [`nnet::multinom()`](https://rdrr.io/pkg/nnet/man/multinom.html),
  [`kernlab::ksvm()`](https://rdrr.io/pkg/kernlab/man/ksvm.html),
  [`nnet::nnet()`](https://rdrr.io/pkg/nnet/man/nnet.html),
  [`earth::earth()`](https://rdrr.io/pkg/earth/man/earth.html),
  `mixOmics` PLS,
  [`parsnip::nullmodel()`](https://parsnip.tidymodels.org/reference/nullmodel.html)
- Classification and discriminant analysis:
  [`naivebayes::naive_bayes()`](https://majkamichal.github.io/naivebayes/reference/naive_bayes.html),
  [`klaR::NaiveBayes()`](https://rdrr.io/pkg/klaR/man/NaiveBayes.html),
  [`MASS::lda()`](https://rdrr.io/pkg/MASS/man/lda.html),
  [`MASS::qda()`](https://rdrr.io/pkg/MASS/man/qda.html),
  [`mda::fda()`](https://rdrr.io/pkg/mda/man/fda.html),
  [`sda::sda()`](https://rdrr.io/pkg/sda/man/sda.html), `sparsediscrim`
- Trees and forests:
  [`rpart::rpart()`](https://rdrr.io/pkg/rpart/man/rpart.html),
  [`C50::C5.0()`](https://topepo.github.io/C5.0/reference/C5.0.html),
  [`partykit::ctree()`](https://rdrr.io/pkg/partykit/man/ctree.html) and
  `cforest()`,
  [`randomForest::randomForest()`](https://rdrr.io/pkg/randomForest/man/randomForest.html),
  [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md),
  [`aorsf::orsf()`](https://docs.ropensci.org/aorsf/reference/orsf.html),
  [`baguette::bagger()`](https://baguette.tidymodels.org/reference/bagger.html),
  [`dbarts::bart()`](https://rdrr.io/pkg/dbarts/man/bart.html)
- Boosting and rules: `xgboost`, `lightgbm`, `catboost`,
  [`mboost::blackboost()`](https://rdrr.io/pkg/mboost/man/blackboost.html),
  [`Cubist::cubist()`](http://topepo.github.io/Cubist/reference/cubist.default.md),
  [`xrf::xrf()`](https://rdrr.io/pkg/xrf/man/xrf.html), H2O GBM and
  RuleFit

`tidypredict` dispatches on the class of the fitted model, so models
fitted through `parsnip` work for any engine whose underlying model
appears above: pass the `parsnip` fit object to
[`tidypredict_fit()`](https://tidypredict.tidymodels.org/reference/tidypredict_fit.md)
just as you would the engine’s own fit.

[`tidypredict_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_interval.md)
and
[`tidypredict_sql_interval()`](https://tidypredict.tidymodels.org/reference/tidypredict_sql_interval.md)
are narrower, and only support [`lm()`](https://rdrr.io/r/stats/lm.html)
and [`glm()`](https://rdrr.io/r/stats/glm.html) models.

### `broom`

The [`tidy()`](https://generics.r-lib.org/reference/tidy.html) function
from broom works with linear models parsed via `tidypredict`

``` r

pm <- parse_model(lm(wt ~ ., mtcars))
tidy(pm)
```

``` R
## # A tibble: 11 × 2
##    term        estimate
##    <chr>          <dbl>
##  1 (Intercept) -0.231  
##  2 mpg         -0.0417 
##  3 cyl         -0.0573 
##  4 disp         0.00669
##  5 hp          -0.00323
##  6 drat        -0.0901 
##  7 qsec         0.200  
##  8 vs          -0.0664 
##  9 am           0.0184 
## 10 gear        -0.0935 
## 11 carb         0.249
```

## Contributing

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

- For questions and discussions about tidymodels packages, modeling, and
  machine learning, please [post on Posit
  Community](https://forum.posit.co/new-topic?category_id=15&tags=tidymodels,question).

- If you think you have encountered a bug, please [submit an
  issue](https://github.com/tidymodels/tidypredict/issues).

- Either way, learn how to create and share a
  [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
  (a minimal, reproducible example), to clearly communicate about your
  code.

- Check out further details on [contributing guidelines for tidymodels
  packages](https://www.tidymodels.org/contribute/) and [how to get
  help](https://www.tidymodels.org/help/).
