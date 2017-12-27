tidypredict
================

[![Build
Status](https://travis-ci.org/edgararuiz/tidypredict.svg?branch=master)](https://travis-ci.org/edgararuiz/tidypredict)

## Intro

The mail goal of **tidypredict** is to use [Tidy
Evaluation](http://rlang.tidyverse.org/articles/tidy-evaluation.html) to
produce a formula that can be executed inside `dplyr` verbs that
calculates the prediction based on a model. In other words, it takes
place of the `predict()` function.

The main motivation for `tidypredict` is to open the possibility to
score the new results inside a database or Spark. The premise is that
even though the model may be created in-memory inside R, there is still
the need to use the results to score at scale.

## Highlights

  - `predict_to_column()` - Helper function, similar to
    `tibble::rowid_to_column()` that makes it easier to add the fitted
    and interval calculations to an analysis.

  - `predict_fit()` /`predict_interval()` - Creates a *tidy eval*
    formula that `dplyr` can run to calculate the predictions. (Used by
    `predict_to_column()`)

  - `parsemodel()` - Reads an R model (`lm` and `glm` only at this time)
    and outputs a tidy `tibble` with the needed information to calculate
    the predictions.

  - `tidypredict` includes a helper function to compare the results of
    the base `predict()` function against the results from the
    `tidypredict` functions, it is called: `test_predictions()`

  - *The parser and the formula creation are separated* - This allows
    for non-R model objects to use the same `predict_...` functions,
    just as long as they provide the same data as the output from
    `parsemodel()` does.

## Advantages

  - Using *tidy eval* allows the resulting formula to be translated to
    SQL-syntax, which allows the predictions to run inside the database

  - The output from `parsemodel()` can be saved to a file, and re-loaded
    from, a *csv* file easily. This means that it is a good alternative
    to saving the, usually large, model variable as an `.rds` file for
    later use, such as in a Shiny app.

  - Because of the separation of the predict functions from the parsing
    functions, models created using different languages (as in not R)
    could still be run at-scale as long as a `data.frame` is produced
    and passed to the prediction functions. Some possibilities are PMML,
    SAS, and other types.

## Installation

Install `tidypredict` using `devtools` as follows:

``` r
devtools::install_github("edgararuiz/tidypredict")
```
