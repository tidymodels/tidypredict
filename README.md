tidypredict
===========

-   [Intro](#intro)
-   [Highlights](#highlights)
-   [Advantages](#advantages)
-   [Example](#example)
    -   [`prediction_to_column()`](#prediction_to_column)
    -   [Prediction functions](#prediction-functions)
    -   [Model parser](#model-parser)
    -   [Save, and reload, a parsed
        model](#save-and-reload-a-parsed-model)

Intro
-----

The mail goal of **tidypredict** is to use [Tidy
Evaluation](http://rlang.tidyverse.org/articles/tidy-evaluation.html) to
produce a formula that can be executed inside `dplyr` verbs that
calculates the prediction based on a model. In other words, it takes
place of the `predict()` function.

The main motivation for `tidypredict` is to open the possibility to
score the new results inside a database or Spark. The premise is that
even though the model may be created in-memory inside R, there is still
the need to use the results to score at scale.

Highlights
----------

-   `predict_to_column()` - Helper function, similar to
    `tibble::rowid_to_column()` that makes it easier to add the fitted
    and interval calculations to an analysis.

-   `predict_fit()` /`predict_interval()` - Creates a *tidy eval*
    formula that `dplyr` can run to calculate the predictions. (Used by
    `predict_to_column()`)

-   `parsemodel()` - Reads an R model (`lm` and `glm` only at this time)
    and outputs a tidy `tibble` with the needed information to calculate
    the predictions.

-   *The parser and the formula creation are separated* - This allows
    for non-R model objects to use the same `predict_...` functions,
    just as long as they provide the same data as the output from
    `parsemodel()` does.

Advantages
----------

-   Using *tidy eval* allows the resulting formula to be translated to
    SQL-syntax, which allows the predictions to run inside the database

-   The output from `parsemodel()` can be saved to a file, and re-loaded
    from, a *csv* file easily. This means that it is a good alternative
    to saving the, usually large, model variable as an `.rds` file for
    later use, such as in a Shiny app.

-   Because of the separation of the predict functions from the parsing
    functions, models created using different languages (as in not R)
    could still be run at-scale as long as a `data.frame` is produced
    and passed to the prediction functions. Some possibilities are PMML,
    SAS, and other types.

Example
-------

    library(dplyr)
    library(tidypredict)

    df <- mtcars %>%
      mutate(cyl = paste0("cyl", cyl))

    model <- lm(mpg ~ wt + am + cyl, data = df)

    model

    ## 
    ## Call:
    ## lm(formula = mpg ~ wt + am + cyl, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)           wt           am      cylcyl6      cylcyl8  
    ##     33.7536      -3.1496       0.1501      -4.2573      -6.0791

### `prediction_to_column()`

    df %>%
      head(10) %>%
      predict_to_column(model)

    ##     mpg  cyl  disp  hp drat    wt  qsec vs am gear carb      fit
    ## 1  21.0 cyl6 160.0 110 3.90 2.620 16.46  0  1    4    4 21.39443
    ## 2  21.0 cyl6 160.0 110 3.90 2.875 17.02  0  1    4    4 20.59128
    ## 3  22.8 cyl4 108.0  93 3.85 2.320 18.61  1  1    4    1 26.59663
    ## 4  21.4 cyl6 258.0 110 3.08 3.215 19.44  1  0    3    1 19.37032
    ## 5  18.7 cyl8 360.0 175 3.15 3.440 17.02  0  0    3    2 16.83986
    ## 6  18.1 cyl6 225.0 105 2.76 3.460 20.22  1  0    3    1 18.59867
    ## 7  14.3 cyl8 360.0 245 3.21 3.570 15.84  0  0    3    4 16.43041
    ## 8  24.4 cyl4 146.7  62 3.69 3.190 20.00  1  0    4    2 23.70638
    ## 9  22.8 cyl4 140.8  95 3.92 3.150 22.90  1  0    4    2 23.83236
    ## 10 19.2 cyl6 167.6 123 3.92 3.440 18.30  1  0    4    4 18.66166

Compare the results against `predict()`

    predict(model, head(df,10))

    ##        1        2        3        4        5        6        7        8 
    ## 21.39443 20.59128 26.59663 19.37032 16.83986 18.59867 16.43041 23.70638 
    ##        9       10 
    ## 23.83236 18.66166

To include the prediction intervals just use the `add_interval` switch

    df %>%
      head(10) %>%
      predict_to_column(model, add_interval = TRUE) %>%
      select(fit, lower, upper)

    ##         fit    lower    upper
    ## 1  21.39443 15.53968 27.24918
    ## 2  20.59128 14.72632 26.45624
    ## 3  26.59663 20.96580 32.22746
    ## 4  19.37032 13.56317 25.17746
    ## 5  16.83986 11.16336 22.51635
    ## 6  18.59867 12.80729 24.39004
    ## 7  16.43041 10.80210 22.05872
    ## 8  23.70638 17.85550 29.55725
    ## 9  23.83236 17.98927 29.67545
    ## 10 18.66166 12.87034 24.45297

Compare the results against `predict()`

    predict(model, head(df,10), interval = "prediction")

    ##         fit      lwr      upr
    ## 1  21.39443 15.53968 27.24918
    ## 2  20.59128 14.72632 26.45624
    ## 3  26.59663 20.96580 32.22746
    ## 4  19.37032 13.56317 25.17746
    ## 5  16.83986 11.16336 22.51635
    ## 6  18.59867 12.80729 24.39004
    ## 7  16.43041 10.80210 22.05872
    ## 8  23.70638 17.85550 29.55725
    ## 9  23.83236 17.98927 29.67545
    ## 10 18.66166 12.87034 24.45297

### Prediction functions

To see the prediction formula, call the `predict_fit()` function.
Because it uses S3 methods, it will decide which parser and formula
generator to use for the model passed in the argument.

    predict_fit(model)

    ## ((((ifelse((cyl) == ("cyl6"), (-4.2573185440316), 0)) + (ifelse((cyl) == 
    ##     ("cyl8"), (-6.07911886695386), 0))) + ((wt) * (-3.14959778114425))) + 
    ##     ((am) * (0.150103119934497))) + (33.7535920047122)

If more granular control than what `predict_to_column()` is needed, then
the function can be used inside a `dplyr` verb command

    df %>%
      head(10) %>%
      mutate(fit = !! predict_fit(model))

    ##     mpg  cyl  disp  hp drat    wt  qsec vs am gear carb      fit
    ## 1  21.0 cyl6 160.0 110 3.90 2.620 16.46  0  1    4    4 21.39443
    ## 2  21.0 cyl6 160.0 110 3.90 2.875 17.02  0  1    4    4 20.59128
    ## 3  22.8 cyl4 108.0  93 3.85 2.320 18.61  1  1    4    1 26.59663
    ## 4  21.4 cyl6 258.0 110 3.08 3.215 19.44  1  0    3    1 19.37032
    ## 5  18.7 cyl8 360.0 175 3.15 3.440 17.02  0  0    3    2 16.83986
    ## 6  18.1 cyl6 225.0 105 2.76 3.460 20.22  1  0    3    1 18.59867
    ## 7  14.3 cyl8 360.0 245 3.21 3.570 15.84  0  0    3    4 16.43041
    ## 8  24.4 cyl4 146.7  62 3.69 3.190 20.00  1  0    4    2 23.70638
    ## 9  22.8 cyl4 140.8  95 3.92 3.150 22.90  1  0    4    2 23.83236
    ## 10 19.2 cyl6 167.6 123 3.92 3.440 18.30  1  0    4    4 18.66166

To add prediction intervals, an additional calculation is neded against
the fitted result

    df %>%
      head(10) %>%
      mutate(fit = !! predict_fit(model),
             lwr = fit + !! predict_interval(model),
             upr = fit - !! predict_interval(model)) %>%
      select(fit, lwr, upr)

    ##         fit      lwr      upr
    ## 1  21.39443 27.24918 15.53968
    ## 2  20.59128 26.45624 14.72632
    ## 3  26.59663 32.22746 20.96580
    ## 4  19.37032 25.17746 13.56317
    ## 5  16.83986 22.51635 11.16336
    ## 6  18.59867 24.39004 12.80729
    ## 7  16.43041 22.05872 10.80210
    ## 8  23.70638 29.55725 17.85550
    ## 9  23.83236 29.67545 17.98927
    ## 10 18.66166 24.45297 12.87034

The confidence interval can also be modified, the default is 0.95

    predict_interval(model, interval = 0.99)

    ## (2.77068295712221) * sqrt((((((((((((ifelse((cyl) == ("cyl6"), 
    ##     (0), 0))) + ((ifelse((cyl) == ("cyl8"), (0), 0)))) + (((wt) * 
    ##     (0)))) + (((am) * (0)))) + (((-0.176776695296637)))) * ((((((ifelse((cyl) == 
    ##     ("cyl6"), (0), 0))) + ((ifelse((cyl) == ("cyl8"), (0), 0)))) + 
    ##     (((wt) * (0)))) + (((am) * (0)))) + (((-0.176776695296637)))) * 
    ##     (6.7766049449091)) + (((((((ifelse((cyl) == ("cyl6"), (0), 
    ##     0))) + ((ifelse((cyl) == ("cyl8"), (0), 0)))) + (((wt) * 
    ##     (0.183559646169165)))) + (((am) * (0)))) + (((-0.590557271637747)))) * 
    ##     ((((((ifelse((cyl) == ("cyl6"), (0), 0))) + ((ifelse((cyl) == 
    ##         ("cyl8"), (0), 0)))) + (((wt) * (0.183559646169165)))) + 
    ##         (((am) * (0)))) + (((-0.590557271637747)))) * (6.7766049449091))) + 
    ##     (((((((ifelse((cyl) == ("cyl6"), (0), 0))) + ((ifelse((cyl) == 
    ##         ("cyl8"), (0), 0)))) + (((wt) * (-0.176199380745393)))) + 
    ##         (((am) * (-0.498926847360626)))) + (((0.769566489443368)))) * 
    ##         ((((((ifelse((cyl) == ("cyl6"), (0), 0))) + ((ifelse((cyl) == 
    ##             ("cyl8"), (0), 0)))) + (((wt) * (-0.176199380745393)))) + 
    ##             (((am) * (-0.498926847360626)))) + (((0.769566489443368)))) * 
    ##         (6.7766049449091))) + (((((((ifelse((cyl) == ("cyl6"), 
    ##     (-0.428347713758794), 0))) + ((ifelse((cyl) == ("cyl8"), 
    ##     (0), 0)))) + (((wt) * (-0.0135489721333101)))) + (((am) * 
    ##     (-0.00972707159804957)))) + (((0.141243115817336)))) * ((((((ifelse((cyl) == 
    ##     ("cyl6"), (-0.428347713758794), 0))) + ((ifelse((cyl) == 
    ##     ("cyl8"), (0), 0)))) + (((wt) * (-0.0135489721333101)))) + 
    ##     (((am) * (-0.00972707159804957)))) + (((0.141243115817336)))) * 
    ##     (6.7766049449091))) + (((((((ifelse((cyl) == ("cyl6"), (-0.332281834786717), 
    ##     0))) + ((ifelse((cyl) == ("cyl8"), (-0.64678807848721), 0)))) + 
    ##     (((wt) * (0.238228067530327)))) + (((am) * (0.0212231164458079)))) + 
    ##     (((-0.419404705620305)))) * ((((((ifelse((cyl) == ("cyl6"), 
    ##     (-0.332281834786717), 0))) + ((ifelse((cyl) == ("cyl8"), 
    ##     (-0.64678807848721), 0)))) + (((wt) * (0.238228067530327)))) + 
    ##     (((am) * (0.0212231164458079)))) + (((-0.419404705620305)))) * 
    ##     (6.7766049449091))) + (6.7766049449091))

### Model parser

The `parsemodel()` function returns a tidy table with the data needed to
run the predictions

    parsemodel(model)

    ## # A tibble: 8 x 9
    ##        labels            vals        type   estimate       qr_1       qr_2
    ##         <chr>           <chr>       <chr>      <dbl>      <dbl>      <dbl>
    ## 1 (Intercept)                   intercept 33.7535920 -0.1767767 -0.5905573
    ## 2          wt                  continuous -3.1495978  0.0000000  0.1835596
    ## 3          am                  continuous  0.1501031  0.0000000  0.0000000
    ## 4         cyl            cyl6 categorical -4.2573185  0.0000000  0.0000000
    ## 5         cyl            cyl8 categorical -6.0791189  0.0000000  0.0000000
    ## 6       model              lm    variable         NA         NA         NA
    ## 7    residual              27    variable         NA         NA         NA
    ## 8      sigma2 6.7766049449091    variable         NA         NA         NA
    ## # ... with 3 more variables: qr_3 <dbl>, qr_4 <dbl>, qr_5 <dbl>

`predict_fit()` does not need all of the columns in this table, it only
requires the first four. The `qr_...` fields are use to calculate the
prediction intervals, the are the result of running `qr.solve()` against
the model's `qr` variable.

### Save, and reload, a parsed model

The output of the model parser can be saved as a `.csv` file and
reloaded at a later time. The predition functions have an S3 method for
`data.frame`, so the `model` entry is used to determine which predict
formula to compile.

    write.csv(parsemodel(model), "model.csv")


    reloaded_model <- read.csv("model.csv")

    df %>%
      head(10) %>%
      predict_to_column(reloaded_model, add_interval = TRUE, vars = c("ft", "up", "lw")) %>%
      select(ft, lw, up)

    ##          ft       lw       up
    ## 1  21.39443 21.39443 21.39443
    ## 2  20.59128 20.59128 20.59128
    ## 3  26.59663 26.59663 26.59663
    ## 4  19.37032 19.37032 19.37032
    ## 5  16.83986 16.83986 16.83986
    ## 6  18.59867 18.59867 18.59867
    ## 7  16.43041 16.43041 16.43041
    ## 8  23.70638 23.70638 23.70638
    ## 9  23.83236 23.83236 23.83236
    ## 10 18.66166 18.66166 18.66166
