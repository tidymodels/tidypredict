tidypredict
================

Installation
------------

Install `tidypredict` using `devtools` as follows:

``` r
devtools::install_github("edgararuiz/tidypredict")
```

Intro
-----

Run the predictions of an R model inside the database. `tidypredict` parses a model fitted in R, such as:

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)
```

And creates the SQL statement needed to calculate the fitted prediction:

``` r
tidypredict_sql(model, dbplyr::simulate_mssql())
```

    ## <SQL> (((`wt`) * (-3.19097213898374)) + ((`cyl`) * (-1.5077949682598))) + (39.6862614802529)

Supported models
----------------

The following models are supported:

-   Linear Regression - `lm()`
-   Generalized Linear model - `glm()`
-   Random Forest - `randomForest()`
