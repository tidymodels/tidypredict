Untitled
================
Edgar Ruiz
December 18, 2017

``` r
library(tidypredict)
```

    ## Loading required package: rlang

    ## Loading required package: purrr

    ## 
    ## Attaching package: 'purrr'

    ## The following objects are masked from 'package:rlang':
    ## 
    ##     %@%, %||%, as_function, flatten, flatten_chr, flatten_dbl,
    ##     flatten_int, flatten_lgl, invoke, list_along, modify, prepend,
    ##     rep_along, splice

    ## Loading required package: tibble

``` r
df <- mtcars %>%
  mutate(cyl = paste0("cyl", cyl))

m1 <- lm(mpg ~ wt + am, weights = cyl, data = mtcars)
m2 <- lm(mpg ~ wt + am, data = mtcars)
m3 <- lm(mpg ~ wt + am, offset = cyl, data = mtcars)
m4 <- lm(mpg ~ wt + cyl, data = df)
m5 <- glm(am ~ wt + mpg, data = mtcars)
m6 <- glm(am ~ cyl + mpg, data = df)

a1 <- as.numeric(predict(m1, mtcars))
a2 <- as.numeric(predict(m2, mtcars))
a3 <- as.numeric(predict(m3, mtcars))
a4 <- as.numeric(predict(m4, df))
a5 <- as.numeric(predict(m5, df))
a6 <- as.numeric(predict(m6, df))

tidypredict(m6)
```

    ## (((ifelse((cyl) == ("cyl6"), (0.0312386570459738), 0)) + (ifelse((cyl) == 
    ##     ("cyl8"), (-0.0331329111234639), 0))) + ((mpg) * (0.0476738160865827))) + 
    ##     (-0.543884568926792)
