Untitled
================
Edgar Ruiz
December 18, 2017

``` r
library(tidypredict)
```

    ## Loading required package: purrr

    ## Loading required package: rlang

    ## 
    ## Attaching package: 'rlang'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     %@%, %||%, as_function, flatten, flatten_chr, flatten_dbl,
    ##     flatten_int, flatten_lgl, invoke, list_along, modify, prepend,
    ##     rep_along, splice

    ## Loading required package: tibble

    ## 
    ## Attaching package: 'tibble'

    ## The following object is masked from 'package:rlang':
    ## 
    ##     has_name

    ## Warning: replacing previous import 'purrr::invoke' by 'rlang::invoke' when
    ## loading 'tidypredict'

    ## Warning: replacing previous import 'purrr::flatten_int' by
    ## 'rlang::flatten_int' when loading 'tidypredict'

    ## Warning: replacing previous import 'purrr::flatten_dbl' by
    ## 'rlang::flatten_dbl' when loading 'tidypredict'

    ## Warning: replacing previous import 'purrr::list_along' by
    ## 'rlang::list_along' when loading 'tidypredict'

    ## Warning: replacing previous import 'purrr::%||%' by 'rlang::%||%' when
    ## loading 'tidypredict'

    ## Warning: replacing previous import 'purrr::flatten_chr' by
    ## 'rlang::flatten_chr' when loading 'tidypredict'

    ## Warning: replacing previous import 'purrr::prepend' by 'rlang::prepend'
    ## when loading 'tidypredict'

    ## Warning: replacing previous import 'purrr::flatten' by 'rlang::flatten'
    ## when loading 'tidypredict'

    ## Warning: replacing previous import 'purrr::rep_along' by 'rlang::rep_along'
    ## when loading 'tidypredict'

    ## Warning: replacing previous import 'purrr::splice' by 'rlang::splice' when
    ## loading 'tidypredict'

    ## Warning: replacing previous import 'purrr::modify' by 'rlang::modify' when
    ## loading 'tidypredict'

    ## Warning: replacing previous import 'purrr::flatten_lgl' by
    ## 'rlang::flatten_lgl' when loading 'tidypredict'

    ## Warning: replacing previous import 'purrr::as_function' by
    ## 'rlang::as_function' when loading 'tidypredict'

    ## Warning: replacing previous import 'purrr::%@%' by 'rlang::%@%' when
    ## loading 'tidypredict'

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

tidypredict(m4)
```

    ## (((ifelse((cyl) == ("cyl6"), (-4.2555824019713), 0)) + (ifelse((cyl) == 
    ##     ("cyl8"), (-6.07085968049089), 0))) + ((wt) * (-3.20561325619286))) + 
    ##     (33.9907940091325)
