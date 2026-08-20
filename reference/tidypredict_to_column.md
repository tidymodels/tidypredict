# Adds the prediction columns to a piped command set.

Adds a new column with the results from tidypredict_fit() to a piped
command set. If add_interval is set to TRUE, it will add two additional
columns- one for the lower and another for the upper prediction interval
bounds.

## Usage

``` r
tidypredict_to_column(
  df,
  model,
  add_interval = FALSE,
  interval = 0.95,
  vars = c("fit", "upper", "lower")
)
```

## Arguments

- df:

  A data.frame or tibble

- model:

  An R model or a parsed model inside a data frame

- add_interval:

  Switch that indicates if the prediction interval columns should be
  added. Defaults to FALSE

- interval:

  The prediction interval, defaults to 0.95. Ignored if add_interval is
  set to FALSE

- vars:

  The name of the variables that this function will produce. Defaults to
  "fit", "upper", and "lower".

## Value

The input data frame with one new column (the fit) added, or three new
columns (fit, upper and lower bounds) when `add_interval` is `TRUE`.

## Examples

``` r
model <- lm(mpg ~ wt, data = mtcars)
tidypredict_to_column(mtcars, model)
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4
#> Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4
#> Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4
#> Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3
#> Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3
#> Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3
#> Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4
#> Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4
#> Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4
#> Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3
#> Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3
#> Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4
#> Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4
#> Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3
#> Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3
#> AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3
#> Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4
#> Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5
#> Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4
#>                     carb       fit
#> Mazda RX4              4 23.282611
#> Mazda RX4 Wag          4 21.919770
#> Datsun 710             1 24.885952
#> Hornet 4 Drive         1 20.102650
#> Hornet Sportabout      2 18.900144
#> Valiant                1 18.793255
#> Duster 360             4 18.205363
#> Merc 240D              2 20.236262
#> Merc 230               2 20.450041
#> Merc 280               4 18.900144
#> Merc 280C              4 18.900144
#> Merc 450SE             3 15.533127
#> Merc 450SL             3 17.350247
#> Merc 450SLC            3 17.083024
#> Cadillac Fleetwood     4  9.226650
#> Lincoln Continental    4  8.296712
#> Chrysler Imperial      4  8.718926
#> Fiat 128               1 25.527289
#> Honda Civic            2 28.653805
#> Toyota Corolla         1 27.478021
#> Toyota Corona          1 24.111004
#> Dodge Challenger       2 18.472586
#> AMC Javelin            2 18.926866
#> Camaro Z28             4 16.762355
#> Pontiac Firebird       2 16.735633
#> Fiat X1-9              1 26.943574
#> Porsche 914-2          2 25.847957
#> Lotus Europa           2 29.198941
#> Ford Pantera L         4 20.343151
#> Ferrari Dino           6 22.480940
#> Maserati Bora          8 18.205363
#> Volvo 142E             2 22.427495
```
