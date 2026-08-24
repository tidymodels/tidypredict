# Turn a path object into an expression

Turn a path object into an expression

## Usage

``` r
path_formula(x)
```

## Arguments

- x:

  a list.

  The input of this function is a list with 4 values.

  - `type` character, must be `"conditional"` or `"set"`.

  - `op` character. if `type == "conditional"` must be `"more"`,
    `"more-equal"`, `"less"`, or `"less-equal"`. if `type == "set"` must
    be `"in"` on `not-in`.

  - `col` character.

  - `val` if `type == "conditional"` and `vals` if `type == "set"`. Can
    be character or numeric. @keywords internal
