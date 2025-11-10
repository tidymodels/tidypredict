# Turn a path object into a combined expression

Turn a path object into a combined expression

## Usage

``` r
path_formulas(path)
```

## Arguments

- path:

  a list of lists.

  This list can contain 0 or more elemements. The elements but each be
  of the following format:

  - `type` character, must be `"conditional"`, `"set"`, or `"all"`.

  - `op` character. if `type == "conditional"` must be `"more"`,
    `"more-equal"`, `"less"`, or `"less-equal"`. if `type == "set"` must
    be `"in"` on `not-in`.

  - `col` character.

  - `val` if `type == "conditional"` and `vals` if `type == "set"`. Can
    be character or numeric. @keywords internal
