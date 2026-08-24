# Construct a single node of a tree

Construct a single node of a tree

## Usage

``` r
generate_tree_node(node, calc_mode = "")
```

## Arguments

- node:

  a list with named elements `path` and `prediction`. See details for
  more.

- calc_mode:

  character, takes values `""` and `"calc_mode"`.

  The `node` list should contain the two lists `path` and `prediction`.

  The `path` element has the following structure:

  This list can contain 0 or more elemements. The elements but each be
  of the following format:

  - `type` character, must be `"conditional"`, `"set"`, or `"all"`.

  - `op` character. if `type == "conditional"` must be `"more"`,
    `"more-equal"`, `"less"`, or `"less-equal"`. if `type == "set"` must
    be `"in"` on `not-in`.

  - `col` character.

  - `val` if `type == "conditional"` and `vals` if `type == "set"`. Can
    be character or numeric.

  The `prediction` list has the following structure:

  It can either be a singular value or a list. If it is a list it will
  have the following 4 named elements `col`, `val`, `op`, and
  `is_intercept`.

  - `col` character, name of column

  - `val` val, numeric of character

  - `op` character, known values are `"none"` and `"multiply"`. `"none"`
    is used then `is_intercept == 1`.

  - `is_intercept`integer, takes values `0` and `1`.\`

  @keywords internal
