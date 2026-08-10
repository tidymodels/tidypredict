# Generate nested case_when for a tree

Generate nested case_when for a tree

## Usage

``` r
generate_nested_case_when_tree(tree_info, na_propagate = FALSE)
```

## Arguments

- tree_info:

  A tree info list from `rpart_tree_info_full()` or similar

- na_propagate:

  Return `NA` for a row once its path reaches a split on a column it is
  missing, rather than sending it down the `.default` branch.
