# Generate nested case_when for a tree

Generate nested case_when for a tree

## Usage

``` r
generate_nested_case_when_tree(tree_info, missing = c("default", "na", "left"))
```

## Arguments

- tree_info:

  A tree info list from `rpart_tree_info_full()` or similar

- missing:

  What a row missing this split's column should do: `"default"` takes
  the `.default` branch, `"na"` returns `NA`, and `"left"` takes the
  left branch.
