# Build nested case_when expression from tree info

Shared helper for building nested tree expressions. This is the nested
equivalent of
[`.build_case_when_tree()`](https://tidypredict.tidymodels.org/reference/dot-build_case_when_tree.md).

## Usage

``` r
.build_nested_case_when_tree(tree_info)
```

## Arguments

- tree_info:

  A tree info list with nodeID, leftChild, rightChild, splitvarName,
  terminal, prediction, and node_splits
