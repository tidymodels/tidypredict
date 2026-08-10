# Build a nested case_when expression for a single node

Build a nested case_when expression for a single node

## Usage

``` r
build_nested_node(node_id, tree_info, missing = "default")
```

## Arguments

- node_id:

  The node ID to build (0-indexed)

- tree_info:

  Tree info list with nodeID, leftChild, rightChild, splitvarName,
  terminal, prediction, and node_splits

- missing:

  What a row missing this split's column should do: `"default"` takes
  the `.default` branch, `"na"` returns `NA`, and `"left"` takes the
  left branch.
