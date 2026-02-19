# Build case_when expression from nodes with predictions and paths

Shared helper for building tree expressions used by ranger and
randomForest classification extractors.

## Usage

``` r
.build_case_when_tree(nodes)
```

## Arguments

- nodes:

  A list of lists, each with `prediction` (numeric) and `path`
