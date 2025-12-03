# Generate trees

Each tree is generated as a flat tree with each node being a seperate
part of the case when. This means that the following tree:

## Usage

``` r
generate_case_when_trees(parsedmodel, default = TRUE)
```

## Details

            +-----+
       +----|x > 0|----+
       |    +-----+    |
       v               v

+——+ +——–+ +–\|y \< 20\|–+ +–\|z \<= 10 \|–+ \| +——+ \| \| +——–+ \| v v
v v a b c d

will be turned into the following `case_when()` statement.

    case_when(
      x >  0 & y <  20 ~ "a",
      x >  0 & y >= 20 ~ "b",
      x <= 0 & z <= 10 ~ "c",
      x <= 0 & z >  10 ~ "d"
    )

instead of a nested `case_when()`s\` like this

    case_when(
      x >  0 ~ case_when(
                 y <  20 ~ "a",
                 y >= 10 ~ "b"
               ),
      x <= 0 ~ case_when(
                 z <= 10 ~ "c",
                 z >  10 ~ "d"
               )
    )

The functions in this file generates these tree.
`generate_case_when_tree()` generates a single tree with
`generate_case_when_trees()` being a convinience wrapper for multiple
trees.

[`generate_tree_node()`](https://tidypredict.tidymodels.org/reference/generate_tree_node.md)
generates the expressions for each a single ndoe in the tree, where
`generate_tree_nodes()` is a convinience wrapper for calculating all
notes.
