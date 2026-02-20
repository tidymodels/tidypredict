# Generate nested case_when trees

These functions generate nested `case_when()` expressions for decision
trees, which are more efficient than flat `case_when()` for both R/dplyr
and SQL execution.

## Usage

``` r
generate_nested_case_when_tree(tree_info)
```

## Arguments

- tree_info:

  A tree info list from `rpart_tree_info_full()` or similar

## Details

The following tree:

            +-----+
       +----|x > 0|----+
       |    +-----+    |
       v               v

+——+ +——–+ +–\|y \< 20\|–+ +–\|z \<= 10 \|–+ \| +——+ \| \| +——–+ \| v v
v v a b c d

will be turned into the following nested `case_when()` statement:

    case_when(
      x > 0 ~ case_when(
        y < 20 ~ "a",
        .default = "b"
      ),
      .default = case_when(
        z <= 10 ~ "c",
        .default = "d"
      )
    )

NA values in predictor columns are not handled by the generated
expression. Users should ensure that predictor columns do not contain NA
values before using the generated expression, or the results will be NA
for those rows.
