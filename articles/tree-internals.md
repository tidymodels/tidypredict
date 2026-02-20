# How tidypredict generates tree formulas

This vignette explains how tidypredict converts decision tree models
into dplyr formulas, and why we chose nested
[`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
expressions over flat ones.

## Nested vs flat case_when

Consider a simple decision tree:

               +-------+
          +----|x <= 5 |----+
          |    +-------+    |
          v                 v
      +-------+           "high"
      |y <= 3 |
      +-------+
       |     |
       v     v
     "low" "med"

This tree has three leaves with predictions “low”, “med”, and “high”.

### Flat case_when (old approach)

The flat approach lists every leaf path as a separate condition:

``` r
case_when(
  x <= 5 & y <= 3 ~ "low",
  x <= 5 & y > 3  ~ "med",
  x > 5           ~ "high"
)
```

Each condition must encode the **entire path** from root to leaf. For a
tree with depth `d`, each condition can have up to `d` comparisons
joined by `&`.

### Nested case_when (current approach)

The nested approach mirrors the tree structure:

``` r
case_when(
  x <= 5 ~ case_when(
    y <= 3 ~ "low",
    .default = "med"
  ),
  .default = "high"
)
```

Each node becomes its own
[`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html),
with the left branch as the condition and the right branch as
`.default`.

### Why nested is better

**1. Fewer comparisons at runtime**

With flat `case_when`, R evaluates conditions sequentially until one
matches. In the worst case (the last leaf), all conditions are checked.
Each condition re-evaluates splits that were already decided higher in
the tree.

With nested `case_when`, each split is evaluated exactly once. The
`.default` clause handles the “else” branch without re-checking the
condition.

**2. More efficient SQL**

SQL databases optimize nested `CASE WHEN` statements better than flat
ones with compound `AND` conditions. The nested structure allows the
query planner to short-circuit evaluation.

Flat SQL:

``` sql
CASE
  WHEN x <= 5 AND y <= 3 THEN 'low'
  WHEN x <= 5 AND y > 3 THEN 'med'
  WHEN x > 5 THEN 'high'
END
```

Nested SQL:

``` sql
CASE
  WHEN x <= 5 THEN
    CASE
      WHEN y <= 3 THEN 'low'
      ELSE 'med'
    END
  ELSE 'high'
END
```

**3. Smaller formula size**

For a balanced tree of depth `d` with `2^d` leaves:

- Flat: Each leaf condition has `d` terms, so total terms = `d * 2^d`
- Nested: Each split appears once, so total terms = `2^d - 1`

For a tree of depth 2 (4 leaves):

- Flat: 2 \* 4 = 8 comparison terms
- Nested: 4 - 1 = 3 comparison terms

For a tree of depth 6 (64 leaves):

- Flat: 6 \* 64 = 384 comparison terms
- Nested: 64 - 1 = 63 comparison terms

## Parsed model versions

tidypredict uses a version number in parsed models to track format
changes: - **Version 1-2**: Used flat
[`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
for trees - **Version 3**: Uses nested
[`case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html)
(current)

When loading a model saved with an older version, tidypredict
automatically uses the appropriate formula builder for backwards
compatibility. See
[`?parse_model`](https://tidypredict.tidymodels.org/reference/parse_model.md)
for details.
