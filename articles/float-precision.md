# Float precision at split boundaries

## The issue

Tree-based models like XGBoost, LightGBM, and CatBoost internally
convert data to 32-bit floats during training. This means that split
thresholds are chosen based on 32-bit precision values. However, R uses
64-bit doubles by default, and most databases also use higher precision
floating-point numbers.

This precision mismatch can cause predictions to differ when a data
point falls exactly on or very close to a split boundary. The 32-bit and
64-bit representations of the same number may round differently, causing
the data point to go left in one system and right in another.

## Which models are affected?

XGBoost and Cubist store everything as 32-bit floats, making them most
susceptible to this issue. LightGBM and CatBoost use 64-bit doubles for
leaf values, which reduces (but does not eliminate) the risk.

## Example

Here is a real example from a Cubist model. When we extract the split
values used in the model’s rules, we see values like:

    variable     value
    lstat    9.5299997
    rm       6.2259998
    rm       6.546
    lstat    5.3899999

These split values should correspond to actual values in the training
data. But when we check, only one of the four matches exactly:

    # Exact matches
    variable value
    rm       6.546

    # Non-matches
    variable     value
    lstat    9.5299997
    rm       6.2259998
    lstat    5.3899999

If we look for nearby values in the training data:

    variable value_data value_split
    rm            6.226   6.2259998
    rm            6.546   6.546
    lstat         5.39    5.3899999
    lstat         9.53    9.5299997

The original training values were `6.226`, `5.39`, and `9.53`, but they
were converted to 32-bit floats during model training, resulting in
slightly different stored thresholds.

Why does this matter? Consider a model with two rules:

    rule 1: rm > 6.2259998
    rule 2: rm <= 6.2259998

If you pass in an observation where `rm` is `6.226`, you might expect
rule 1 to apply since `6.226 > 6.2259998`. But the native model applies
rule 2 because it internally converted `6.226` to `6.2259998` during
training, making them equal.

## What tidypredict does

tidypredict extracts split thresholds from the model and uses them in R
formulas or SQL queries. Since R and databases typically use 64-bit
floats, the comparisons are done at 64-bit precision against thresholds
that were originally determined at 32-bit precision.

In most cases, this works fine because data points rarely fall exactly
on split boundaries. However, you should always verify predictions match
using
[`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md).

## Pros and cons

Pros of using tidypredict despite this issue:

- In-database scoring avoids moving large datasets out of the database
- SQL translation is portable across database systems
- For most rows, predictions will match exactly

Risks:

- A small fraction of predictions may differ unpredictably
- For classification, boundary cases could flip the predicted class
- It is hard to know in advance which specific rows will be affected

When are values likely to hit boundaries?

- Integer or rounded data (e.g., age in whole years, prices rounded to
  cents)
- Data with limited unique values or repeated measurements
- Scoring the same data used for training

Continuous or high-precision real-world measurements are less likely to
land exactly on split boundaries.

Considerations:

- Use
  [`tidypredict_test()`](https://tidypredict.tidymodels.org/reference/tidypredict_test.md)
  to quantify the discrepancy rate on your data
- The magnitude of difference is usually small (just a neighboring leaf
  value)
- High-stakes applications (medical, financial) may need stricter
  validation

## Recommendations

1.  Accept small differences: For production use, consider that a tiny
    fraction of predictions may differ at exact boundaries. Decide if
    this is acceptable for your use case.

2.  Use native predictions when possible: For applications where perfect
    agreement is critical, consider using the native model’s predict
    function rather than SQL translation.
