# returns the right output

    Code
      rlang::expr_text(tidypredict_fit(bart_round(pm)))
    Output
      [1] "(case_when(x1 <= -1.319 ~ -0.271, .default = case_when(x1 <= \n    2.265 ~ case_when(x1 <= 1.04 ~ 0.014, .default = 0.152), \n    .default = -0.059)) + case_when(x1 <= 0.087 ~ -0.061, .default = 0.041) + \n    (case_when(x1 <= -1.365 ~ -0.217, .default = case_when(x1 <= \n        2.265 ~ case_when(x1 <= 1.04 ~ -0.061, .default = 0.176), \n        .default = 0.434)) + case_when(x1 <= 0.087 ~ -0.081, \n        .default = 0.025)))/2 * 13.631 + 3.022"

# classification models are not supported

    Code
      tidypredict_fit(model)
    Condition
      Error in `check_bart_supported()`:
      ! Classification `dbarts::bart()` models are not supported.
      i Only regression models can be converted to tidy formulas.
      i Classification uses the probit link, which cannot be translated to SQL.

---

    Code
      parse_model(model)
    Condition
      Error in `check_bart_supported()`:
      ! Classification `dbarts::bart()` models are not supported.
      i Only regression models can be converted to tidy formulas.
      i Classification uses the probit link, which cannot be translated to SQL.

# models fit without trees are not supported

    Code
      tidypredict_fit(model)
    Condition
      Error in `check_bart_supported()`:
      ! `tidypredict_fit()` needs the trees of the `dbarts::bart()` model.
      i Refit the model with `keeptrees = TRUE`.

# models fit on unnamed predictors are not supported

    Code
      tidypredict_fit(model)
    Condition
      Error in `bart_column_map()`:
      ! `tidypredict_fit()` needs named predictors, the `dbarts::bart()` model was fit on an unnamed matrix.

# .extract_bart_trees() errors on the wrong model

    Code
      .extract_bart_trees(lm(mpg ~ wt, mtcars))
    Condition
      Error in `.extract_bart_trees()`:
      ! `model` must be <bart>, not a <lm> object.

---

    Code
      .extract_bart_scaling(lm(mpg ~ wt, mtcars))
    Condition
      Error in `.extract_bart_scaling()`:
      ! `model` must be <bart>, not a <lm> object.

