# returns the right output

    Code
      lapply(lps, round_print)
    Output
      [[1]]
      [1] "-113.7146 + (Sepal.Length * 44.55288) + (Sepal.Width * -7.615958) + (Petal.Length * 33.55954) + (Petal.Width * -31.25587) + (Sepal.Length * Sepal.Length * -9.471719) + (Sepal.Length * Sepal.Width * 12.40483) + (Sepal.Length * Petal.Length * 4.500207) + (Sepal.Length * Petal.Width * 4.776127) + (Sepal.Width * Sepal.Width * -7.78527) + (Sepal.Width * Petal.Length * -1.111079) + (Sepal.Width * Petal.Width * 2.104098) + (Petal.Length * Petal.Length * -19.3881) + (Petal.Length * Petal.Width * 17.93504) + (Petal.Width * Petal.Width * -53.02295)"
      
      [[2]]
      [1] "-68.43729 + (Sepal.Length * 18.01286) + (Sepal.Width * 15.9607) + (Petal.Length * 3.268785) + (Petal.Width * -14.71256) + (Sepal.Length * Sepal.Length * -4.751382) + (Sepal.Length * Sepal.Width * 3.676217) + (Sepal.Length * Petal.Length * 8.631712) + (Sepal.Length * Petal.Width * -6.454503) + (Sepal.Width * Sepal.Width * -9.855483) + (Sepal.Width * Petal.Length * -2.116022) + (Sepal.Width * Petal.Width * 19.48032) + (Petal.Length * Petal.Length * -9.901879) + (Petal.Length * Petal.Width * 26.93723) + (Petal.Width * Petal.Width * -43.6224)"
      
      [[3]]
      [1] "-67.70908 + (Sepal.Length * 7.372475) + (Sepal.Width * 13.24526) + (Petal.Length * 6.234069) + (Petal.Width * 9.661976) + (Sepal.Length * Sepal.Length * -5.266933) + (Sepal.Length * Sepal.Width * 3.479726) + (Sepal.Length * Petal.Length * 9.960146) + (Sepal.Length * Petal.Width * -1.788152) + (Sepal.Width * Sepal.Width * -7.937721) + (Sepal.Width * Petal.Length * -1.102689) + (Sepal.Width * Petal.Width * 8.472851) + (Petal.Length * Petal.Length * -6.70291) + (Petal.Length * Petal.Width * 2.890918) + (Petal.Width * Petal.Width * -9.657025)"
      

# tidypredict_test errors for qda models

    Code
      tidypredict_test(model, iris)
    Condition
      Error in `tidypredict_test()`:
      ! `tidypredict_test()` does not support `MASS::qda()` models.
      i Use `tidypredict_fit()` directly for multiclass predictions.

# inline functions in the formula are rejected

    Code
      tidypredict_fit(model)
    Condition
      Error in `acceptable_lm()`:
      x Functions inside the formula are not supported.
      i Functions detected: "log".  Use `dplyr` transformations to prepare the data.

