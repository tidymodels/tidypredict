# Model can be saved and re-loaded

    Code
      tidypredict_fit(pm)
    Output
      41.1350198 + (wt * -0.6978035) + (disp * -0.1224733) + (ifelse(cyl == 
          "cyl6", 1, 0) * -12.9466721) + (ifelse(cyl == "cyl8", 1, 
          0) * -17.0571646) + (wt * ifelse(cyl == "cyl6", 1, 0) * -3.1372923) + 
          (wt * ifelse(cyl == "cyl8", 1, 0) * -1.3253343) + (disp * 
          ifelse(cyl == "cyl6", 1, 0) * 0.1416155) + (disp * ifelse(cyl == 
          "cyl8", 1, 0) * 0.1199615)

# we get better error from QR decomposition issues (#124)

    Code
      tidypredict::tidypredict_fit(lm_fit)
    Condition
      Error in `parse_model()`:
      x Unable to calculate inverse of QR decomposition.
      i This is likely happening because the predictors contain a linear combination of predictors. Please remove and try again.

