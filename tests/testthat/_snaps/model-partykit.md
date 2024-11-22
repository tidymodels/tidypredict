# Model can be saved and re-loaded

    Code
      tidypredict_fit(pm)
    Output
      [[1]]
      case_when(wt <= 2.32 ~ 29.0285714, wt <= 3.46 & wt > 2.32 ~ 19.8571429, 
          wt > 3.46 & wt > 2.32 ~ 14.7)
      

