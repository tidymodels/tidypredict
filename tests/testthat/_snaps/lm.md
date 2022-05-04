# tidypredict works when variable names are subset of other variables

    Code
      tidypredict_fit(model4)
    Output
      3.18618757155392 + (wt * -1.5247865111278) + (wt_sq * 0.152224442011265) + 
          (ifelse(char_cyl == "cyl6", 1, 0) * 0.402192959289173) + 
          (ifelse(char_cyl == "cyl8", 1, 0) * 0.373779132787549) + 
          (ifelse(char_cyl_2 == "b", 1, 0) * 0.235159663741481) + (ifelse(char_cyl_2 == 
          "c", 1, 0) * 0.223666615868374)

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

