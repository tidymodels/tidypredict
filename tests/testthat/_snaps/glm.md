# tidypredict works when variable names are subset of other variables

    Code
      tidypredict_fit(model4)
    Output
      1 - 1/(1 + exp(132.797404713083 + (wt * -78.468619565122) + (wt_sq * 
          9.48372241378896) + (ifelse(char_cyl == "cyl6", 1, 0) * 17.4668465348677) + 
          (ifelse(char_cyl == "cyl8", 1, 0) * 8.63109418961669) + (ifelse(char_cyl_2 == 
          "b", 1, 0) * 0.931105422756543) + (ifelse(char_cyl_2 == "c", 
          1, 0) * 16.3493196799229)))

# Model can be saved and re-loaded

    Code
      tidypredict_fit(pm)
    Output
      1 - 1/(1 + exp(24.1453276 + (wt * -7.8977178) + (disp * -0.0269566) + 
          (ifelse(cyl == "cyl6", 1, 0) * 4.8670863) + (ifelse(cyl == 
          "cyl8", 1, 0) * 10.9478336)))

