# Model can be saved and re-loaded

    Code
      tidypredict_fit(pm)
    Output
      1 - 1/(1 + exp(2.913526 + (ifelse(age > 32, age - 32, 0) * -0.0375715) + 
          (ifelse(pclass == "2nd", 1, 0) * ifelse(sex == "male", 1, 
              0) * -1.7680945) + (ifelse(pclass == "3rd", 1, 0) * -5.030056) + 
          (ifelse(pclass == "3rd", 1, 0) * ifelse(sibsp < 4, 4 - sibsp, 
              0) * 0.6186527) + (ifelse(pclass == "3rd", 1, 0) * ifelse(sex == 
          "male", 1, 0) * 1.2226954) + (ifelse(sex == "male", 1, 0) * 
          -3.1856245) + (ifelse(sex == "male", 1, 0) * ifelse(age < 
          16, 16 - age, 0) * 0.241814)))

