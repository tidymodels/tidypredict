# Returns expected dplyr formula

    Code
      rlang::expr_text(tf)
    Output
      [1] "((ifelse(nox > 0.66799998, -1.11 + crim * -0.02 + nox * 21.4 + \n    rm * 0.1 + age * -0.003 + dis * 2.93 + ptratio * -0.13 + \n    b * 0.008 + lstat * -0.33, 0) + ifelse(lstat > 9.5900002 & \n    nox <= 0.66799998, 23.57 + crim * 0.05 + nox * -5.2 + rm * \n    3.1 + age * -0.048 + dis * -0.81 + rad * 0.02 + tax * -0.0041 + \n    ptratio * -0.71 + b * 0.01 + lstat * -0.15, 0) + ifelse(lstat <= \n    9.5900002 & rm <= 6.2259998, 1.18 + crim * 3.83 + rm * 4.3 + \n    age * -0.06 + dis * -0.09 + tax * -0.003 + ptratio * -0.08 + \n    lstat * -0.11, 0) + ifelse(lstat <= 9.5900002 & rm > 6.2259998, \n    -4.71 + crim * 2.22 + zn * 0.008 + nox * -1.7 + rm * 9.2 + \n        age * -0.04 + dis * -0.71 + rad * 0.03 + tax * -0.0182 + \n        ptratio * -0.72 + lstat * -0.83, 0))/((nox > 0.66799998) + \n    (lstat > 9.5900002 & nox <= 0.66799998) + (lstat <= 9.5900002 & \n    rm <= 6.2259998) + (lstat <= 9.5900002 & rm > 6.2259998)) + \n    (ifelse(dis <= 1.7553999 & lstat > 5.1199999, 122.32 + crim * \n        -0.29 + nox * -21.6 + rm * -3 + dis * -30.88 + rad * \n        0.02 + tax * -0.001 + b * -0.023 + lstat * -0.73, 0) + \n        ifelse(rm <= 6.5450001 & lstat > 5.1199999, 27.8 + crim * \n            -0.16 + zn * 0.007 + nox * -3.9 + rm * 2 + age * \n            -0.035 + dis * -0.7 + rad * 0.28 + tax * -0.0135 + \n            ptratio * -0.6 + b * 0.013 + lstat * -0.25, 0) + \n        ifelse(rm > 6.5450001 & lstat > 5.1199999, 22.21 + crim * \n            -0.04 + zn * 0.01 + indus * -0.02 + nox * -4 + rm * \n            4.7 + dis * -0.34 + rad * 0.11 + tax * -0.0248 + \n            ptratio * -0.9 + b * 0.002 + lstat * -0.1, 0) + ifelse(lstat <= \n        5.1199999 & rm <= 8.0340004, -71.95 + rm * 17 + age * \n        -0.06 + tax * -0.0112 + ptratio * -0.48 + lstat * -0.03, \n        0) + ifelse(rm > 8.0340004 & dis > 3.1991999, -32.79 + \n        crim * -0.01 + zn * 0.005 + nox * -1.8 + rm * 12.9 + \n        age * -0.117 + dis * -0.15 + rad * 0.04 + tax * -0.0246 + \n        ptratio * -1.05 + lstat * -0.04, 0) + ifelse(lstat <= \n        5.1199999 & dis <= 3.1991999, 53.41 + rm * 1.6 + dis * \n        -7.16 + tax * 0.0088 + lstat * -0.68, 0))/((dis <= 1.7553999 & \n        lstat > 5.1199999) + (rm <= 6.5450001 & lstat > 5.1199999) + \n        (rm > 6.5450001 & lstat > 5.1199999) + (lstat <= 5.1199999 & \n        rm <= 8.0340004) + (rm > 8.0340004 & dis > 3.1991999) + \n        (lstat <= 5.1199999 & dis <= 3.1991999)) + (ifelse(nox > \n    0.66799998, -36.31 + crim * 0.08 + nox * 48.4 + dis * 7.52 + \n    b * 0.01 + lstat * -0.24, 0) + ifelse(lstat > 9.5299997 & \n    nox <= 0.66799998, 28.04 + nox * -4.8 + rm * 2.9 + age * \n    -0.051 + dis * -0.86 + rad * 0.01 + tax * -0.0019 + ptratio * \n    -0.72 + lstat * -0.12, 0) + ifelse(lstat <= 9.5299997, -26.05 + \n    crim * 0.89 + nox * -2.3 + rm * 9.6 + dis * -0.17 + rad * \n    0.02 + tax * -0.0055 + ptratio * -0.12 + b * 0.001 + lstat * \n    -0.74, 0) + ifelse(lstat <= 9.5299997 & dis <= 2.6403, 136.67 + \n    crim * 7.2 + nox * -96.6 + rm * 1.1 + tax * -0.0033 + ptratio * \n    -3.31 + lstat * -0.1, 0))/((nox > 0.66799998) + (lstat > \n    9.5299997 & nox <= 0.66799998) + (lstat <= 9.5299997) + (lstat <= \n    9.5299997 & dis <= 2.6403)))/3"

# Model can be saved and re-loaded

    Code
      tidypredict_fit(pm)
    Output
      (ifelse(nox > 0.668, -1.11 + crim * -0.02 + nox * 21.4 + rm * 
          0.1 + age * -0.003 + dis * 2.93 + ptratio * -0.13 + b * 0.008 + 
          lstat * -0.33, 0) + ifelse(lstat > 9.5900002 & nox <= 0.668, 
          23.57 + crim * 0.05 + nox * -5.2 + rm * 3.1 + age * -0.048 + 
              dis * -0.81 + rad * 0.02 + tax * -0.0041 + ptratio * 
              -0.71 + b * 0.01 + lstat * -0.15, 0) + ifelse(lstat <= 
          9.5900002 & rm <= 6.2259998, 1.18 + crim * 3.83 + rm * 4.3 + 
          age * -0.06 + dis * -0.09 + tax * -0.003 + ptratio * -0.08 + 
          lstat * -0.11, 0) + ifelse(lstat <= 9.5900002 & rm > 6.2259998, 
          -4.71 + crim * 2.22 + zn * 0.008 + nox * -1.7 + rm * 9.2 + 
              age * -0.04 + dis * -0.71 + rad * 0.03 + tax * -0.0182 + 
              ptratio * -0.72 + lstat * -0.83, 0) + ifelse(dis <= 1.7553999 & 
          lstat > 5.1199999, 122.32 + crim * -0.29 + nox * -21.6 + 
          rm * -3 + dis * -30.88 + rad * 0.02 + tax * -0.001 + b * 
          -0.023 + lstat * -0.73, 0) + ifelse(rm <= 6.5450001 & lstat > 
          5.1199999, 27.8 + crim * -0.16 + zn * 0.007 + nox * -3.9 + 
          rm * 2 + age * -0.035 + dis * -0.7 + rad * 0.28 + tax * -0.0135 + 
          ptratio * -0.6 + b * 0.013 + lstat * -0.25, 0) + ifelse(rm > 
          6.5450001 & lstat > 5.1199999, 22.21 + crim * -0.04 + zn * 
          0.01 + indus * -0.02 + nox * -4 + rm * 4.7 + dis * -0.34 + 
          rad * 0.11 + tax * -0.0248 + ptratio * -0.9 + b * 0.002 + 
          lstat * -0.1, 0) + ifelse(lstat <= 5.1199999 & rm <= 8.0340004, 
          -71.95 + rm * 17 + age * -0.06 + tax * -0.0112 + ptratio * 
              -0.48 + lstat * -0.03, 0) + ifelse(rm > 8.0340004 & dis > 
          3.1991999, -32.79 + crim * -0.01 + zn * 0.005 + nox * -1.8 + 
          rm * 12.9 + age * -0.117 + dis * -0.15 + rad * 0.04 + tax * 
          -0.0246 + ptratio * -1.05 + lstat * -0.04, 0) + ifelse(lstat <= 
          5.1199999 & dis <= 3.1991999, 53.41 + rm * 1.6 + dis * -7.16 + 
          tax * 0.0088 + lstat * -0.68, 0) + ifelse(nox > 0.668, -36.31 + 
          crim * 0.08 + nox * 48.4 + dis * 7.52 + b * 0.01 + lstat * 
          -0.24, 0) + ifelse(lstat > 9.5299997 & nox <= 0.668, 28.04 + 
          nox * -4.8 + rm * 2.9 + age * -0.051 + dis * -0.86 + rad * 
          0.01 + tax * -0.0019 + ptratio * -0.72 + lstat * -0.12, 0) + 
          ifelse(lstat <= 9.5299997, -26.05 + crim * 0.89 + nox * -2.3 + 
              rm * 9.6 + dis * -0.17 + rad * 0.02 + tax * -0.0055 + 
              ptratio * -0.12 + b * 0.001 + lstat * -0.74, 0) + ifelse(lstat <= 
          9.5299997 & dis <= 2.6403, 136.67 + crim * 7.2 + nox * -96.6 + 
          rm * 1.1 + tax * -0.0033 + ptratio * -3.31 + lstat * -0.1, 
          0))/3

