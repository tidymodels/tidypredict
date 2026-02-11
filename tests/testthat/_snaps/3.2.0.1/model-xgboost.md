# returns the right output

    Code
      rlang::expr_text(tf)
    Output
      [1] "case_when(qsec < 20 & wt < 3.19000006 ~ 0.138461545, (qsec >= \n    20 | is.na(qsec)) & wt < 3.19000006 ~ -0.100000009, hp < \n    264 & (wt >= 3.19000006 | is.na(wt)) ~ -0.141666666, (hp >= \n    264 | is.na(hp)) & (wt >= 3.19000006 | is.na(wt)) ~ 0.075000003) + \n    case_when(qsec < 20 & wt < 3.1500001 ~ 0.0994230807, (qsec >= \n        20 | is.na(qsec)) & wt < 3.1500001 ~ -0.0599999987, hp < \n        264 & (wt >= 3.1500001 | is.na(wt)) ~ -0.102500007, (hp >= \n        264 | is.na(hp)) & (wt >= 3.1500001 | is.na(wt)) ~ 0.0786538497) + \n    case_when(gear < 4 ~ -0.0735312551, wt < 3.1500001 & (gear >= \n        4 | is.na(gear)) ~ 0.0720817298, (wt >= 3.1500001 | is.na(wt)) & \n        (gear >= 4 | is.na(gear)) ~ -0.0186758228) + case_when(gear < \n    4 ~ -0.0528505854, qsec < 20 & (gear >= 4 | is.na(gear)) ~ \n    0.0427994467, (qsec >= 20 | is.na(qsec)) & (gear >= 4 | is.na(gear)) ~ \n    -0.0515981652) + 0.5"

