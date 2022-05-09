# Confirm SQL function returns a query

    Code
      rlang::expr_text(tidypredict_sql(xgb_reglinear, dbplyr::simulate_odbc())[[1]])
    Output
      [1] "\"0.0 + CASE\\nWHEN ((`qsec` < 19.9549999 OR ((`qsec`) IS NULL)) AND (`wt` < 3.18000007 OR ((`wt`) IS NULL))) THEN (0.138461545)\\nWHEN (`qsec` >= 19.9549999 AND (`wt` < 3.18000007 OR ((`wt`) IS NULL))) THEN (-0.100000009)\\nWHEN ((`hp` < 290.0 OR ((`hp`) IS NULL)) AND `wt` >= 3.18000007) THEN (-0.141666666)\\nWHEN (`hp` >= 290.0 AND `wt` >= 3.18000007) THEN (0.075000003)\\nEND + CASE\\nWHEN ((`qsec` < 19.9549999 OR ((`qsec`) IS NULL)) AND (`wt` < 3.01250005 OR ((`wt`) IS NULL))) THEN (0.0994230807)\\nWHEN (`qsec` >= 19.9549999 AND (`wt` < 3.01250005 OR ((`wt`) IS NULL))) THEN (-0.0599999987)\\nWHEN ((`hp` < 254.5 OR ((`hp`) IS NULL)) AND `wt` >= 3.01250005) THEN (-0.102500007)\\nWHEN (`hp` >= 254.5 AND `wt` >= 3.01250005) THEN (0.0786538497)\\nEND + CASE\\nWHEN ((`gear` < 3.5 OR ((`gear`) IS NULL))) THEN (-0.0735312551)\\nWHEN ((`wt` < 3.01250005 OR ((`wt`) IS NULL)) AND `gear` >= 3.5) THEN (0.0720817298)\\nWHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN (-0.0186758228)\\nEND + CASE\\nWHEN ((`gear` < 3.5 OR ((`gear`) IS NULL))) THEN (-0.0528505854)\\nWHEN ((`qsec` < 19.9500008 OR ((`qsec`) IS NULL)) AND `gear` >= 3.5) THEN (0.0427994467)\\nWHEN (`qsec` >= 19.9500008 AND `gear` >= 3.5) THEN (-0.0515981652)\\nEND + 0.5\""

---

    Code
      rlang::expr_text(tidypredict_sql(xgb_reglogistic, dbplyr::simulate_odbc())[[1]])
    Output
      [1] "\"1.0 - 1.0 / (1.0 + EXP(0.0 + CASE\\nWHEN (`wt` >= 3.18000007) THEN (-0.436363667)\\nWHEN ((`qsec` < 19.1849995 OR ((`qsec`) IS NULL)) AND (`wt` < 3.18000007 OR ((`wt`) IS NULL))) THEN (0.428571463)\\nWHEN (`qsec` >= 19.1849995 AND (`wt` < 3.18000007 OR ((`wt`) IS NULL))) THEN (-0.0)\\nEND + CASE\\nWHEN ((`wt` < 3.01250005 OR ((`wt`) IS NULL))) THEN (0.311573088)\\nWHEN ((`hp` < 222.5 OR ((`hp`) IS NULL)) AND `wt` >= 3.01250005) THEN (-0.392053694)\\nWHEN (`hp` >= 222.5 AND `wt` >= 3.01250005) THEN (-0.0240745768)\\nEND + CASE\\nWHEN ((`gear` < 3.5 OR ((`gear`) IS NULL))) THEN (-0.355945677)\\nWHEN ((`wt` < 3.01250005 OR ((`wt`) IS NULL)) AND `gear` >= 3.5) THEN (0.325712085)\\nWHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN (-0.0384863913)\\nEND + CASE\\nWHEN ((`gear` < 3.5 OR ((`gear`) IS NULL))) THEN (-0.309683114)\\nWHEN ((`wt` < 3.01250005 OR ((`wt`) IS NULL)) AND `gear` >= 3.5) THEN (0.283893973)\\nWHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN (-0.032039877)\\nEND + LN(0.5 / (1.0 - 0.5))))\""

# Model can be saved and re-loaded

    Code
      tidypredict_fit(pm)
    Output
      0 + case_when((qsec < 19.9549999 | is.na(qsec)) & (wt < 3.1800001 | 
          is.na(wt)) ~ 0.1384615, qsec >= 19.9549999 & (wt < 3.1800001 | 
          is.na(wt)) ~ -0.1, (hp < 290 | is.na(hp)) & wt >= 3.1800001 ~ 
          -0.1416667, hp >= 290 & wt >= 3.1800001 ~ 0.075) + case_when((qsec < 
          19.9549999 | is.na(qsec)) & (wt < 3.0125 | is.na(wt)) ~ 0.0994231, 
          qsec >= 19.9549999 & (wt < 3.0125 | is.na(wt)) ~ -0.06, (hp < 
              254.5 | is.na(hp)) & wt >= 3.0125 ~ -0.1025, hp >= 254.5 & 
              wt >= 3.0125 ~ 0.0786538) + case_when((gear < 3.5 | is.na(gear)) ~ 
          -0.0735313, (wt < 3.0125 | is.na(wt)) & gear >= 3.5 ~ 0.0720817, 
          wt >= 3.0125 & gear >= 3.5 ~ -0.0186758) + case_when((gear < 
          3.5 | is.na(gear)) ~ -0.0528506, (qsec < 19.9500008 | is.na(qsec)) & 
          gear >= 3.5 ~ 0.0427994, qsec >= 19.9500008 & gear >= 3.5 ~ 
          -0.0515982) + 0.5

