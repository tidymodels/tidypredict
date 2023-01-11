# Confirm SQL function returns a query

    Code
      rlang::expr_text(tidypredict_sql(xgb_reglinear, dbplyr::simulate_odbc())[[1]])
    Output
      [1] "\"((((0.0 + CASE\\nWHEN ((`qsec` < 19.9549999 OR (`qsec` IS NULL)) AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.138461545\\nWHEN (`qsec` >= 19.9549999 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN -0.100000009\\nWHEN ((`hp` < 290.0 OR (`hp` IS NULL)) AND `wt` >= 3.18000007) THEN -0.141666666\\nWHEN (`hp` >= 290.0 AND `wt` >= 3.18000007) THEN 0.075000003\\nEND) + CASE\\nWHEN ((`qsec` < 19.9549999 OR (`qsec` IS NULL)) AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.138461545\\nWHEN (`qsec` >= 19.9549999 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN -0.100000009\\nWHEN ((`hp` < 290.0 OR (`hp` IS NULL)) AND `wt` >= 3.18000007) THEN -0.141666666\\nWHEN (`hp` >= 290.0 AND `wt` >= 3.18000007) THEN 0.075000003\\nEND) + CASE\\nWHEN ((`qsec` < 19.9549999 OR (`qsec` IS NULL)) AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.138461545\\nWHEN (`qsec` >= 19.9549999 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN -0.100000009\\nWHEN ((`hp` < 290.0 OR (`hp` IS NULL)) AND `wt` >= 3.18000007) THEN -0.141666666\\nWHEN (`hp` >= 290.0 AND `wt` >= 3.18000007) THEN 0.075000003\\nEND) + CASE\\nWHEN ((`qsec` < 19.9549999 OR (`qsec` IS NULL)) AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.138461545\\nWHEN (`qsec` >= 19.9549999 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN -0.100000009\\nWHEN ((`hp` < 290.0 OR (`hp` IS NULL)) AND `wt` >= 3.18000007) THEN -0.141666666\\nWHEN (`hp` >= 290.0 AND `wt` >= 3.18000007) THEN 0.075000003\\nEND) + 0.5\""

---

    Code
      rlang::expr_text(tidypredict_sql(xgb_reglogistic, dbplyr::simulate_odbc())[[1]])
    Output
      [1] "\"1.0 - 1.0 / (1.0 + EXP(((((0.0 + CASE\\nWHEN (`wt` >= 3.18000007) THEN -0.436363667\\nWHEN ((`qsec` < 19.1849995 OR (`qsec` IS NULL)) AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.428571463\\nWHEN (`qsec` >= 19.1849995 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN -0.0\\nEND) + CASE\\nWHEN (`wt` >= 3.18000007) THEN -0.436363667\\nWHEN ((`qsec` < 19.1849995 OR (`qsec` IS NULL)) AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.428571463\\nWHEN (`qsec` >= 19.1849995 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN -0.0\\nEND) + CASE\\nWHEN (`wt` >= 3.18000007) THEN -0.436363667\\nWHEN ((`qsec` < 19.1849995 OR (`qsec` IS NULL)) AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.428571463\\nWHEN (`qsec` >= 19.1849995 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN -0.0\\nEND) + CASE\\nWHEN (`wt` >= 3.18000007) THEN -0.436363667\\nWHEN ((`qsec` < 19.1849995 OR (`qsec` IS NULL)) AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.428571463\\nWHEN (`qsec` >= 19.1849995 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN -0.0\\nEND) + LN(0.5 / (1.0 - 0.5))))\""
