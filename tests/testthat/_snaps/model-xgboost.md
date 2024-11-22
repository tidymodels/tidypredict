# Confirm SQL function returns SQL query

    Code
      no_large[i]
    Output
      $reg_sqr
      <SQL> ((((0.0 + CASE
      WHEN ((`qsec` < 19.9549999 OR (`qsec` IS NULL)) AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.138461545
      WHEN (`qsec` >= 19.9549999 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN -0.100000009
      WHEN ((`hp` < 290.0 OR (`hp` IS NULL)) AND `wt` >= 3.18000007) THEN -0.141666666
      WHEN (`hp` >= 290.0 AND `wt` >= 3.18000007) THEN 0.075000003
      END) + CASE
      WHEN ((`qsec` < 19.9549999 OR (`qsec` IS NULL)) AND (`wt` < 3.01250005 OR (`wt` IS NULL))) THEN 0.0994230807
      WHEN (`qsec` >= 19.9549999 AND (`wt` < 3.01250005 OR (`wt` IS NULL))) THEN -0.0599999987
      WHEN ((`hp` < 254.5 OR (`hp` IS NULL)) AND `wt` >= 3.01250005) THEN -0.102500007
      WHEN (`hp` >= 254.5 AND `wt` >= 3.01250005) THEN 0.0786538497
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.0735312551
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.0720817298
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.0186758228
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.0528505854
      WHEN ((`qsec` < 19.9500008 OR (`qsec` IS NULL)) AND `gear` >= 3.5) THEN 0.0427994467
      WHEN (`qsec` >= 19.9500008 AND `gear` >= 3.5) THEN -0.0515981652
      END) + 0.5
      

---

    Code
      no_large[i]
    Output
      $bin_log
      <SQL> ((((0.0 + CASE
      WHEN (`wt` >= 3.18000007) THEN -0.585323453
      WHEN ((`qsec` < 18.7550011 OR (`qsec` IS NULL)) AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.327238798
      WHEN (`qsec` >= 18.7550011 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN -0.0154890772
      END) + CASE
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL))) THEN 0.244045496
      WHEN ((`hp` < 222.5 OR (`hp` IS NULL)) AND `wt` >= 3.01250005) THEN -0.464340538
      WHEN (`hp` >= 222.5 AND `wt` >= 3.01250005) THEN -0.0828785598
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.403446138
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.284780741
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.0918746367
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.341941148
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.253132522
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.0762592852
      END) + 0.5
      

---

    Code
      no_large[i]
    Output
      $reg_log
      <SQL> 1.0 - 1.0 / (1.0 + EXP(((((0.0 + CASE
      WHEN (`wt` >= 3.18000007) THEN -0.436363667
      WHEN ((`qsec` < 19.1849995 OR (`qsec` IS NULL)) AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.428571463
      WHEN (`qsec` >= 19.1849995 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN -0.0
      END) + CASE
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL))) THEN 0.311573088
      WHEN ((`hp` < 222.5 OR (`hp` IS NULL)) AND `wt` >= 3.01250005) THEN -0.392053694
      WHEN (`hp` >= 222.5 AND `wt` >= 3.01250005) THEN -0.0240745768
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.355945677
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.325712085
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.0384863913
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.309683114
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.283893973
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.032039877
      END) + LN(0.5 / (1.0 - 0.5))))
      

---

    Code
      no_large[i]
    Output
      $bin_log
      <SQL> 1.0 - 1.0 / (1.0 + EXP(((((0.0 + CASE
      WHEN (`wt` >= 3.18000007) THEN -0.436363667
      WHEN ((`qsec` < 19.1849995 OR (`qsec` IS NULL)) AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.428571463
      WHEN (`qsec` >= 19.1849995 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN -0.0
      END) + CASE
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL))) THEN 0.311573088
      WHEN ((`hp` < 222.5 OR (`hp` IS NULL)) AND `wt` >= 3.01250005) THEN -0.392053694
      WHEN (`hp` >= 222.5 AND `wt` >= 3.01250005) THEN -0.0240745768
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.355945677
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.325712085
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.0384863913
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.309683114
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.283893973
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.032039877
      END) + LN(0.5 / (1.0 - 0.5))))
      

---

    Code
      no_large[i]
    Output
      $reg_log_base
      <SQL> 1.0 - 1.0 / (1.0 + EXP(((((0.0 + CASE
      WHEN (`wt` >= 3.18000007) THEN -0.354515553
      WHEN ((`qsec` < 18.7550011 OR (`qsec` IS NULL)) AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.505574405
      WHEN (`qsec` >= 18.7550011 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.131739721
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.346223652
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.436893433
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.033788003
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.302007079
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.355824143
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.028081635
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.269374669
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.304945588
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.0233395267
      END) + LN(0.40625 / (1.0 - 0.40625))))
      

---

    Code
      no_large[i]
    Output
      $bin_log_base
      <SQL> 1.0 - 1.0 / (1.0 + EXP(((((0.0 + CASE
      WHEN (`wt` >= 3.18000007) THEN -0.354515553
      WHEN ((`qsec` < 18.7550011 OR (`qsec` IS NULL)) AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.505574405
      WHEN (`qsec` >= 18.7550011 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.131739721
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.346223652
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.436893433
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.033788003
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.302007079
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.355824143
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.028081635
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.269374669
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.304945588
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.0233395267
      END) + LN(0.40625 / (1.0 - 0.40625))))
      

---

    Code
      no_large[i]
    Output
      $reg_log_deep
      <SQL> 1.0 - 1.0 / (1.0 + EXP(((((0.0 + CASE
      WHEN (`wt` >= 3.18000007) THEN -0.436363667
      WHEN ((`qsec` < 19.1849995 OR (`qsec` IS NULL)) AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.428571463
      WHEN (`qsec` >= 19.1849995 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN -0.0
      END) + CASE
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL))) THEN 0.311573088
      WHEN ((`hp` < 222.5 OR (`hp` IS NULL)) AND `wt` >= 3.01250005) THEN -0.392053694
      WHEN (`hp` >= 222.5 AND `wt` >= 3.01250005) THEN -0.0240745768
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.355945677
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.325712085
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.0384863913
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.309683114
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.283893973
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.032039877
      END) + LN(0.5 / (1.0 - 0.5))))
      

---

    Code
      no_large[i]
    Output
      $bin_log_deep
      <SQL> 1.0 - 1.0 / (1.0 + EXP(((((0.0 + CASE
      WHEN (`wt` >= 3.18000007) THEN -0.436363667
      WHEN ((`qsec` < 19.1849995 OR (`qsec` IS NULL)) AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.428571463
      WHEN (`qsec` >= 19.1849995 AND (`wt` < 3.18000007 OR (`wt` IS NULL))) THEN -0.0
      END) + CASE
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL))) THEN 0.311573088
      WHEN ((`hp` < 222.5 OR (`hp` IS NULL)) AND `wt` >= 3.01250005) THEN -0.392053694
      WHEN (`hp` >= 222.5 AND `wt` >= 3.01250005) THEN -0.0240745768
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.355945677
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.325712085
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.0384863913
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.309683114
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.283893973
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.032039877
      END) + LN(0.5 / (1.0 - 0.5))))
      

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

