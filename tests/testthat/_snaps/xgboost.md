# Confirm SQL function returns SQL query

    Code
      xgb_sql[i]
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
      xgb_sql[i]
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
      xgb_sql[i]
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
      xgb_sql[i]
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
      xgb_sql[i]
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
      xgb_sql[i]
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
      xgb_sql[i]
    Output
      $reg_log_large
      <SQL> 1.0 - 1.0 / (1.0 + EXP(((((((((((((((((((((((((((((((((((((((((((0.0 + CASE
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
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.275577009
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.252453178
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.0266750772
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.248323873
      WHEN ((`qsec` < 17.6599998 OR (`qsec` IS NULL)) AND `gear` >= 3.5) THEN 0.261978835
      WHEN (`qsec` >= 17.6599998 AND `gear` >= 3.5) THEN -0.00959526002
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.225384533
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.218285918
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.0373593047
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.205454513
      WHEN ((`qsec` < 18.7550011 OR (`qsec` IS NULL)) AND `gear` >= 3.5) THEN 0.196076646
      WHEN (`qsec` >= 18.7550011 AND `gear` >= 3.5) THEN -0.0544253439
      END) + CASE
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL))) THEN 0.149246693
      WHEN ((`qsec` < 17.4099998 OR (`qsec` IS NULL)) AND `wt` >= 3.01250005) THEN 0.0354709327
      WHEN (`qsec` >= 17.4099998 AND `wt` >= 3.01250005) THEN -0.226075932
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.184417158
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.176768288
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.0237750355
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.168993726
      WHEN ((`qsec` < 18.6049995 OR (`qsec` IS NULL)) AND `gear` >= 3.5) THEN 0.155569643
      WHEN (`qsec` >= 18.6049995 AND `gear` >= 3.5) THEN -0.0325752236
      END) + CASE
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL))) THEN 0.119126029
      WHEN (`wt` >= 3.01250005) THEN -0.105012275
      END) + CASE
      WHEN ((`qsec` < 17.1749992 OR (`qsec` IS NULL))) THEN 0.117254682
      WHEN (`qsec` >= 17.1749992) THEN -0.0994235724
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.097100094
      WHEN (`wt` >= 3.18000007) THEN -0.10567718
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0824323222
      WHEN (`wt` >= 3.18000007) THEN -0.091120176
      END) + CASE
      WHEN ((`qsec` < 17.5100002 OR (`qsec` IS NULL))) THEN 0.0854752287
      WHEN (`qsec` >= 17.5100002) THEN -0.0764453933
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0749477893
      WHEN (`wt` >= 3.18000007) THEN -0.0799863264
      END) + CASE
      WHEN ((`qsec` < 17.7099991 OR (`qsec` IS NULL))) THEN 0.0728750378
      WHEN (`qsec` >= 17.7099991) THEN -0.0646049976
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0682478622
      WHEN (`wt` >= 3.18000007) THEN -0.0711427554
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0579533465
      WHEN (`wt` >= 3.18000007) THEN -0.0613371208
      END) + CASE
      WHEN ((`qsec` < 18.1499996 OR (`qsec` IS NULL))) THEN 0.0595484748
      WHEN (`qsec` >= 18.1499996) THEN -0.0546668135
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0535288528
      WHEN (`wt` >= 3.18000007) THEN -0.0558333211
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0454574414
      WHEN (`wt` >= 3.18000007) THEN -0.048143398
      END) + CASE
      WHEN ((`qsec` < 18.5600014 OR (`qsec` IS NULL))) THEN 0.0422042683
      WHEN (`qsec` >= 18.5600014) THEN -0.0454404354
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0420555808
      WHEN (`wt` >= 3.18000007) THEN -0.0449385941
      END) + CASE
      WHEN ((`qsec` < 18.5600014 OR (`qsec` IS NULL))) THEN 0.0393446013
      WHEN (`qsec` >= 18.5600014) THEN -0.0425945036
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0391179025
      WHEN (`wt` >= 3.18000007) THEN -0.0420661867
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0304145869
      WHEN (`qsec` >= 18.4099998) THEN -0.031833414
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0362136625
      WHEN (`wt` >= 3.18000007) THEN -0.038949281
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0295153651
      WHEN (`qsec` >= 18.4099998) THEN -0.0307046026
      END) + CASE
      WHEN ((`drat` < 3.80999994 OR (`drat` IS NULL))) THEN -0.0306891855
      WHEN (`drat` >= 3.80999994) THEN 0.0288283136
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0271221269
      WHEN (`qsec` >= 18.4099998) THEN -0.0281750448
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0228891298
      WHEN (`qsec` >= 18.4099998) THEN -0.0238814205
      END) + CASE
      WHEN ((`drat` < 3.80999994 OR (`drat` IS NULL))) THEN -0.0296511576
      WHEN (`drat` >= 3.80999994) THEN 0.0280048084
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0214707125
      WHEN (`qsec` >= 18.4099998) THEN -0.0224219449
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0181306079
      WHEN (`qsec` >= 18.4099998) THEN -0.0190209728
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0379650332
      WHEN (`wt` >= 3.18000007) THEN -0.0395050682
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0194106717
      WHEN (`qsec` >= 18.4099998) THEN -0.0202215631
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0164139606
      WHEN (`qsec` >= 18.4099998) THEN -0.0171694476
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.013879573
      WHEN (`qsec` >= 18.4099998) THEN -0.0145772668
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0117362784
      WHEN (`qsec` >= 18.4099998) THEN -0.0123759825
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0388614088
      WHEN (`wt` >= 3.18000007) THEN -0.0400568396
      END) + LN(0.5 / (1.0 - 0.5))))
      

---

    Code
      xgb_sql[i]
    Output
      $bin_log_large
      <SQL> 1.0 - 1.0 / (1.0 + EXP(((((((((((((((((((((((((((((((((((((((((((0.0 + CASE
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
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.275577009
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.252453178
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.0266750772
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.248323873
      WHEN ((`qsec` < 17.6599998 OR (`qsec` IS NULL)) AND `gear` >= 3.5) THEN 0.261978835
      WHEN (`qsec` >= 17.6599998 AND `gear` >= 3.5) THEN -0.00959526002
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.225384533
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.218285918
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.0373593047
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.205454513
      WHEN ((`qsec` < 18.7550011 OR (`qsec` IS NULL)) AND `gear` >= 3.5) THEN 0.196076646
      WHEN (`qsec` >= 18.7550011 AND `gear` >= 3.5) THEN -0.0544253439
      END) + CASE
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL))) THEN 0.149246693
      WHEN ((`qsec` < 17.4099998 OR (`qsec` IS NULL)) AND `wt` >= 3.01250005) THEN 0.0354709327
      WHEN (`qsec` >= 17.4099998 AND `wt` >= 3.01250005) THEN -0.226075932
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.184417158
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL)) AND `gear` >= 3.5) THEN 0.176768288
      WHEN (`wt` >= 3.01250005 AND `gear` >= 3.5) THEN -0.0237750355
      END) + CASE
      WHEN ((`gear` < 3.5 OR (`gear` IS NULL))) THEN -0.168993726
      WHEN ((`qsec` < 18.6049995 OR (`qsec` IS NULL)) AND `gear` >= 3.5) THEN 0.155569643
      WHEN (`qsec` >= 18.6049995 AND `gear` >= 3.5) THEN -0.0325752236
      END) + CASE
      WHEN ((`wt` < 3.01250005 OR (`wt` IS NULL))) THEN 0.119126029
      WHEN (`wt` >= 3.01250005) THEN -0.105012275
      END) + CASE
      WHEN ((`qsec` < 17.1749992 OR (`qsec` IS NULL))) THEN 0.117254682
      WHEN (`qsec` >= 17.1749992) THEN -0.0994235724
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.097100094
      WHEN (`wt` >= 3.18000007) THEN -0.10567718
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0824323222
      WHEN (`wt` >= 3.18000007) THEN -0.091120176
      END) + CASE
      WHEN ((`qsec` < 17.5100002 OR (`qsec` IS NULL))) THEN 0.0854752287
      WHEN (`qsec` >= 17.5100002) THEN -0.0764453933
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0749477893
      WHEN (`wt` >= 3.18000007) THEN -0.0799863264
      END) + CASE
      WHEN ((`qsec` < 17.7099991 OR (`qsec` IS NULL))) THEN 0.0728750378
      WHEN (`qsec` >= 17.7099991) THEN -0.0646049976
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0682478622
      WHEN (`wt` >= 3.18000007) THEN -0.0711427554
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0579533465
      WHEN (`wt` >= 3.18000007) THEN -0.0613371208
      END) + CASE
      WHEN ((`qsec` < 18.1499996 OR (`qsec` IS NULL))) THEN 0.0595484748
      WHEN (`qsec` >= 18.1499996) THEN -0.0546668135
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0535288528
      WHEN (`wt` >= 3.18000007) THEN -0.0558333211
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0454574414
      WHEN (`wt` >= 3.18000007) THEN -0.048143398
      END) + CASE
      WHEN ((`qsec` < 18.5600014 OR (`qsec` IS NULL))) THEN 0.0422042683
      WHEN (`qsec` >= 18.5600014) THEN -0.0454404354
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0420555808
      WHEN (`wt` >= 3.18000007) THEN -0.0449385941
      END) + CASE
      WHEN ((`qsec` < 18.5600014 OR (`qsec` IS NULL))) THEN 0.0393446013
      WHEN (`qsec` >= 18.5600014) THEN -0.0425945036
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0391179025
      WHEN (`wt` >= 3.18000007) THEN -0.0420661867
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0304145869
      WHEN (`qsec` >= 18.4099998) THEN -0.031833414
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0362136625
      WHEN (`wt` >= 3.18000007) THEN -0.038949281
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0295153651
      WHEN (`qsec` >= 18.4099998) THEN -0.0307046026
      END) + CASE
      WHEN ((`drat` < 3.80999994 OR (`drat` IS NULL))) THEN -0.0306891855
      WHEN (`drat` >= 3.80999994) THEN 0.0288283136
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0271221269
      WHEN (`qsec` >= 18.4099998) THEN -0.0281750448
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0228891298
      WHEN (`qsec` >= 18.4099998) THEN -0.0238814205
      END) + CASE
      WHEN ((`drat` < 3.80999994 OR (`drat` IS NULL))) THEN -0.0296511576
      WHEN (`drat` >= 3.80999994) THEN 0.0280048084
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0214707125
      WHEN (`qsec` >= 18.4099998) THEN -0.0224219449
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0181306079
      WHEN (`qsec` >= 18.4099998) THEN -0.0190209728
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0379650332
      WHEN (`wt` >= 3.18000007) THEN -0.0395050682
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0194106717
      WHEN (`qsec` >= 18.4099998) THEN -0.0202215631
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0164139606
      WHEN (`qsec` >= 18.4099998) THEN -0.0171694476
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.013879573
      WHEN (`qsec` >= 18.4099998) THEN -0.0145772668
      END) + CASE
      WHEN ((`qsec` < 18.4099998 OR (`qsec` IS NULL))) THEN 0.0117362784
      WHEN (`qsec` >= 18.4099998) THEN -0.0123759825
      END) + CASE
      WHEN ((`wt` < 3.18000007 OR (`wt` IS NULL))) THEN 0.0388614088
      WHEN (`wt` >= 3.18000007) THEN -0.0400568396
      END) + LN(0.5 / (1.0 - 0.5))))
      

---

    Code
      xgb_sql[i]
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
      xgb_sql[i]
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

