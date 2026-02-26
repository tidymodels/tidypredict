# alert triggered with threshold = 0

    Code
      cat(t$message)
    Output
      tidypredict test results
      Difference threshold: 0
      
      Fitted records above the threshold: 13
      
      Fit max  difference:
      Lower max difference:
      Upper max difference:3.5527136788005e-15

# alert with intervals

    Code
      cat(t$message)
    Output
      tidypredict test results
      Difference threshold: 0
      
      Fitted records above the threshold: 16
      Lower interval records above the threshold: 15
      Upper interval records above the threshold: 11
      
      Fit max  difference:7.105427357601e-15
      Lower max difference:7.105427357601e-15
      Upper max difference:3.5527136788005e-15

# xgboost alert branch

    Code
      cat(t$message)
    Output
      tidypredict test results
      Difference threshold: 0
      
      Fitted records above the threshold: 18
      
      Max difference: 8.40138671520663e-07

# glmnet alert branch (mocked)

    Code
      cat(t$message)
    Output
      tidypredict test results
      Difference threshold: 0
      
      Fitted records above the threshold: 2368
      
      Max difference: -969.880981369456

# lightgbm alert branch (mocked)

    Code
      cat(t$message)
    Output
      tidypredict test results
      Difference threshold: 0
      
      Fitted records above the threshold: 32
      
      Max difference: 983.714176060289

# catboost alert branch (mocked)

    Code
      cat(t$message)
    Output
      tidypredict test results
      Difference threshold: 0
      
      Fitted records above the threshold: 32
      
      Max difference: 982.713951655463

