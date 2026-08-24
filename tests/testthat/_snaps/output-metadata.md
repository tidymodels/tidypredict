# models with no metadata method say so

    Code
      tidypredict_output_type(structure(list(), class = "made_up_model"))
    Condition
      Error in `tidypredict_output_type()`:
      ! `tidypredict_output_type()` is not available for models of class <made_up_model>.

# an rpart parsed model cannot say which mode it came from

    Code
      tidypredict_output_type(pm)
    Condition
      Error in `tidypredict_output_type()`:
      ! `tidypredict_output_type()` is not available for models of class <parsed_model>.

