# errors on unsupported models

    Code
      tidypredict_fit(rbf)
    Condition
      Error in `parse_model()`:
      ! Only linear kernlab SVM models are supported.
      i The model must use the "vanilladot" (linear) kernel.

---

    Code
      tidypredict_fit(multi)
    Condition
      Error in `parse_model()`:
      ! Only binary classification kernlab SVM models are supported.
      i This model has 3 classes.

---

    Code
      tidypredict_fit(noprob)
    Condition
      Error in `parse_model()`:
      ! Classification kernlab SVM models require a probability model.
      i Refit with `prob.model = TRUE`.

