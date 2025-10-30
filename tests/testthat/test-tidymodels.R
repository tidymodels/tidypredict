test_that("works with parsnip model specification", {
  etitanic_fac <- dplyr::mutate(earth::etitanic, survived = as.factor(survived))

  # Classification
  model <- parsnip::fit(
    parsnip::set_engine(parsnip::mars(mode = "classification"), "earth"),
    survived ~ age + sibsp,
    data = etitanic_fac
  )

  expect_snapshot(
    tidypredict_test(
      model,
      df = etitanic_fac
    )
  )

  # Regression
  model <- parsnip::fit(
    parsnip::set_engine(parsnip::mars(mode = "regression"), "earth"),
    survived ~ age + sibsp,
    data = etitanic
  )

  expect_snapshot(
    tidypredict_test(
      model,
      df = etitanic_fac
    )
  )
})
