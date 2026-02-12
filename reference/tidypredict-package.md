# tidypredict: Run Predictions Inside the Database

It parses a fitted 'R' model object, and returns a formula in 'Tidy
Eval' code that calculates the predictions. It works with several
databases back-ends because it leverages 'dplyr' and 'dbplyr' for the
final 'SQL' translation of the algorithm. It currently supports lm(),
glm(), randomForest(), ranger(), earth(), xgb.Booster.complete(),
lgb.Booster(), catboost.Model(), cubist(), and ctree() models.

## See also

Useful links:

- <https://tidypredict.tidymodels.org>

- <https://github.com/tidymodels/tidypredict>

- Report bugs at <https://github.com/tidymodels/tidypredict/issues>

## Author

**Maintainer**: Emil Hvitfeldt <emil.hvitfeldt@posit.co>

Authors:

- Edgar Ruiz <edgar@posit.co>

- Max Kuhn <max@posit.co>
