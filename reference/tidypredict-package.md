# tidypredict: Run Predictions Inside the Database

It parses a fitted 'R' model object, and returns a formula in 'Tidy
Eval' code that calculates the predictions. It works with several
databases back-ends because it leverages 'dplyr' and 'dbplyr' for the
final 'SQL' translation of the algorithm. Dozens of model classes are
supported; see the "Supported models" article at
<https://tidypredict.tidymodels.org/articles/models.html> for the
current list.

## See also

Useful links:

- <https://tidypredict.tidymodels.org>

- <https://github.com/tidymodels/tidypredict>

- Report bugs at <https://github.com/tidymodels/tidypredict/issues>

## Author

**Maintainer**: Emil Hvitfeldt <emil.hvitfeldt@posit.co>
([ORCID](https://orcid.org/0000-0002-0679-1945))

Authors:

- Emil Hvitfeldt <emil.hvitfeldt@posit.co>
  ([ORCID](https://orcid.org/0000-0002-0679-1945))

- Edgar Ruiz <edgar@posit.co>

- Max Kuhn <max@posit.co>
  ([ORCID](https://orcid.org/0000-0003-2402-136X))

Other contributors:

- Posit Software, PBC ([ROR](https://ror.org/03wc8by49)) \[copyright
  holder, funder\]
