## Comments

This is a patch release. It fixes a regression with xgboost 3.4.0, which
canonicalises `booster = "dart"` to `"gbtree"` in the saved model JSON. This
package inferred dropout models from that name, so predictions for dropout
models diverged from `predict()`. The dropout weights are now detected directly.
This was reported to us by the xgboost maintainers from their reverse dependency
checks.

## R CMD check results

* 0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 3 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
