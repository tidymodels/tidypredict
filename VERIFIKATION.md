# Verifikation: Issue #318 (tidymodels/tidypredict)

Diese Datei dient einer **unabhängigen KI** zur Prüfung, ob die Änderungen
korrekt und vollständig sind. Alle Änderungen liegen im Branch `issue-318-docs`
im Repo `/tmp/tidypredict`.

## Kontext

- **Issue:** https://github.com/tidymodels/tidypredict/issues/318
  "Documentation gaps: `\value`, `@keywords internal` typo, stale model lists"
- **Basis-HEAD:** `3626fdf` (Document the edge-case test battery in CONTRIBUTING.md (#384))
- **Wichtig:** Ein Teil des Issues war im HEAD **bereits gefixt** (README
  "Supported models" mit Link zu models.html, `\value` auf `tidypredict_sql`/
  `tidypredict_sql_interval`). Diese Punkte wurden NICHT erneut angefasst.

## Wie prüfen

```bash
cd /tmp/tidypredict
git diff 3626fdf..HEAD --stat        # Übersicht der geänderten Dateien
git diff 3626fdf..HEAD               # Vollständiger Diff
```

## Checkliste (Punkt für Punkt aus dem Issue)

### Punkt 1: `@keywords internal` mit 2 Leerzeichen (R/tree.R)
**Soll:** Keine Zeile `#'  @keywords internal` (2 Leerzeichen) mehr in R/tree.R.
**Prüfen:**
```bash
grep -nP "^#'  @keywords internal" R/tree.R   # muss leer sein
grep -nP "^#' @keywords internal" R/tree.R    # 5 Treffer (Zeilen 51,121,207,241,363)
```
**Erwartung:** 3 Stellen (121, 207, 241) von 2 auf 1 Leerzeichen korrigiert.
Die man/-Dateien `generate_tree_node.Rd`, `path_formula.Rd`, `path_formulas.Rd`
enthalten jetzt `\keyword{internal}` statt des Literal-Strings `@keywords internal`.

### Punkt 2: Fehlende `\value` auf 9 Topics
**Soll:** Alle 9 Topics haben `\value` in ihrer man/-Datei.
**Prüfen:**
```bash
for f in tidypredict_fit tidypredict_interval tidypredict_test \
         tidypredict_to_column tidypredict_sql tidypredict_sql_interval \
         acceptable_formula as_parsed_model tidy.pm_regression; do
  grep -q '\\value' man/$f.Rd && echo "$f: OK" || echo "$f: FEHLT"
done
```
**Erwartung:** Alle 9 "OK". (`tidypredict_sql`/`tidypredict_sql_interval`
hatten schon `\value` – unverändert.)

### Punkt 3: `man/tidypredict_test.Rd` model-Param-Enumeration
**Soll:** Der Satz "It currently supports lm(), glm() and randomForest() models"
ist entfernt (NAMESPACE hat 40+ Methoden).
**Prüfen:**
```bash
grep -n "currently supports" man/tidypredict_test.Rd R/tidypredict_test.R  # muss leer sein
```

### Punkt 4: `man/parse_model.Rd` Model types vervollständigt
**Soll:** Die 5 fehlenden Typen sind ergänzt: `pm_bart`, `pm_naive_bayes`,
`pm_nnet`, `pm_multiclass_regression`, `pm_nullmodel_classification`.
Plus Hinweis "This list is not exhaustive".
**Prüfen:**
```bash
grep -c "pm_bart\|pm_naive_bayes\|pm_nnet\|pm_multiclass_regression\|pm_nullmodel_classification" man/parse_model.Rd  # = 5
grep -n "not exhaustive" man/parse_model.Rd
```

### Punkt 5: README "Supported models" + "Elastic net"
**Soll:** README.md/README.Rmd verweisen bereits auf models.html (im HEAD gefixt,
nicht angefasst). "Elastic net" kommt in README.Rmd nicht mehr vor.
**Prüfen:**
```bash
grep -n "Elastic net" README.Rmd README.md   # muss leer sein
grep -c "articles/models.html" README.md README.Rmd  # je 1
```

### Punkt 6: Fehlende `@examples`
**Soll:** `as_parsed_model`, `tidypredict_to_column`, `tidy.pm_regression`
haben jetzt `@examples` in R-Quelle + man/-Datei.
**Prüfen:**
```bash
for f in as_parsed_model tidypredict_to_column tidy.pm_regression; do
  echo "$f: $(grep -c '\\examples' man/$f.Rd)"
done
```
**Erwartung:** je 1.

### Punkt 7: `\dontrun{}` → `@examplesIf`
**Soll:** `set_catboost_categories` nutzt `@examplesIf rlang::is_installed("catboost")`
statt `\dontrun{}`.
**Prüfen:**
```bash
grep -n "dontrun" man/set_catboost_categories.Rd R/model-catboost.R  # muss leer sein
grep -n "examplesIf" man/set_catboost_categories.Rd R/model-catboost.R
```

### Punkt 8: cubist aus Version-3-Liste entfernt
**Soll:** In `man/parse_model.Rd` (Version 3) steht cubist NICHT mehr in der
nested-case_when-Liste, weil `build_tree_formula.pm_tree_cubist` auf den
flachen Rule-Averaging-Builder routet. cubist bleibt in der `pm_tree`-Liste.
**Prüfen:**
```bash
sed -n '38,40p' man/parse_model.Rd   # Version 3: ...catboost, partykit) OHNE cubist
sed -n '55,58p' man/parse_model.Rd   # pm_tree: ...randomForest, cubist) MIT cubist
```

### Punkt 9: Stray `NULL` nach `"_PACKAGE"`
**Soll:** `R/tidypredict-package.R` hat kein `NULL` mehr direkt nach `"_PACKAGE"`.
**Prüfen:**
```bash
sed -n '22,26p' R/tidypredict-package.R   # "_PACKAGE" gefolgt von Leerzeile + utils::globalVariables
```

## Zusätzliche Verifikation

### R-Syntax aller geänderten Dateien
```bash
for f in R/acceptable.R R/as-parsed-model.R R/model-catboost.R R/parsemodel.R \
         R/predict-column.R R/predict-fit.R R/predict-interval.R R/tidymodels.R \
         R/tidypredict-package.R R/tidypredict_test.R R/tree.R; do
  Rscript -e "invisible(parse('$f'))" && echo "OK: $f" || echo "FEHLER: $f"
done
```
**Erwartung:** Alle "OK".

### roxygen2-Regenerierung (optional, braucht R + roxygen2)
```bash
cd /tmp/tidypredict && Rscript -e 'roxygen2::roxygenise()'
```
**Hinweis:** roxygen2 8.1.0 meldet S3-Warnungen für `build_tree_formula.*`
(kein `@export`). Diese sind **vorbestehend** und nicht durch diese Änderung
verursacht. `DESCRIPTION`/`NAMESPACE` wurden nach der Regenerierung auf den
Originalstand zurückgesetzt (nur die man/-Dateien + R-Quellen sind Teil des PR).

## Nicht angefasste Punkte (bewusst)

- **README "Supported models"** – war im HEAD schon aktualisiert (Link zu models.html).
- **`_pkgdown.yml` reference-Sektion** – Design-Frage, im Issue als "rather than
  defects" markiert, nicht Teil der konkreten Fix-Liste.
- **Dot-prefixed exports (`@family orbital helpers`)** – Design-Frage, nicht Teil
  der konkreten Fix-Liste.
- **`tidypredict_sql`/`tidypredict_sql_interval` `\value`** – waren schon da.

## Geänderte Dateien (Übersicht)

**R-Quellen (11):** acceptable.R, as-parsed-model.R, model-catboost.R,
parsemodel.R, predict-column.R, predict-fit.R, predict-interval.R, tidymodels.R,
tidypredict-package.R, tidypredict_test.R, tree.R

**man/-Dateien (12):** acceptable_formula.Rd, as_parsed_model.Rd,
generate_tree_node.Rd, parse_model.Rd, path_formula.Rd, path_formulas.Rd,
set_catboost_categories.Rd, tidy.pm_regression.Rd, tidypredict_fit.Rd,
tidypredict_interval.Rd, tidypredict_test.Rd, tidypredict_to_column.Rd
