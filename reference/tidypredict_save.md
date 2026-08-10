# Save and re-load a parsed model

`tidypredict_save()` writes a parsed model to a YAML file, and
`tidypredict_load()` reads one back. Together they persist a model's
prediction formula without needing the original model object, or the
package that fitted it, to be available later.

Use these rather than calling
[`yaml::write_yaml()`](https://yaml.r-lib.org/reference/write_yaml.html)
directly. `yaml` defaults to 7 significant digits, which is not enough
to represent a split threshold exactly: a re-loaded tree model can then
send rows down a different branch than the model it was saved from.

## Usage

``` r
tidypredict_save(x, file)

tidypredict_load(file)
```

## Arguments

- x:

  A fitted model, or a parsed model from
  [`parse_model()`](https://tidypredict.tidymodels.org/reference/parse_model.md).
  Fitted models are parsed before being saved.

- file:

  Path to write the YAML file to, or read it from.

## Value

`tidypredict_save()` returns `x`, invisibly, so it can be used in a
pipe. `tidypredict_load()` returns a parsed model object.

## Examples

``` r
model <- lm(mpg ~ wt + cyl, data = mtcars)

path <- tempfile(fileext = ".yml")
tidypredict_save(model, path)

loaded <- tidypredict_load(path)
tidypredict_fit(loaded)
#> 39.686261480253 + (wt * -3.19097213898375) + (cyl * -1.5077949682598)
```
