# Validate regression model inputs

Ensures that a supplied regression model satisfies the basic assumptions
required by the heteroscedasticity testing infrastructure. The checks
include class validation, successful fitting, availability of finite
residuals, and minimum sample size requirements.

## Usage

``` r
rvalidateModelInputs(model, test_name, min_obs = 10)
```

## Arguments

- model:

  A fitted model object produced by
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) or
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html).

- test_name:

  A scalar character identifier used in error messages to reference the
  calling test.

- min_obs:

  Minimum number of observations required for the calling procedure.
  Defaults to `10`.

## Value

Invisibly returns `model` when validation passes.

## Examples

``` r
mod <- stats::lm(mpg ~ wt, data = mtcars)
heteroTests:::rvalidateModelInputs(mod, test_name = "Demo Test")
```
