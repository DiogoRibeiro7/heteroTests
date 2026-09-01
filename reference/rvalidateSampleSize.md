# Validate test-specific sample size requirements

Evaluates whether the supplied data meet the minimum observation counts
required by individual heteroscedasticity diagnostics.

## Usage

``` r
rvalidateSampleSize(test_name, model = NULL, data = NULL, groups = NULL, ...)
```

## Arguments

- test_name:

  Name of the heteroscedasticity test whose requirements are being
  validated.

- model:

  A fitted [stats::lm](https://rdrr.io/r/stats/lm.html) object
  representing the mean specification to be diagnosed.

- data:

  A [base::data.frame](https://rdrr.io/r/base/data.frame.html) (or
  object coercible to one) containing the variables referenced by
  `model`. It must include the observations used to fit `model` and will
  be checked for missing values.

- groups:

  Optional grouping vector used for per-group requirements. If omitted,
  the function attempts to derive the grouping variable from `...` when
  `group_var` is supplied.

- ...:

  Additional arguments forwarded to dynamic requirement functions (for
  example the number of `lags` in an ARCH LM test). Unused entries are
  ignored when computing static requirements.

## Value

A validation result describing whether the sample-size checks passed.
