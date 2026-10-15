# Run enhanced heteroscedasticity diagnostics

Executes one or more robust diagnostics with automatic enhancements for
small samples and optional studentisation.

## Usage

``` r
runAdvancedDiagnostics(
  model,
  data,
  tests = "all",
  auto_enhance = TRUE,
  bootstrap_B = 999,
  parallel = FALSE,
  ci_level = 0.95
)

rrunAdvancedDiagnostics(
  model,
  data,
  tests = "all",
  auto_enhance = TRUE,
  bootstrap_B = 999,
  parallel = FALSE,
  ci_level = 0.95
)
```

## Arguments

- model:

  A fitted [stats::lm](https://rdrr.io/r/stats/lm.html) object
  representing the mean specification to be diagnosed.

- data:

  A [base::data.frame](https://rdrr.io/r/base/data.frame.html) (or
  object coercible to one) containing the variables referenced by
  `model`. It must include the observations used to fit `model` and will
  be checked for missing values.

- tests:

  Character vector of tests to run. Use "all" (default) to run the White
  and Breusch-Pagan diagnostics, or supply a subset such as
  `c("white", "bp")`.

- auto_enhance:

  Logical, enable automatic bootstrap for small samples (n \< 50) and
  studentization for Breusch-Pagan.

- bootstrap_B:

  Number of bootstrap replications used when automatic enhancement
  triggers bootstrap. Defaults to 999, the default of every resampling
  test in the package since 0.15.0; it was 500 before.

- parallel:

  Logical, allow parallel bootstrap evaluation when the `parallel`
  package is available. A seeded call gives the same result each time;
  see *Parallel replicates* in
  [`bootstrapTestStatistic()`](https://diogoribeiro7.github.io/heteroTests/reference/rbootstrap_test_statistic.md).

- ci_level:

  Confidence level for reported intervals.

## Value

A list with the executed results and metadata describing the
enhancements that were applied.

## Deprecated name

`rrunAdvancedDiagnostics()` is deprecated since 0.15.0: use
`runAdvancedDiagnostics()`, the same function under the package's naming
scheme. The old name still works, returns what it returned and warns; it
leaves the exports in 1.0.0.
