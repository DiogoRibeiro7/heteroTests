# Summarize diagnostics

Summarize diagnostic test results.

## Usage

``` r
# S3 method for class 'HeteroDiagnostic'
summary(object, tests = c("white", "koenker"), ...)
```

## Arguments

- object:

  A `HeteroDiagnostic` object.

- tests:

  Character vector naming the diagnostics to summarise. Defaults to the
  White and Koenker tests, the default of
  [`runHeteroTests`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md);
  it was the White and the classical Breusch-Pagan tests up to 0.14.0.

- ...:

  Further arguments passed to the individual tests.

## Value

Named numeric vector of test statistics.
