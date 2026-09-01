# Summarize diagnostics

Summarize diagnostic test results.

## Usage

``` r
# S3 method for class 'HeteroDiagnostic'
summary(object, tests = c("white", "breusch_pagan"), ...)
```

## Arguments

- object:

  A `HeteroDiagnostic` object.

- tests:

  Character vector naming the diagnostics to summarise.

- ...:

  Further arguments passed to the individual tests.

## Value

Named numeric vector of test statistics.
