# Safely fit a linear model

Wraps [`stats::lm()`](https://rdrr.io/r/stats/lm.html) in a `tryCatch`
block that logs the error via
[`ht_log()`](https://diogoribeiro7.github.io/heteroTests/reference/ht_log.md)
and optionally attempts recovery (e.g., dropping rows with missing
values) before rethrowing. This helper is used internally by diagnostic
tests so failures are easier to debug.

## Usage

``` r
safe_lm(formula, data, ..., .recover = TRUE)
```

## Arguments

- formula:

  Model formula.

- data:

  Data frame to evaluate the formula in.

- ...:

  Additional arguments passed to
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html).

- .recover:

  Logical flag controlling whether the helper should attempt automatic
  recovery strategies (e.g., removing problematic rows) before
  rethrowing the error.

## Value

A fitted model object.
