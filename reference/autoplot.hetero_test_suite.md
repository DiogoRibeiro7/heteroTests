# Autoplot heteroscedasticity diagnostics

Generates a bar chart of diagnostic p-values, highlighting tests below
the conventional 5% threshold.

## Usage

``` r
# S3 method for class 'hetero_test_suite'
autoplot(object, ...)
```

## Arguments

- object:

  A
  [`hetero_test_suite`](https://diogoribeiro7.github.io/heteroTests/reference/hetero_test_suite.md)
  or
  [`hetero_grouped_suite`](https://diogoribeiro7.github.io/heteroTests/reference/hetero_test_suite.md).

- ...:

  Additional arguments passed to lower-level plotting helpers.

## Value

A `ggplot` object.
