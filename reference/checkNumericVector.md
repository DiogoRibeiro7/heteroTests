# Validate numeric vector

Ensures `x` is numeric and non-empty. Used internally by helpers that
expect numeric input.

## Usage

``` r
checkNumericVector(x, name = "x")
```

## Arguments

- x:

  Object to check.

- name:

  Optional variable name for error messages.

## Value

Invisible `x` if valid, otherwise an error is thrown.
