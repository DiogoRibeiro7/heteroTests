# Safe variance calculation

Computes the variance of `x` while guarding against near-zero values
that could lead to division by zero in subsequent calculations.

## Usage

``` r
safe_var(x)
```

## Arguments

- x:

  Numeric vector.

## Value

A variance value with a minimum of `.Machine$double.eps`.
