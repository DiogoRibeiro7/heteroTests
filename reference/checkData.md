# Validate data argument (deprecated)

Ensures that `data` is a data.frame. Used internally for input
validation across the package.

## Usage

``` r
checkData(data)
```

## Arguments

- data:

  Object to check.

## Value

Invisible `data` if valid, otherwise an error is thrown.

## Details

Deprecated since 0.14.0: an internal helper of the package, which leaves
the exports in 1.0.0. It still works and warns.
