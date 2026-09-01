# Validate data argument

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
