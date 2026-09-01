# Process validation helper outputs

Standardises how validation helper results are handled across the
diagnostic implementations.

## Usage

``` r
rprocessValidationResult(result)
```

## Arguments

- result:

  A validation result (or compatible list) produced by the helper
  routines.

## Value

Invisibly returns `result` after issuing warnings or errors.
