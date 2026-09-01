# Reset cached validation results

Clears the internal validation cache so that subsequent checks recompute
their diagnostics. This helper is primarily used in automated tests
where deterministic behaviour is required.

## Usage

``` r
clearValidationCache()
```
