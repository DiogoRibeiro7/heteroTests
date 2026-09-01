# Register a custom diagnostic

Adds a function to the diagnostics registry so it can be called by
`runHeteroTests`.

## Usage

``` r
registerDiagnostic(name, fun)
```

## Arguments

- name:

  Name of the diagnostic.

- fun:

  Function taking `model` and `data` arguments.

## Value

Invisibly returns `NULL`.

## Examples

``` r
custom <- function(model, data) list(statistic = 0)
registerDiagnostic("custom", custom)
```
