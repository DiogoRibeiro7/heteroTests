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

## See also

[`listDiagnostics`](https://diogoribeiro7.github.io/heteroTests/reference/listDiagnostics.md)
for the names already registered.

## Examples

``` r
custom <- function(model, data) list(statistic = 0)
registerDiagnostic("custom", custom)
```
