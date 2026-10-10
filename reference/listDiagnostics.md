# List the registered diagnostics

Returns the names
[`runHeteroTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md)
accepts in `tests`: the diagnostics the package registers and any added
with
[`registerDiagnostic()`](https://diogoribeiro7.github.io/heteroTests/reference/registerDiagnostic.md).

## Usage

``` r
listDiagnostics()
```

## Value

A character vector of the registered names, sorted.

## Details

`"box_m"` is registered for
[`runMultivariateTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runMultivariateTests.md),
which calls it with data and a grouping factor rather than with a model.

## See also

[`registerDiagnostic()`](https://diogoribeiro7.github.io/heteroTests/reference/registerDiagnostic.md),
[`runHeteroTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md)

## Examples

``` r
listDiagnostics()
#>  [1] "box_m"               "breusch_pagan"       "cook_weisberg"      
#>  [4] "high_dimensional"    "koenker"             "ncv"                
#>  [7] "quantile_regression" "rank_permutation"    "spatial_hetero"     
#> [10] "spread_level"        "student_bp"          "szroeter"           
#> [13] "variance_form"       "white"               "white_bootstrap"    
#> [16] "wild_bootstrap"     
```
