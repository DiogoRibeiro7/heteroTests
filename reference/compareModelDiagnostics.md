# Compare diagnostics across models

Runs `runDiagnostics` for multiple models and collects test statistics.
Diagnostics that cannot be computed (for example, due to validation
failures) emit warnings and fill the corresponding entries with
`NA_real_`. The original error messages are preserved on the returned
data frame via the `diagnostic_errors` attribute.

## Usage

``` r
compareModelDiagnostics(models, data = NULL, tests = c("white", "breusch_pagan"))
```

## Arguments

- models:

  List of fitted models or formulas.

- data:

  Optional data frame when formulas are supplied.

- tests:

  Diagnostics to run.

## Value

A data frame of statistics, one row per model. Failed diagnostics are
represented by `NA_real_` entries and the underlying error messages can
be retrieved from the `diagnostic_errors` attribute.

## See also

[`analyzeMLResiduals`](https://diogoribeiro7.github.io/heteroTests/reference/analyzeMLResiduals.md)

## Examples

``` r
 data(mtcars)
 m1 <- lm(mpg ~ wt + qsec, mtcars)
 m2 <- lm(mpg ~ wt + hp, mtcars)
 compareModelDiagnostics(list(m1, m2))
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.8225 df = 5 p = 0.0373
#> [INFO] Running Breusch-Pagan test
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.5431 df = 5 p = 0.2569
#> [INFO] Running Breusch-Pagan test
#>            white breusch_pagan
#> Model1 11.822480      3.134790
#> Model2  6.543086      1.026766
```
