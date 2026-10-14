# Generate a suite of diagnostic plots

This convenience wrapper returns residual-vs-fitted and spread-level
plots to help visually assess heteroscedastic patterns.

## Usage

``` r
plotDiagnosticSuite(model, enhanced = FALSE)
```

## Arguments

- model:

  A fitted model of class `lm`.

- enhanced:

  Logical, passed to
  [`plotResidualsFitted()`](https://diogoribeiro7.github.io/heteroTests/reference/plotResidualsFitted.md).
  `TRUE` returns what the deprecated
  [`plotDiagnosticSuiteEnhanced()`](https://diogoribeiro7.github.io/heteroTests/reference/plotDiagnosticSuiteEnhanced.md)
  returned.

## Value

A list with elements `residuals_fitted` and `spread_level`, each a
`ggplot` object.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
plots <- plotDiagnosticSuite(m)
plots$residuals_fitted
#> `geom_smooth()` using formula = 'y ~ x'
```
