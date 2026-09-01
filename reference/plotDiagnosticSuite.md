# Generate a suite of diagnostic plots

This convenience wrapper returns residual-vs-fitted and spread-level
plots to help visually assess heteroscedastic patterns.

## Usage

``` r
plotDiagnosticSuite(model)
```

## Arguments

- model:

  A fitted model of class `lm`.

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
