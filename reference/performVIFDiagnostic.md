# Variance inflation factors

Compute variance inflation factors for assessing multicollinearity. Each
VIF is the corresponding diagonal element of the inverse correlation
matrix of the design, so the computation works for models containing
factors (each contrast column receives its own VIF).

## Usage

``` r
performVIFDiagnostic(model)
```

## Arguments

- model:

  A fitted `lm` model.

## Value

A named numeric vector of VIF values, one per design-matrix column
(excluding the intercept). Perfectly collinear columns are reported as
`Inf`.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
performVIFDiagnostic(m)
#>       wt     qsec 
#> 1.031487 1.031487 
```
