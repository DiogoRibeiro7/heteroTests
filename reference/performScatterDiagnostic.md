# Scatter-plot diagnostics for heteroscedasticity

Computes correlations between absolute residuals and selected variables.

## Usage

``` r
performScatterDiagnostic(model, data, vars)
```

## Details

High correlations suggest increasing spread with the explanatory
variables and motivate variance-stabilizing transformations.

## Arguments

- model:

  an object of class `lm`.

- data:

  data frame used to fit `model`.

- vars:

  character vector of variable names.

## Value

A named numeric vector of correlations.

## References

Cleveland, W. S. (1979). Robust locally weighted regression and
smoothing scatterplots. *Journal of the American Statistical
Association*, 74(368), 829–836.

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performScatterDiagnostic(m, mtcars, c("wt", "qsec"))
#>         wt       qsec 
#> -0.1406436  0.3453712 
```
