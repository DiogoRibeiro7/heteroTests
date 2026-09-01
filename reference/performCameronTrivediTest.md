# Perform Cameron-Trivedi decomposition test

Regresses squared residuals on fitted values and their squares.

## Usage

``` r
performCameronTrivediTest(model)
```

## Details

Linear and quadratic terms are jointly tested in an auxiliary regression
of \\e^2\\ on \\\hat{y}\\ and \\\hat{y}^2\\. The resulting \\F\\
statistic assesses both linear and nonlinear forms of
heteroscedasticity.

## Arguments

- model:

  an object of class `lm`.

## Value

An object of class `htest` containing the F statistic, p-value and
degrees of freedom.

## References

Cameron, A. C., & Trivedi, P. K. (1990). Regression-based tests for
heteroskedasticity in the linear model. *Journal of Econometrics*,
47(1), 267–288.

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performCameronTrivediTest(m)
#> [INFO] Running Cameron-Trivedi test
#> 
#>  Cameron-Trivedi decomposition test
#> 
#> data:  mpg ~ wt + qsec
#> F = 4.0999, df1 = 2, df2 = 29, p-value = 0.02704
#> 
```
