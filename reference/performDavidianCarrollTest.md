# Perform Davidian-Carroll test

Regresses `log(residual^2)` on fitted values using a polynomial.

## Usage

``` r
performDavidianCarrollTest(model, degree = 2)
```

## Details

A polynomial in the fitted values is fitted to \\\log(e^2)\\. Joint
significance of the polynomial terms is evaluated with an \\F\\
statistic.

## Arguments

- model:

  an object of class `lm`.

- degree:

  polynomial degree.

## Value

An object of class `htest`.

## References

Davidian, M., & Carroll, R. J. (1987). Variance function estimation.
*Journal of the American Statistical Association*, 82(400), 1079–1091.

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performDavidianCarrollTest(m)
#> 
#>  Davidian-Carroll test
#> 
#> data:  mpg ~ wt + qsec
#> F = 1.7459, df1 = 2, df2 = 29, p-value = 0.1923
#> 
```
