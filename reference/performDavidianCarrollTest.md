# Perform Davidian-Carroll test

Regresses `log(residual^2)` on fitted values using a polynomial.

## Usage

``` r
performDavidianCarrollTest(model, data = NULL, degree = 2)
```

## Details

A polynomial in the fitted values is fitted to \\\log(e^2)\\. Joint
significance of the polynomial terms is evaluated with an \\F\\
statistic.

## Arguments

- model:

  an object of class `lm`.

- data:

  optional data frame the model was fitted to. When `NULL`, the default,
  the model frame of `model` is used. The statistic is computed from the
  residuals and fitted values of `model`, so `data` does not change the
  result; it is accepted so that every test takes `(model, data, ...)`.

- degree:

  polynomial degree. Before 0.15.0 `degree` was the second argument: a
  number passed there by position is still read as `degree`, with a
  warning that shows the new call; that form stops working in 1.0.0.

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
