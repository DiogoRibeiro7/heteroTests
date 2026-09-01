# Perform Engle's ARCH LM test

Regresses squared residuals on their lags to detect ARCH effects.

## Usage

``` r
performArchLMTest(model, lags = 1)
```

## Details

The statistic is \\n R^2\\ from an auxiliary regression of \\e_t^2\\ on
its lagged values, where \\R^2\\ is the coefficient of determination.
Under the null of no ARCH effects it follows a chi-square distribution
with degrees of freedom equal to the number of lags.

## Arguments

- model:

  an object of class `lm`.

- lags:

  number of lags to include.

## Value

An object of class `htest` containing the test statistic, p-value and
degrees of freedom.

## References

Engle, R. F. (1982). Autoregressive conditional heteroskedasticity with
estimates of the variance of United Kingdom inflation. *Econometrica*,
50(4), 987–1007. [doi:10.2307/1912773](https://doi.org/10.2307/1912773)

Hamilton, J. D. (1994). *Time Series Analysis*. Princeton University
Press.

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performArchLMTest(m, lags = 2)
#> [INFO] Running ARCH LM test
#> 
#>  Engle's ARCH LM test
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 3.4573, df = 2, p-value = 0.1775
#> 
```
