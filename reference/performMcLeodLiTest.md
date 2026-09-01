# Perform McLeod-Li test

Applies the Ljung-Box test to squared residuals.

## Usage

``` r
performMcLeodLiTest(model, lags = 10)
```

## Details

Serial correlation in \\e_t^2\\ indicates conditional
heteroscedasticity. The Ljung-Box statistic is compared to a chi-square
distribution with the chosen number of lags.

## Arguments

- model:

  an object of class `lm`.

- lags:

  number of lags for the Ljung-Box test.

## Value

An object of class `htest` containing the test statistic, p-value and
degrees of freedom.

## References

McLeod, A. I., & Li, W. K. (1983). Diagnostic checking ARMA time series
models using squared-residual autocorrelations. *Journal of Time Series
Analysis*, 4(4), 269–273.

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performMcLeodLiTest(m, lags = 10)
#> [INFO] Running McLeod-Li test
#> 
#>  McLeod-Li test for heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 16.291, df = 10, p-value = 0.09161
#> 
```
