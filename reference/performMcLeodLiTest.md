# Perform McLeod-Li test

Applies the Ljung-Box test to squared residuals.

## Usage

``` r
performMcLeodLiTest(model, data = NULL, lags = 10)
```

## Details

Serial correlation in \\e_t^2\\ indicates conditional
heteroscedasticity. The Ljung-Box statistic is compared to a chi-square
distribution with the chosen number of lags.

## Arguments

- model:

  an object of class `lm`.

- data:

  optional data frame the model was fitted to. When `NULL`, the default,
  the model frame of `model` is used. The statistic is computed from the
  residuals of `model`, so `data` does not change the result; it is
  accepted so that every test takes `(model, data, ...)`. Before 0.15.0
  the second argument was `lags`: a number passed there by position is
  still read as `lags`, with a warning that shows the new call; that
  form stops working in 1.0.0.

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
