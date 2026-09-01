# Run time-series heteroscedasticity tests

Convenience wrapper for Engle's ARCH LM and McLeod-Li tests.

## Usage

``` r
runTimeSeriesTests(model, lags = 1, tests = c("arch_lm", "mcleod_li"))
```

## Arguments

- model:

  A fitted `lm` object.

- lags:

  Number of lags for both tests.

- tests:

  Character vector of test names.

## Value

A named list of `htest` objects.

## See also

[`runPanelTests`](https://diogoribeiro7.github.io/heteroTests/reference/runPanelTests.md)

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, mtcars)
 runTimeSeriesTests(m, lags = 2)
#> [INFO] Running ARCH LM test
#> [INFO] Running McLeod-Li test
#> $arch_lm
#> 
#>  Engle's ARCH LM test
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 3.4573, df = 2, p-value = 0.1775
#> 
#> 
#> $mcleod_li
#> 
#>  McLeod-Li test for heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 4.8058, df = 2, p-value = 0.09046
#> 
#> 
```
