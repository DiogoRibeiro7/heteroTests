# Perform Spread-Level test

Tests the slope of `log(|residuals|)` on `log(fitted)`.

## Usage

``` r
performSpreadLevelTest(model, data = NULL)
```

## Details

A slope near 0 indicates constant variance. A significant slope suggests
a power transformation may stabilize the spread.

## Arguments

- model:

  an object of class `lm`.

- data:

  optional data frame the model was fitted to. When `NULL`, the default,
  the model frame of `model` is used. The statistic is computed from the
  residuals and fitted values of `model`, so `data` does not change the
  result; it is accepted so that every test takes `(model, data, ...)`.

## Value

An object of class `htest`.

## References

Cleveland, W. S. (1979). Robust locally weighted regression and
smoothing scatterplots. *Journal of the American Statistical
Association*, 74(368), 829–836.

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performSpreadLevelTest(m)
#> 
#>  Spread-Level test
#> 
#> data:  mpg ~ wt + qsec
#> t = 0.43832, df = 30, p-value = 0.6643
#> 
```
