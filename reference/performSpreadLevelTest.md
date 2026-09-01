# Perform Spread-Level test

Tests the slope of `log(|residuals|)` on `log(fitted)`.

## Usage

``` r
performSpreadLevelTest(model)
```

## Details

A slope near 0 indicates constant variance. A significant slope suggests
a power transformation may stabilize the spread.

## Arguments

- model:

  an object of class `lm`.

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
#> t = 0.43832, = 30, p-value = 0.6643
#> 
```
