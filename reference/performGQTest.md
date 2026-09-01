# Perform Goldfeld-Quandt test for heteroscedasticity

Implements the Goldfeld-Quandt test on a fitted linear model with
directional or two-sided alternatives.

## Usage

``` r
performGQTest(model, data, order_by, fraction = 0.2,
  alternative = c("greater", "two.sided", "less"))
```

## Details

Observations are ordered by a suspected variance-driving variable. With
the split point fixed at the sample midpoint, a central fraction is
omitted and the original model is re-estimated on the lower and upper
segments. The statistic is the residual mean square of segment 2 divided
by the residual mean square of segment 1 and is therefore directional
rather than being forced above one. Split arithmetic and p-value
conventions match `lmtest::gqtest(..., point = 0.5)`.

## Arguments

- model:

  an object of class `lm`.

- data:

  data frame used to fit `model`.

- order_by:

  single column name used to order observations.

- fraction:

  fraction of observations omitted from the middle; must lie strictly
  between zero and one.

- alternative:

  alternative hypothesis: `"greater"` for increasing variance from
  segment 1 to segment 2, `"less"` for decreasing variance, or
  `"two.sided"` for either direction.

## Value

An object of class `htest` containing the directional GQ statistic,
p-value, degrees of freedom and alternative hypothesis.

## References

Goldfeld, S. M., & Quandt, R. E. (1965). Some tests for
homoscedasticity. *Journal of the American Statistical Association*,
60(310), 539–547.
[doi:10.1080/01621459.1965.10480811](https://doi.org/10.1080/01621459.1965.10480811)

Greene, W. H. (2018). *Econometric Analysis* (8th ed.). Pearson.

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performGQTest(m, mtcars, order_by = "wt")
#> [INFO] Running Goldfeld-Quandt test
#> 
#>  Goldfeld-Quandt test for heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec
#> GQ = 0.47915, df1 = 10, df2 = 9, p-value = 0.8664
#> alternative hypothesis: variance increases from segment 1 to 2
#> 
 performGQTest(m, mtcars, order_by = "wt", alternative = "two.sided")
#> [INFO] Running Goldfeld-Quandt test
#> 
#>  Goldfeld-Quandt test for heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec
#> GQ = 0.47915, df1 = 10, df2 = 9, p-value = 0.2672
#> alternative hypothesis: variance changes from segment 1 to 2
#> 
```
