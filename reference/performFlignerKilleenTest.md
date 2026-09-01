# Perform Fligner-Killeen test for homogeneity of variances

Non-parametric test based on ranks.

## Usage

``` r
performFlignerKilleenTest(model, data, group)
```

## Details

Absolute residuals are ranked after adjusting for group medians. The
statistic approximates a chi-square distribution with \\k-1\\ degrees of
freedom.

## Arguments

- model:

  an object of class `lm`.

- data:

  data frame used to fit `model`.

- group:

  name of the grouping variable.

## Value

An object of class `htest` containing the chi-squared statistic, p-value
and degrees of freedom.

## References

Fligner, M. A., & Killeen, T. J. (1976). Distribution-free two-sample
tests for scale. *Journal of the American Statistical Association*,
71(353), 210–213.

## Examples

``` r
 data(mtcars)
 mtcars$cyl <- factor(mtcars$cyl)
 m <- lm(mpg ~ wt, data = mtcars)
 performFlignerKilleenTest(m, mtcars, "cyl")
#> 
#>  Fligner-Killeen test for homogeneity of variances
#> 
#> data:  mpg ~ wt
#> X-squared = 4.5639, = 2, p-value = 0.1021
#> 
```
