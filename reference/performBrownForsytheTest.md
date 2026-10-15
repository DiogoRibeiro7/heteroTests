# Perform Brown-Forsythe test for equality of variances

Brown-Forsythe test using medians instead of means.

## Usage

``` r
performBrownForsytheTest(model, data, group)
```

## Details

The absolute deviations from the group medians are compared across
groups. The resulting statistic is an \\F\\ ratio with \\k-1\\ and
\\N-k\\ degrees of freedom.

## Arguments

- model:

  an object of class `lm`.

- data:

  data frame used to fit `model`.

- group:

  name of the grouping variable.

## Value

An object of class `htest` containing the F statistic, p-value and
degrees of freedom.

## References

Brown, M. B., & Forsythe, A. B. (1974). Robust tests for the equality of
variances. *Journal of the American Statistical Association*, 69(346),
364–367.

## Examples

``` r
 data(mtcars)
 mtcars$cyl <- factor(mtcars$cyl)
 m <- lm(mpg ~ wt, data = mtcars)
 performBrownForsytheTest(m, mtcars, "cyl")
#> 
#>  Brown-Forsythe test for equality of variances
#> 
#> data:  mpg ~ wt
#> F = 2.474, df1 = 2, df2 = 29, p-value = 0.1019
#> 
```
