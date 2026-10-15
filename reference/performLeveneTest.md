# Perform Levene's test for equality of variances

Tests equality of variances across groups using residuals from a linear
model.

## Usage

``` r
performLeveneTest(model, data = NULL, group)
```

## Details

Absolute deviations from group means are analyzed via a one-way ANOVA.
The resulting \\F\\ statistic has \\k-1\\ and \\N-k\\ degrees of freedom
under the null of equal variances.

## Arguments

- model:

  an object of class `lm`.

- data:

  data frame used to fit `model`. When `NULL`, the default, the model
  frame of `model` is used, so `group` must then be a variable of the
  model. The model frame holds the terms of the model rather than its
  variables (`log(x)` rather than `x`), so a model that transforms a
  variable needs `data`.

- group:

  name of the grouping variable.

## Value

An object of class `htest` containing the F statistic, p-value and
degrees of freedom.

## References

Levene, H. (1960). Robust tests for equality of variances. In
*Contributions to Probability and Statistics* (pp. 278–292). Stanford
University Press.

Brown, M. B., & Forsythe, A. B. (1974). Robust tests for the equality of
variances. *Journal of the American Statistical Association*, 69(346),
364–367.
[doi:10.1080/01621459.1974.10482955](https://doi.org/10.1080/01621459.1974.10482955)

## Examples

``` r
 data(mtcars)
 mtcars$cyl <- factor(mtcars$cyl)
 m <- lm(mpg ~ wt, data = mtcars)
 performLeveneTest(m, mtcars, "cyl")
#> 
#>  Levene's test for equality of variances
#> 
#> data:  mpg ~ wt
#> F = 2.6597, df1 = 2, df2 = 29, p-value = 0.08699
#> 
```
