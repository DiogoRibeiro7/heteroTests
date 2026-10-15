# Perform O'Brien test for equality of variances

Implements O'Brien's observation-level transformed-residual test for
equality of group variances.

## Usage

``` r
performOBrienTest(model, data, group)
```

## Arguments

- model:

  A fitted `lm` object.

- data:

  Data frame used to fit `model` and containing the grouping variable.

- group:

  Grouping variable name.

## Details

For each residual within a group, O'Brien's transformation combines its
squared deviation from the group mean with the group's sample variance
and sample size. A one-way ANOVA of those transformed observations
yields an approximate \\F\_{k-1,N-k}\\ test. Every group must contain at
least three observations.

## Value

An object of class `htest` containing the F statistic, numerator and
denominator degrees of freedom, and p-value.

## References

O'Brien, R. G. (1981). A simple test for variance effects in
experimental designs. *Psychological Bulletin*, 89(3), 570–574.

## Examples

``` r
 set.seed(1702)
 n <- 25
 d <- data.frame(g = factor(rep(letters[1:3], each = n)), x = rnorm(3 * n))
 d$y <- 2 + d$x + rnorm(3 * n, sd = rep(c(1, 1, 1.8), each = n))
 m <- lm(y ~ x, data = d)
 performOBrienTest(m, d, "g")
#> 
#>  O'Brien test for equality of variances
#> 
#> data:  y ~ x
#> F = 4.1739, df1 = 2, df2 = 72, p-value = 0.01927
#> alternative hypothesis: at least one group variance differs
#> 
```
