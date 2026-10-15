# Perform Hartley's Fmax test

Compares the maximum and minimum group residual variances using
Hartley's maximum F-ratio distribution.

## Usage

``` r
performHartleyFmaxTest(model, data, group)
```

## Arguments

- model:

  A fitted `lm` object.

- data:

  Data frame used to fit `model`.

- group:

  Name of the grouping variable.

## Details

The statistic is \\F\_{max}=\max_j s_j^2/\min_j s_j^2\\. Under normality
and equal group sizes its null distribution is the maximum F-ratio
distribution for \\k\\ independent mean squares with common degrees of
freedom, evaluated with
[`SuppDists::pmaxFratio()`](https://rdrr.io/pkg/SuppDists/man/maxFratio.html).
If group sizes differ, the function warns and rounds the mean group
size, then subtracts one, to obtain an approximate common integer
degrees of freedom.

## Value

An object of class `htest` containing the Fmax statistic, number of
groups, reference degrees of freedom and p-value.

## References

Hartley, H. O. (1950). The maximum F-ratio as a short-cut test for
heterogeneity of variance. *Biometrika*, 37(3/4), 308–312.

## Examples

``` r
 set.seed(1701)
 n <- 20
 d <- data.frame(g = factor(rep(letters[1:3], each = n)), x = rnorm(3 * n))
 d$y <- 1 + d$x + rnorm(3 * n)
 m <- lm(y ~ x, data = d)
 performHartleyFmaxTest(m, d, "g")
#> [INFO] Running Hartley's Fmax test
#> 
#>  Hartley's Fmax test
#> 
#> data:  y ~ x
#> F = 2.0941, groups = 3, df = 19, p-value = 0.2552
#> alternative hypothesis: at least one group variance differs
#> 
```
