# Perform Bartlett's test for equality of variances

Uses `bartlett.test` on model residuals grouped by a factor.

## Usage

``` r
performBartlettTest(model, data, group)
```

## Details

The test statistic compares the pooled variance to the individual group
variances. It is computed as \\\chi^2 = (N - k) \ln S_p^2 - \sum
(n_i - 1) \ln s_i^2\\, where \\S_p^2\\ is the pooled variance and
\\s_i^2\\ the group variances. Under the null it approximates a
chi-square distribution with \\k-1\\ degrees of freedom.

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

Bartlett, M. S. (1937). Properties of sufficiency and statistical tests.
*Proceedings of the Royal Society of London*, 160(901), 268–282.
[doi:10.1098/rspa.1937.0109](https://doi.org/10.1098/rspa.1937.0109)

Hartley, H. O. (1950). The maximum F-ratio as a short-cut test for
heterogeneity of variance. *Biometrika*, 37(3/4), 308–312.
[doi:10.2307/2332383](https://doi.org/10.2307/2332383)

## Examples

``` r
 data(mtcars)
 mtcars$cyl <- factor(mtcars$cyl)
 m <- lm(mpg ~ wt, data = mtcars)
 performBartlettTest(m, mtcars, "cyl")
#> 
#>  Bartlett's test for equality of variances
#> 
#> data:  mpg ~ wt
#> X-squared = 4.1572, = 2, p-value = 0.1251
#> 
```
