# Bartlett compatibility alias

Preserves the historical modified-Bartlett entry point while delegating
to the canonical Bartlett implementation.

## Usage

``` r
performModifiedBartlettTest(model, data, group)
```

## Arguments

- model:

  A fitted `lm` object.

- data:

  Data frame used to fit `model`.

- group:

  Grouping variable name.

## Details

The pre-0.7.1 implementation manually evaluated the usual finite-sample
correction that is already part of the standard Bartlett chi-squared
statistic. It did not define a distinct test. This function now
delegates to
[`performBartlettTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBartlettTest.md)
so the two entry points cannot drift while compatibility is retained.

## Value

An object of class `htest` with statistic, degrees of freedom and
p-value identical to
[`performBartlettTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBartlettTest.md).

## References

Bartlett, M. S. (1937). Properties of sufficiency and statistical tests.
*Proceedings of the Royal Society of London A*, 160(901), 268–282.

## Examples

``` r
 data(mtcars)
 mtcars$cyl <- factor(mtcars$cyl)
 m <- lm(mpg ~ wt, data = mtcars)
 performModifiedBartlettTest(m, mtcars, "cyl")
#> 
#>  Bartlett's test for equality of variances
#> 
#> data:  mpg ~ wt
#> X-squared = 4.1572, = 2, p-value = 0.1251
#> 
```
