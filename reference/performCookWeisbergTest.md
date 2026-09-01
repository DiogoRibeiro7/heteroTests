# Perform Cook-Weisberg test for heteroscedasticity

The Cook-Weisberg (1983) score test with the fitted values as the sole
variance regressor.

## Usage

``` r
performCookWeisbergTest(model)
```

## Details

Writing \\\hat{\sigma}^2 = \sum_i \hat{e}\_i^2 / n\\ for the
maximum-likelihood error variance, the test regresses the scaled squared
residuals \\\hat{e}\_i^2 / \hat{\sigma}^2\\ on the fitted values and
refers half the explained sum of squares to a chi-square distribution
with one degree of freedom. This is the diagnostic returned by Stata's
`estat hettest` with the `fitted` option, and it is exactly
[`performNCVTest`](https://diogoribeiro7.github.io/heteroTests/reference/performNCVTest.md)
with its default variance model.

The chi-square reference distribution follows from the null variance of
\\\hat{e}^2 / \sigma^2\\ being 2, which holds under normal errors. When
that assumption is doubtful, prefer
[`performKoenkerTest`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md).

## Arguments

- model:

  an object of class `lm`.

## Value

An object of class `htest` containing the score statistic, its single
degree of freedom and the p-value.

## Validation

Reproduces [`car::ncvTest()`](https://rdrr.io/pkg/car/man/ncvTest.html)
to within `1e-8`; see `tests/testthat/test-pass-a-reference.R`. Releases
before 0.7.0 returned \\n R^2\\ from regressing the raw squared
residuals on the fitted values, which is the studentized (Koenker)
statistic rather than the Cook-Weisberg score test.

## References

Cook, R. D., & Weisberg, S. (1983). Diagnostics for heteroscedasticity
in regression. *Biometrika*, 70(1), 1–10.
[doi:10.1093/biomet/70.1.1](https://doi.org/10.1093/biomet/70.1.1)

## See also

[`performNCVTest`](https://diogoribeiro7.github.io/heteroTests/reference/performNCVTest.md),
[`performKoenkerTest`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md),
[`performBPTest`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md)

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performCookWeisbergTest(m)
#> [INFO] Running Cook-Weisberg test
#> [INFO] Running NCV score test
#> 
#>  Cook-Weisberg test for heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 0.59099, df = 1, p-value = 0.442
#> alternative hypothesis: error variance depends on the fitted values
#> 
```
