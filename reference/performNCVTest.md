# Perform the non-constant variance (NCV) score test

The Cook-Weisberg score test for non-constant error variance, the
diagnostic returned by
[`car::ncvTest()`](https://rdrr.io/pkg/car/man/ncvTest.html).

## Usage

``` r
performNCVTest(model, var_formula = NULL)
```

## Details

Let \\\hat{e}\_i\\ denote the OLS residuals and \\\hat{\sigma}^2 =
\sum_i \hat{e}\_i^2 / n\\ the maximum-likelihood variance estimate. The
test regresses the scaled squared residuals \\u_i = \hat{e}\_i^2 /
\hat{\sigma}^2\\ on the variance regressors \\Z\\ and computes half the
explained sum of squares of that auxiliary regression. Under
homoscedasticity and normal errors the statistic is asymptotically
chi-square with degrees of freedom equal to the number of variance
regressors.

With `var_formula = NULL` the fitted values are the sole variance
regressor, matching the default of
[`car::ncvTest()`](https://rdrr.io/pkg/car/man/ncvTest.html) and Stata's
`estat hettest`.

Because the divisor 2 is the null variance of \\\hat{e}^2 / \sigma^2\\
under normality, the test is sensitive to non-normal errors;
[`performKoenkerTest`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md)
provides the studentized variant, which is robust to non-normal
kurtosis.

## Arguments

- model:

  an object of class `lm`.

- var_formula:

  optional one-sided formula giving the variance model, evaluated in the
  model frame of `model` (for example `~ x1 + x2`). When `NULL` the
  fitted values are used.

## Value

An object of class `htest` containing the chi-square score statistic,
its degrees of freedom and the p-value.

## Validation

Reproduces [`car::ncvTest()`](https://rdrr.io/pkg/car/man/ncvTest.html)
to within `1e-8` for both the default and the `var_formula` forms; see
`tests/testthat/test-pass-a-reference.R`. Releases before 0.7.0
regressed the *absolute* residuals on the fitted values and reported a t
statistic, which is a Glejser-type test rather than the Cook-Weisberg
score test.

## References

Cook, R. D., & Weisberg, S. (1983). Diagnostics for heteroscedasticity
in regression. *Biometrika*, 70(1), 1–10.
[doi:10.1093/biomet/70.1.1](https://doi.org/10.1093/biomet/70.1.1)

Fox, J., & Weisberg, S. (2019). *An R Companion to Applied Regression*
(3rd ed.). SAGE.

## See also

[`performCookWeisbergTest`](https://diogoribeiro7.github.io/heteroTests/reference/performCookWeisbergTest.md),
[`performKoenkerTest`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md),
[`performBPTest`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md)

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performNCVTest(m)
#> [INFO] Running NCV score test
#> 
#>  Cook-Weisberg score test for non-constant variance
#> 
#> data:  mpg ~ wt + qsec; variance model: fitted values
#> X-squared = 0.59099, df = 1, p-value = 0.442
#> alternative hypothesis: error variance depends on the variance model
#> 

 # Score test against a specific variance model rather than the fitted values
 performNCVTest(m, var_formula = ~wt)
#> [INFO] Running NCV score test
#> 
#>  Cook-Weisberg score test for non-constant variance
#> 
#> data:  mpg ~ wt + qsec; variance model: wt
#> X-squared = 0.066469, df = 1, p-value = 0.7965
#> alternative hypothesis: error variance depends on the variance model
#> 
```
