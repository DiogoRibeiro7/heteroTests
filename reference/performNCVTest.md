# Perform the non-constant variance (NCV) score test (deprecated)

The Cook-Weisberg score test for non-constant error variance, the
diagnostic returned by
[`car::ncvTest()`](https://rdrr.io/pkg/car/man/ncvTest.html).

## Usage

``` r
performNCVTest(model, data = NULL, var_formula = NULL)
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

## Deprecated

`performNCVTest()` is deprecated since 0.14.0: use
[`performCookWeisbergTest`](https://diogoribeiro7.github.io/heteroTests/reference/performCookWeisbergTest.md),
which gained `var_formula` and is the one entry point for the
Cook-Weisberg score test. With a variance formula,
`performNCVTest(model, var_formula)` returns what
`performCookWeisbergTest(model, var_formula)` returns; without one the
two return the same statistic, degrees of freedom and p-value, and
[`performCookWeisbergTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performCookWeisbergTest.md)
labels the result as the Cook-Weisberg test. The old name still works,
returns what it returned and warns.

## Weighted fits

On a fit with weights the test is computed from the Pearson residuals
\\\sqrt{w_i}\\ e_i\\, so it tests whether the weights are adequate; see
the section on weighted fits in
[`performKoenkerTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md),
which also covers weights estimated by
[`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md).

## Arguments

- model:

  an object of class `lm`.

- data:

  optional data frame the model was fitted to, in which `var_formula` is
  evaluated, as in
  [`performCookWeisbergTest`](https://diogoribeiro7.github.io/heteroTests/reference/performCookWeisbergTest.md).
  When `NULL`, the default, the model frame of `model` is used.

- var_formula:

  optional one-sided formula giving the variance model, evaluated in
  `data` (for example `~ x1 + x2`). When `NULL` the fitted values are
  used. Before 0.15.0 `var_formula` was the second argument: a formula
  passed there by position is still read as `var_formula`, with a
  warning; that form stops working in 1.0.0.

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
 # Deprecated: use performCookWeisbergTest(m) and
 # performCookWeisbergTest(m, var_formula = ~wt)
 suppressWarnings(performNCVTest(m, var_formula = ~wt))
#> [INFO] Running NCV score test
#> 
#>  Cook-Weisberg score test for non-constant variance
#> 
#> data:  mpg ~ wt + qsec; variance model: wt
#> X-squared = 0.066469, df = 1, p-value = 0.7965
#> alternative hypothesis: error variance depends on the variance model
#> 
```
