# Deprecated HC covariance pseudo-test

`performHCCovarianceTest()` previously transformed HC0–HC4 leverage
adjustments into an auxiliary-regression statistic and assigned a
chi-squared reference distribution. HC0–HC4 are
heteroscedasticity-consistent covariance estimators; the cited
literature does not justify that construction as a heteroscedasticity
hypothesis test.

## Usage

``` r
performHCCovarianceTest(
  model,
  data,
  type = c("HC0", "HC1", "HC2", "HC3", "HC4")
)
```

## Arguments

- model:

  A fitted regression model retained for backward-compatible argument
  matching.

- data:

  The model data retained for backward-compatible argument matching.

- type:

  Character string naming an HC covariance estimator. Retained only for
  backward-compatible argument matching.

## Value

This function does not return a test result. It signals an error with
migration guidance.

## Details

The function is retained temporarily so existing callers receive an
explicit migration error rather than silently obtaining invalid
inferential output. For testing whether error variance depends on
regressors, use
[`performBPTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md),
[`performKoenkerTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md),
or
[`performWhiteTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTest.md).
For heteroscedasticity-robust coefficient inference, use a covariance
estimator such as
[`sandwich::vcovHC()`](https://zeileis.codeberg.page/sandwich/reference/vcovHC.html)
directly.

## References

MacKinnon, J. G., & White, H. (1985). Some heteroskedasticity-consistent
covariance matrix estimators with improved finite sample properties.
*Journal of Econometrics, 29*(3), 305–325.

Long, J. S., & Ervin, L. H. (2000). Using heteroscedasticity consistent
standard errors in the linear regression model. *The American
Statistician, 54*(3), 217–224.
