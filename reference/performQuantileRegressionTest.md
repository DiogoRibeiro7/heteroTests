# Quantile regression heteroscedasticity test

Tests equality of regression slopes across two or more conditional
quantiles using the joint Wald-type test implemented by
[`quantreg::anova.rqs()`](https://rdrr.io/pkg/quantreg/man/anova.rq.html).
The procedure accounts for dependence among quantile-specific estimates
from the same sample rather than treating their covariance matrices as
independent.

## Usage

``` r
performQuantileRegressionTest(
  model,
  data,
  taus = c(0.25, 0.75),
  se_type = c("nid", "ker"),
  iid = TRUE
)
```

## Arguments

- model:

  A fitted [stats::lm](https://rdrr.io/r/stats/lm.html) object
  describing the mean structure whose conditional quantile slopes are to
  be compared.

- data:

  A [base::data.frame](https://rdrr.io/r/base/data.frame.html)
  containing the variables referenced in `model`.

- taus:

  Numeric vector containing at least two distinct quantiles strictly
  between zero and one.

- se_type:

  Standard-error method used by
  [`quantreg::anova.rqs()`](https://rdrr.io/pkg/quantreg/man/anova.rq.html);
  supported values are `"nid"` and `"ker"`.

- iid:

  Logical indicating whether identical conditional densities are assumed
  when computing the joint test.

## Value

An `htest` object containing the F-like joint statistic, numerator and
denominator degrees of freedom, p-value, fitted quantiles, and
quantile-specific slope estimates.

## Details

Under a pure location-shift model with homoskedastic errors, regression
slopes are equal across quantiles. Rejection therefore provides evidence
against that location-shift/homoskedastic specification. The result
should not be interpreted as a universal test for every possible form of
heteroscedasticity.

## References

Koenker, R., & Bassett, G. (1982). Robust tests for heteroscedasticity
based on regression quantiles. *Econometrica, 50*(1), 43–61.

Koenker, R. (2005). *Quantile Regression*. Cambridge University Press.

## Examples

``` r
if (requireNamespace("quantreg", quietly = TRUE)) {
  # The test needs at least 40 observations, so mtcars (32) is too small.
  data(boston_housing, package = "heteroTests")
  model <- lm(medv ~ lstat + rm, data = boston_housing)
  performQuantileRegressionTest(model, boston_housing)
}
#> Warning: Residual outliers detected at rows 369 (|z| > 5). Inspect leverage before running Quantile regression.
#> Warning: 3 non-positive fis
#> Warning: 3 non-positive fis
#> 
#>  Quantile regression joint test of equality of slopes
#> 
#> data:  medv ~ lstat + rm
#> F = 35.595, df1 = 2, df2 = 1010, p-value = 1.11e-15
#> alternative hypothesis: at least one slope differs across quantiles
#> sample estimates:
#>         tau_0.25   tau_0.75
#> lstat -0.7240311 -0.3129101
#> rm     3.0542943  8.3192288
#> 
```
