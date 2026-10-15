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
  data = NULL,
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
  containing the variables referenced in `model`. When `NULL`, the
  default, the model frame of `model` is used. It holds the terms of the
  model rather than its variables (`log(x)` rather than `x`), so a model
  that transforms a variable needs `data`, and so may a weighted fit
  whose weights are a column of the data, which the model frame holds as
  `(weights)`.

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

Weighted fits are refused, although the resampling tests accept them. On
a fit with weights \\w_i\\ the null hypothesis is that the Pearson
residuals \\\sqrt{w_i}\\ e_i\\ have constant variance, which is a
statement about the regression of \\\sqrt{w_i}\\ y_i\\ on \\\sqrt{w_i}\\
x_i\\. The quantile regressions this test fits are of \\y\\ on \\x\\,
where the errors have variance \\\sigma^2 / w_i\\ by assumption, so
their slopes differ across quantiles even when the weights are right.
Fitting them on the weighted scale does not by itself repair that: there
the conditional quantiles are the regression plus a constant, and the
constant is not a column of the weighted regressors. Adding a column of
ones would restore the location-shift form, but that version of the test
needs its own derivation and its own size study, and has neither.

## References

Koenker, R., & Bassett, G. (1982). Robust tests for heteroscedasticity
based on regression quantiles. *Econometrica, 50*(1), 43–61.

Koenker, R. (2005). *Quantile Regression*. Cambridge University Press.

## Examples

``` r
if (requireNamespace("quantreg", quietly = TRUE)) {
  # The test needs at least 40 observations, so mtcars (32) is too small.
  model <- lm(stations ~ mag + depth, data = quakes)
  performQuantileRegressionTest(model, quakes)
}
#> 
#>  Quantile regression joint test of equality of slopes
#> 
#> data:  stations ~ mag + depth
#> F = 26.909, df1 = 2, df2 = 1998, p-value = 2.938e-12
#> alternative hypothesis: at least one slope differs across quantiles
#> sample estimates:
#>          tau_0.25    tau_0.75
#> mag   38.60737066 49.11654471
#> depth  0.01311127  0.01088702
#> 
```
