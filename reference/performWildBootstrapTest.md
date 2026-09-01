# Wild bootstrap heteroscedasticity test

Calibrates the Breusch-Pagan Lagrange multiplier statistic with a
*null-imposed* wild bootstrap, providing a p-value that is accurate in
small samples and under non-normal errors without relying on the
asymptotic \\\chi^2\\ approximation.

## Usage

``` r
performWildBootstrapTest(
  model,
  data,
  B = 499,
  distribution = c("rademacher", "mammen"),
  progress = interactive()
)
```

## Arguments

- model:

  A fitted [stats::lm](https://rdrr.io/r/stats/lm.html) object
  describing the mean structure whose residual variance is to be
  assessed.

- data:

  A [base::data.frame](https://rdrr.io/r/base/data.frame.html) (or
  compatible object) containing the variables referenced in `model`. The
  data must include all observations used to fit `model` and should not
  contain unresolved missing values.

- B:

  Integer number of bootstrap replications. Defaults to `499`.

- distribution:

  Multiplier distribution used for the wild perturbation. Supported
  options are "rademacher" and "mammen".

- progress:

  Logical flag controlling whether progress should be reported when
  running the bootstrap loop.

## Value

An object of class `htest` containing the observed Breusch-Pagan
statistic, bootstrap p-value, and additional details under the
`bootstrap` element.

## Details

The observed statistic is the usual \\n R^2\\ from the auxiliary
regression of the squared residuals on the regressors. The bootstrap
reference distribution, however, must be generated **under the
homoscedastic null** — otherwise it inherits the heteroscedasticity the
test is looking for and the procedure loses all power. Each bootstrap
sample is therefore built from leverage-standardised, mean-centred
residuals \\r_i = e_i/\sqrt{1-h\_{ii}}\\ (which have a common variance
under \\H_0\\) resampled i.i.d. and perturbed by a wild multiplier
\\v_i\\, so that \\y_i^\* = \hat y_i + r^\*\_i v_i\\. The statistic is
recomputed on each refit and the p-value is \\(1 + \\\\T_b^\* \ge
T\\)/(B + 1)\\. Under \\H_0\\ this controls size even for heavy-tailed
errors; under the alternative the observed statistic is extreme relative
to the homoscedastic reference, giving power.

## References

Davidson, R., & Flachaire, E. (2008). The wild bootstrap, tamed at last.
*Journal of Econometrics, 146*(1), 162–169.

Godfrey, L. G. (2006). Tests for regression models with
heteroskedasticity of unknown form. *Computational Statistics & Data
Analysis, 50*(10), 2715–2733.

Wu, C. F. J. (1986). Jackknife, bootstrap and other resampling methods
in regression analysis. *The Annals of Statistics, 14*(4), 1261–1295.

## Examples

``` r
data(mtcars)
model <- lm(mpg ~ wt + qsec, data = mtcars)
if (interactive()) {
  set.seed(123)
  performWildBootstrapTest(model, mtcars, B = 199, progress = FALSE)
}
```
