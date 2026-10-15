# Wild bootstrap heteroscedasticity test

Calibrates the Breusch-Pagan Lagrange multiplier statistic with a
*null-imposed* wild bootstrap, providing a p-value that is accurate in
small samples and under non-normal errors without relying on the
asymptotic \\\chi^2\\ approximation.

## Usage

``` r
performWildBootstrapTest(
  model,
  data = NULL,
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
  contain unresolved missing values. When `NULL`, the default, the model
  frame of `model` is used. It holds the terms of the model rather than
  its variables (`log(x)` rather than `x`), so a model that transforms a
  variable needs `data`, and so may a weighted fit whose weights are a
  column of the data, which the model frame holds as `(weights)`.

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

Each replicate is refitted on the design matrix of the fit, with its
offset, whether that was given as an argument or written in the formula.
The formula is not evaluated again, so a transformed regressor such as
`log(x)`, or a transformed response, is refitted on the scale of the
fit.

## Weighted fits

A fit with weights \\w_i\\ is ordinary least squares on \\\sqrt{w_i}\\
y_i\\ and \\\sqrt{w_i}\\ x_i\\, whose errors have constant variance
under the null hypothesis, and the bootstrap works in that regression.
The statistic is computed from its residuals, the Pearson residuals
\\\sqrt{w_i}\\ e_i\\, with the auxiliary regressors on their original
scale; the residuals that are resampled are the Pearson residuals
divided by \\\sqrt{1 - h\_{ii}}\\, with the leverages of that
regression, and centred; and each replicate refits it. With equal
weights the result is that of the unweighted fit.

The weights are held fixed in every replicate. When they were estimated
from the same data, as by
[`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md),
the replicates leave that estimation out, and the test inherits the
error of the tests that take the weights as known. In
`inst/validation/weighted-fits-size.R`, on data whose standard deviation
is \\\exp(0.3 x)\\, with the variance function correctly specified,
normal errors and `B = 499`, its rejection rate at the 5% level was
0.086 with 150 observations and 0.105 with 600. In the same design with
the true weights it was 0.054 and 0.048, and with the weights estimated
again in each replicate, which this function does not do, 0.060 and
0.051.
[`performVarianceFormTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performVarianceFormTest.md)
allows for the estimation.

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
