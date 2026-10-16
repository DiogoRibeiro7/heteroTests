# Perform Glejser test for heteroscedasticity

Regresses absolute residuals on a transformation of a suspected
variable. By default, `robust = TRUE` since 0.15.0, the absolute
residuals are first corrected for the asymmetry of the errors: this is
the version of Im (2000) and of Machado and Santos Silva (2000), which
keeps its level when the errors are asymmetric, from about 150
observations on (see the section on asymmetric errors for smaller
samples). `robust = FALSE` gives Glejser's (1969) statistic, the default
up to 0.14.0.

## Usage

``` r
performGlejserTest(model, data = NULL, variable,
                   transformation = c("abs", "sqrt", "inverse", "inverse_sqrt"),
                   robust = TRUE)
```

## Details

A variety of transformations of the explanatory variable (e.g. absolute
value, square root) can reveal a relationship between scale and the
covariate. Significance of the slope parameter is assessed with a t
test. For a weighted fit the residuals are the Pearson residuals.

Glejser's statistic, `robust = FALSE`, has its nominal level only when
positive and negative errors are equally likely. The corrected statistic
of the default does not need that, and is the one to use when the
residuals may be skewed or their shape is unknown.

Either form conditions on one variable and one transformation. Scanning
several transformations and reporting only the smallest p-value
invalidates the nominal level. Simulated size and power of both
statistics are recorded in `inst/validation/glejser-skewed-errors.csv`;
those of Glejser's statistic in `inst/validation/pass-a-size-power.csv`,
and those of the default in `inst/validation/full-sweep-size-power.csv`
and `inst/validation/weighted-fits-size.csv`.

## Arguments

- model:

  an object of class `lm`.

- data:

  data frame used to fit `model`. When `NULL`, the default, the model
  frame of `model` is used, so `variable` must then be a variable of the
  model. The model frame holds the terms of the model rather than its
  variables (`log(x)` rather than `x`), so a model that transforms a
  variable needs `data`, as does a model with an
  [`offset()`](https://rdrr.io/r/stats/offset.html) term.

- variable:

  name of the suspected variable.

- transformation:

  transformation applied to `variable`.

- robust:

  logical. When `TRUE`, the default since 0.15.0, the absolute residuals
  are corrected for the asymmetry of the errors before the auxiliary
  regression. When `FALSE` the statistic is Glejser's (1969), which has
  its nominal level when positive and negative errors are equally
  likely, as they are for symmetric errors; it is what the function
  returned by default up to 0.14.0. See the sections on asymmetric
  errors and on the default.

## Value

An object of class `htest` containing the t statistic, p-value and
degrees of freedom. The `method` element says which of the two
statistics was computed.

## Asymmetric errors

Glejser's statistic takes the absolute residuals for the absolute
errors. The two differ by the estimation error of the mean equation,
\$\$\|\hat{e}\_i\| \approx \|e_i\| - \mathrm{sign}(e_i)\\ x_i^\top
(\hat{\beta} - \beta),\$\$ and the second term averages out only when
positive and negative errors are equally likely (Godfrey 1996). When
they are not, and the variable tested is correlated with the regressors,
the test does not in general have its nominal level, and a larger sample
does not help. In the designs of
`inst/validation/glejser-skewed-errors.R` Glejser's statistic,
`robust = FALSE`, rejects a true null hypothesis about 13% of the time
at the 5% level under exponential errors and about 17% under lognormal
errors, at every sample size from 50 to 1000. For other error
distributions the rate can also fall below the nominal level.

`robust = TRUE` replaces the regressand \\\|\hat{e}\_i\|\\ by
\$\$\|\hat{e}\_i\| - \hat{m}\\ \hat{e}\_i, \qquad \hat{m} = \frac{1}{n}
\sum\_{i=1}^{n} \mathrm{sign}(\hat{e}\_i),\$\$ where \\\hat{m}\\ is the
proportion of positive residuals less the proportion of negative ones.
This is the correction of Im (2000). The regressand of Machado and
Santos Silva (2000), \\\hat{e}\_i\\\[I(\hat{e}\_i \ge 0) -
\hat{\eta}\]\\ with \\\hat{\eta}\\ the proportion of non-negative
residuals, is half of it when no residual is exactly zero, and the two
then give the same test. A residual that is zero up to rounding error is
counted as zero, so that the statistic does not depend on the sign of a
rounding error.

The auxiliary regression is otherwise the same. Both references state
the statistic as \\n R^2\\ of that regression, referred to a chi-squared
distribution with one degree of freedom. The t statistic reported here
is the signed form of it, \\n R^2 = n t^2 / (t^2 + n - 2)\\, and its
p-value is taken from the t distribution with \\n - 2\\ degrees of
freedom, as for Glejser's statistic. The two p-values agree as the
sample grows.

In the same study, with 5000 replications in each cell, the corrected
statistic is within simulation error of the 5% level from 150
observations on: all 27 of its cells with a true null hypothesis and 150
observations or more are inside the 99% interval around 5%, from 4.21%
to 5.79%, at 4.4% to 5.7%. At 50 observations it rejects 5.7% to 6.6% of
the time under skewed errors, and four of those six cells are above that
interval, at 5.80% to 6.58%. Under symmetric errors \\\hat{m}\\ is close
to zero and the two statistics differ little: the corrected one rejects
0.5 to 0.7 percentage points less often at 50 observations and gives up
one or two points of power there. For a weighted fit the correction is
applied to the Pearson residuals.

## The default

Up to 0.14.0 the default was `robust = FALSE`. Since 0.15.0 it is
`robust = TRUE`, so a call that does not give `robust` returns a
different statistic and p-value from those it returned before, and its
`method` ends in "(robust to asymmetric errors)". `robust = FALSE`
returns what such a call returned up to 0.14.0.

Glejser's statistic needs positive and negative errors to be equally
likely, which is seldom known: in the study above it rejects a true null
hypothesis 8.8% to 17.7% of the time at the 5% level when the errors are
skewed, at every sample size from 50 to 1000, where the corrected
statistic rejects 4.6% to 5.7% of the time from 150 observations on. The
change costs some power when the errors are symmetric, 1.1 and 1.9
percentage points at 50 observations under Gaussian errors in that
study, and the corrected statistic is above the 5% level at 50
observations under skewed errors, as above.

## References

Glejser, H. (1969). A new test for heteroskedasticity. *Journal of the
American Statistical Association*, 64(325), 316–323.

Godfrey, L. G. (1996). Some results on the Glejser and Koenker tests for
heteroskedasticity. *Journal of Econometrics*, 72(1–2), 275–299.
[doi:10.1016/0304-4076(94)01722-0](https://doi.org/10.1016/0304-4076%2894%2901722-0)

Im, K. S. (2000). Robustifying Glejser test of heteroskedasticity.
*Journal of Econometrics*, 97(1), 179–188.
[doi:10.1016/S0304-4076(99)00061-5](https://doi.org/10.1016/S0304-4076%2899%2900061-5)

Machado, J. A. F. and Santos Silva, J. M. C. (2000). Glejser's test
revisited. *Journal of Econometrics*, 97(1), 189–202.
[doi:10.1016/S0304-4076(00)00016-6](https://doi.org/10.1016/S0304-4076%2800%2900016-6)

## See also

[`performKoenkerTest`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md)
for a test on the squared residuals that needs neither normal nor
symmetric errors;
[`performParkTest`](https://diogoribeiro7.github.io/heteroTests/reference/performParkTest.md)
and
[`performHarveyTest`](https://diogoribeiro7.github.io/heteroTests/reference/performHarveyTest.md)
for related parametric tests.

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 # The default, which keeps its level under asymmetric errors from about
 # 150 observations
 performGlejserTest(m, mtcars, "wt")
#> [INFO] Running Glejser test
#> 
#>  Glejser test for heteroscedasticity (robust to asymmetric errors)
#> 
#> data:  mpg ~ wt + qsec
#> t = -0.39528, df = 30, p-value = 0.6954
#> 

 # Glejser's (1969) statistic, the default up to 0.14.0
 performGlejserTest(m, mtcars, "wt", robust = FALSE)
#> [INFO] Running Glejser test
#> 
#>  Glejser test for heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec
#> t = -0.44953, df = 30, p-value = 0.6563
#> 

 # Examine an alternative transformation
 performGlejserTest(m, mtcars, "wt", transformation = "inverse")
#> [INFO] Running Glejser test
#> 
#>  Glejser test for heteroscedasticity (robust to asymmetric errors)
#> 
#> data:  mpg ~ wt + qsec
#> t = 1.1243, df = 30, p-value = 0.2698
#> 
```
