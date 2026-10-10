# Perform Glejser test for heteroscedasticity

Regresses absolute residuals on a transformation of a suspected
variable. With `robust = TRUE` the test is the version of Im (2000) and
of Machado and Santos Silva (2000), which keeps its level when the
errors are asymmetric.

## Usage

``` r
performGlejserTest(model, data, variable,
                   transformation = c("abs", "sqrt", "inverse", "inverse_sqrt"),
                   robust = FALSE)
```

## Details

A variety of transformations of the explanatory variable (e.g. absolute
value, square root) can reveal a relationship between scale and the
covariate. Significance of the slope parameter is assessed with a t
test. For a weighted fit the residuals are the Pearson residuals.

The default statistic has its nominal level when positive and negative
errors are equally likely. Use `robust = TRUE` when the residuals are
skewed or their shape is unknown.

Either form conditions on one variable and one transformation. Scanning
several transformations and reporting only the smallest p-value
invalidates the nominal level. Simulated size and power are recorded in
`inst/validation/pass-a-size-power.csv` for the default and in
`inst/validation/glejser-skewed-errors.csv` for both.

## Arguments

- model:

  an object of class `lm`.

- data:

  data frame used to fit `model`.

- variable:

  name of the suspected variable.

- transformation:

  transformation applied to `variable`.

- robust:

  logical. When `FALSE` (the default) the statistic is Glejser's (1969),
  which has its nominal level when positive and negative errors are
  equally likely, as they are for symmetric errors. When `TRUE` the
  absolute residuals are corrected for the asymmetry of the errors
  before the auxiliary regression. See the section on asymmetric errors.

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
`inst/validation/glejser-skewed-errors.R` the default statistic rejects
a true null hypothesis about 13% of the time at the 5% level under
exponential errors and about 17% under lognormal errors, at every sample
size from 50 to 1000. For other error distributions the rate can also
fall below the nominal level.

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
freedom, as for the default. The two p-values agree as the sample grows.

In the same study the corrected statistic is within simulation error of
the 5% level from 150 observations on. At 50 observations it rejects
5.7% to 6.6% of the time under skewed errors. Under symmetric errors
\\\hat{m}\\ is close to zero and the two statistics differ little: the
corrected one rejects about half a percentage point less often at 50
observations and gives up one or two points of power there. For a
weighted fit the correction is applied to the Pearson residuals.

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
 performGlejserTest(m, mtcars, "wt")
#> [INFO] Running Glejser test
#> 
#>  Glejser test for heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec
#> t = -0.44953, df = 30, p-value = 0.6563
#> 

 # The version that keeps its level under asymmetric errors
 performGlejserTest(m, mtcars, "wt", robust = TRUE)
#> [INFO] Running Glejser test
#> 
#>  Glejser test for heteroscedasticity (robust to asymmetric errors)
#> 
#> data:  mpg ~ wt + qsec
#> t = -0.39528, df = 30, p-value = 0.6954
#> 

 # Examine an alternative transformation
 performGlejserTest(m, mtcars, "wt", transformation = "inverse")
#> [INFO] Running Glejser test
#> 
#>  Glejser test for heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec
#> t = 0.9141, df = 30, p-value = 0.368
#> 
```
