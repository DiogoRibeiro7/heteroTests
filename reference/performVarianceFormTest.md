# Test the functional form of the error variance

Tests whether the error variance follows a stated parametric function of
stated variables, \\\sigma_i^2 = h(z_i; \gamma)\\. Every other test in
the package has constant variance as its null hypothesis, so it can say
that the variance moves with some variables but not whether an
exponential or a power function of them describes it. Here the variance
function is the null hypothesis, and rejecting it means that the chosen
shape is inadequate.

## Usage

``` r
performVarianceFormTest(
  model,
  data = NULL,
  var_formula = NULL,
  form = c("exponential", "power"),
  against = "squares"
)
```

## Arguments

- model:

  A fitted [stats::lm](https://rdrr.io/r/stats/lm.html) object for the
  mean equation, or the result of
  [`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md).
  In the second case the variance function that was fitted is the one
  tested, and `var_formula` and `form` must be left at their defaults.

- data:

  Optional [base::data.frame](https://rdrr.io/r/base/data.frame.html)
  holding the variables named in `var_formula` or `against`. It is
  needed only when they are not in the model frame, and is matched to
  the fitted rows by row name.

- var_formula:

  One-sided formula naming the variance regressors \\z_i\\, for example
  `~ x`. `NULL`, the default, uses the regressors of `model`.

- form:

  Character scalar giving the variance function under the null:
  `"exponential"` (the default) or `"power"`. They are defined in the
  section on variance functions of
  [`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md).

- against:

  The terms the variance function is tested against. The default,
  `"squares"`, adds the squares and pairwise products of the variance
  regressors, on the scale the form uses (logarithms for the power
  form). A one-sided formula such as `~ w` or `~ I(x^3)` names the added
  terms instead, which tests the variance function against a variable it
  leaves out or against a particular departure.

## Value

An object of class `htest` with the F statistic, its numerator and
denominator degrees of freedom, and the p-value for the null hypothesis
that the variance function is correctly specified. `estimate` holds the
fitted coefficients of the variance function: slopes of the log-variance
for the exponential form and exponents for the power form. The element
`variance_function` records the form, the variance regressors and the
added terms that entered the test.

## Details

The procedure has three steps.

1.  The variance function is fitted to the residuals of `model` and the
    model is refitted by weighted least squares, exactly as
    [`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md)
    does.

2.  The squared standardized residuals \\r_i = \tilde{e}\_i^2 /
    \hat\sigma_i^2\\ of the weighted fit are formed. If the variance
    function is right their mean does not depend on anything.

3.  \\r_i\\ is regressed on a constant, on \\g_i = \hat\sigma_i^{-2}\\
    \partial \sigma_i^2 / \partial \gamma\\, and on the added terms
    \\a_i\\. The statistic is the F test that the coefficients on
    \\a_i\\ are zero.

For the exponential form \\g_i\\ is \\z_i\\, and for the power form it
is \\\log z_i\\. The regression is the score direction of the variance
function extended by the added terms, so the test is a studentized
Lagrange multiplier test of the stated form against that extension.

Step 3 keeps \\g_i\\ in the regression for a reason. Estimating
\\\gamma\\ fits the variance along \\g_i\\, so those directions say
little about the form: a Breusch–Pagan or Koenker test of the weighted
fit on the same variance regressors rejects a wrong form about as often
as the right one. The information is in the terms outside the fitted
variance function. Conditioning on \\g_i\\ is also what makes the null
distribution free of the error in \\\hat\gamma\\ to first order, which
is the regression-based construction of Wooldridge (1990, 1991).

## Interpretation and caveats

A rejection says that the added terms move the variance beyond what the
stated function allows. A non-rejection says only that those terms do
not; it does not establish the form. Two functions that are close over
the observed range of \\z_i\\ cannot be told apart, and the test will
then accept both: a variance that is linear in a regressor is accepted
as exponential or as a power when the regressor varies over a short
range. Running the test for several forms is a comparison of fits, and
choosing the form with the largest p-value is not a test.

The test judges the shape, not the fitted coefficients. Because it
conditions on \\g_i\\, it does not respond to weights that are off along
the variance regressors themselves. A test of constant variance on the
weighted fit is sensitive to that, with the reservations about its level
described under weighted fits in
[`performKoenkerTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md).

The F reference distribution assumes that the standardized errors have a
constant fourth moment, the assumption behind the Koenker test. It does
not assume normality.

With \\q\\ variance regressors the default adds up to \\q (q + 1) / 2\\
terms. When that is large relative to the sample, name the variance
regressors with `var_formula` or the added terms with `against`.

## Validation

No reference implementation exists, so the statistic is checked against
a reconstruction from its definition to `1e-8`
(`tests/testthat/test-variance-form.R`) and by simulation. Over 24
designs in which the form under test was right (both forms, constant and
heteroscedastic variances, 50 to 400 observations, Gaussian and \\t_5\\
errors, 5000 replications each) the rejection rate at the 5% level ran
from 4.2% to 5.7%. Power against a log-variance that is quadratic in a
regressor was 98% at 150 observations. Against the other log-linear form
it was 22% at 150 observations and 74% at 400. See
`inst/validation/README.md`.

## References

Wooldridge, J. M. (1990). A unified approach to robust, regression-based
specification tests. *Econometric Theory, 6*(1), 17–43.
[doi:10.1017/S0266466600004898](https://doi.org/10.1017/S0266466600004898)

Wooldridge, J. M. (1991). On the application of robust, regression-based
diagnostics to models of conditional means and conditional variances.
*Journal of Econometrics, 47*(1), 5–46.
[doi:10.1016/0304-4076(91)90076-P](https://doi.org/10.1016/0304-4076%2891%2990076-P)

Harvey, A. C. (1976). Estimating regression models with multiplicative
heteroscedasticity. *Econometrica, 44*(3), 461–465.
[doi:10.2307/1913974](https://doi.org/10.2307/1913974)

Koenker, R. (1981). A note on studentizing a test for
heteroscedasticity. *Journal of Econometrics, 17*(1), 107–112.
[doi:10.1016/0304-4076(81)90062-2](https://doi.org/10.1016/0304-4076%2881%2990062-2)

Carroll, R. J., & Ruppert, D. (1988). *Transformation and Weighting in
Regression*. Chapman & Hall. Chapter 3 treats the estimation and
checking of variance functions.

## See also

[`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md)
fits the variance functions tested here.
[`performHarveyTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performHarveyTest.md)
and
[`performParkTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performParkTest.md)
test constant variance against the exponential and the power form
respectively.

## Examples

``` r
# Standard deviation proportional to x, so the variance is a power of x
set.seed(2026)
n <- 300
sim <- data.frame(x = runif(n, 1, 5))
sim$y <- 1 + 2 * sim$x + sim$x * rnorm(n)
mod <- lm(y ~ x, data = sim)

# The power form is the true one; estimate is the fitted exponent
performVarianceFormTest(mod, form = "power")
#> [INFO] Running variance-function specification test (power form)
#> 
#>  Specification test of the variance function (power form)
#> 
#> data:  y ~ x; variance regressors: model regressors; against: squares and products of the variance regressors
#> F = 0.028898, df1 = 1, df2 = 297, p-value = 0.8651
#> alternative hypothesis: the variance function is misspecified
#> sample estimates:
#>   log(x) 
#> 1.830335 
#> 

# The exponential form in x is not
performVarianceFormTest(mod, form = "exponential")
#> [INFO] Running variance-function specification test (exponential form)
#> 
#>  Specification test of the variance function (exponential form)
#> 
#> data:  y ~ x; variance regressors: model regressors; against: squares and products of the variance regressors
#> F = 5.604, df1 = 1, df2 = 297, p-value = 0.01856
#> alternative hypothesis: the variance function is misspecified
#> sample estimates:
#>         x 
#> 0.7009237 
#> 

# Test the variance function of a weighted fit directly
wls <- fitWLS(mod, form = "power")
performVarianceFormTest(wls)
#> [INFO] Running variance-function specification test (power form)
#> 
#>  Specification test of the variance function (power form)
#> 
#> data:  y ~ x; variance regressors: model regressors; against: squares and products of the variance regressors
#> F = 0.028898, df1 = 1, df2 = 297, p-value = 0.8651
#> alternative hypothesis: the variance function is misspecified
#> sample estimates:
#>   log(x) 
#> 1.830335 
#> 

# Test a variance function against one particular added term
performVarianceFormTest(mod, form = "exponential", against = ~ log(x))
#> [INFO] Running variance-function specification test (exponential form)
#> 
#>  Specification test of the variance function (exponential form)
#> 
#> data:  y ~ x; variance regressors: model regressors; against: log(x)
#> F = 5.0415, df1 = 1, df2 = 297, p-value = 0.02548
#> alternative hypothesis: the variance function is misspecified
#> sample estimates:
#>         x 
#> 0.7009237 
#> 
```
