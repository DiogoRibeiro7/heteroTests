# Studentized Breusch–Pagan test (deprecated)

`performStudentizedBPTest()` is deprecated since 0.14.0: use
[`performKoenkerTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md),
which computes the same statistic. It still works, returns what it
returned before and warns. On a fit returned by
[`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md)
the two differ, and
[`performKoenkerTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md)
is the correct one: the model frame of that fit still holds the weights
of the model
[`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md)
refitted (none, for an unweighted model), not the estimated weights, and
`performStudentizedBPTest()` reads them from there. That is to be fixed
in 0.16.0.

## Usage

``` r
performStudentizedBPTest(model, data = NULL)
```

## Arguments

- model:

  A fitted [stats::lm](https://rdrr.io/r/stats/lm.html) object providing
  the residuals and design matrix for the auxiliary regression.

- data:

  A [base::data.frame](https://rdrr.io/r/base/data.frame.html)
  containing the variables used to fit `model`. It must include all
  observations referenced by the model object. When `NULL`, the default,
  the model frame of `model` is used. It holds the terms of the model
  rather than its variables (`log(x)` rather than `x`), so a model that
  transforms a variable needs `data`, as does a model with an
  [`offset()`](https://rdrr.io/r/stats/offset.html) term, and so may a
  fit whose `weights` or `offset` argument is a column of the data,
  which the model frame holds as `(weights)` or `(offset)`.

## Value

An object of class `htest` reporting the chi-squared statistic and
p-value for the null hypothesis of constant error variance.

## Details

Computes Koenker's (1981) studentized form of the Breusch–Pagan Lagrange
multiplier statistic, \\n R^2\\ from the regression of the squared
residuals on the regressors. The classical Breusch–Pagan statistic
assumes normal errors and this one does not: in the simulations of
`inst/validation/` it holds its level under \\t_5\\ errors, where the
classical statistic rejects far too often.

Following Koenker (1981) and the implementation in
[lmtest::bptest()](https://rdrr.io/pkg/lmtest/man/bptest.html), the
procedure fits an auxiliary regression of \\e_i^2 - \hat{\sigma}^2\\ on
the regressors from the original model (including the intercept), where
\\e_i\\ denotes the residuals and \\\hat{\sigma}^2 = \sum e_i^2 / n\\
the mean of their squares. With \\\hat{g}\_i\\ the fitted values of that
regression, the statistic \\T = n \sum \hat{g}\_i^2 / \sum (e_i^2 -
\hat{\sigma}^2)^2\\ is its \\n R^2\\. Under homoskedasticity it is
asymptotically chi-squared with degrees of freedom equal to the number
of regressors beyond the intercept, whether or not the errors are
normal. On a weighted fit \\e_i\\ is the Pearson residual \\\sqrt{w_i}\\
times the raw residual and the auxiliary regression is unweighted; see
the section on weighted fits in
[`performKoenkerTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md).
This departs from
[`lmtest::bptest()`](https://rdrr.io/pkg/lmtest/man/bptest.html), which
keeps the raw residuals and weights the auxiliary regression. The
implementation shares the validation helpers used across the package to
ensure that: (i) the model and data satisfy minimum sample-size
thresholds via
[rvalidateModelInputs()](https://diogoribeiro7.github.io/heteroTests/reference/rvalidateModelInputs.md)
and
[rvalidateDataInputs()](https://diogoribeiro7.github.io/heteroTests/reference/rvalidateDataInputs.md),
(ii) missing values are handled by
[rhandleMissingValues()](https://diogoribeiro7.github.io/heteroTests/reference/rhandleMissingValues.md),
and (iii) the requirements registered for this test in
[rvalidateTestRequirements()](https://diogoribeiro7.github.io/heteroTests/reference/rvalidateTestRequirements.md)
are met.

## References

Koenker, R. (1981). A note on studentizing a test for
heteroscedasticity. *Journal of Econometrics, 17*(1), 107–112.
[doi:10.1016/0304-4076(81)90062-2](https://doi.org/10.1016/0304-4076%2881%2990062-2)

Davidson, R., & MacKinnon, J. G. (2004). *Econometric Theory and
Methods*. Oxford University Press. Section 7.5 derives the test as \\n
R^2\\ from the regression of the squared residuals on the variance
regressors.

## See also

[`performBPTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md)
for the classical LM statistic, which assumes normal errors, and
[`performKoenkerTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md),
which computes the same studentized \\n R^2\\ statistic on the squared
residuals. The robust workflow in
[performBPTestRobust()](https://diogoribeiro7.github.io/heteroTests/reference/performBPTestRobust.md)
augments the studentized statistic with bootstrap diagnostics.

## Examples

``` r
data(mtcars)
mod <- lm(mpg ~ wt + qsec, data = mtcars)
# Deprecated: warns, and returns the statistic of performKoenkerTest()
suppressWarnings(performStudentizedBPTest(mod, mtcars))
#> [INFO] Running Studentized Breusch-Pagan test
#> 
#>  Studentized Breusch-Pagan test
#> 
#> data:  mod
#> X-squared = 3.0858, df = 2, p-value = 0.2138
#> alternative hypothesis: heteroscedasticity present
#> 
performKoenkerTest(mod, mtcars)
#> [INFO] Running Koenker test
#> 
#>  Koenker studentized Breusch-Pagan test
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 3.0858, df = 2, p-value = 0.2138
#> 
```
