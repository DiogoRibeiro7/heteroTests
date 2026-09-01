# Studentized Breusch–Pagan test

Computes the Koenker–Bassett studentized Lagrange Multiplier statistic
for heteroscedasticity by regressing centred squared residuals on the
regressors. Compared with the classical Breusch–Pagan test, the
studentized version is less sensitive to violations of normality and
small-sample bias.

## Usage

``` r
performStudentizedBPTest(model, data)
```

## Arguments

- model:

  A fitted [stats::lm](https://rdrr.io/r/stats/lm.html) object providing
  the residuals and design matrix for the auxiliary regression.

- data:

  A [base::data.frame](https://rdrr.io/r/base/data.frame.html)
  containing the variables used to fit `model`. It must include all
  observations referenced by the model object.

## Value

An object of class `htest` reporting the chi-squared statistic and
p-value for the null hypothesis of constant error variance.

## Details

Following Koenker (1981) and the implementation in
[lmtest::bptest()](https://rdrr.io/pkg/lmtest/man/bptest.html), the
procedure fits an auxiliary regression of \\e_i^2 - \hat{\sigma}^2\\ on
the regressors from the original model (including the intercept), where
\\e_i\\ denotes the weighted residuals and \\\hat{\sigma}^2\\ their mean
squared error. Under homoskedasticity the statistic \\T = n \sum w_i
\hat{g}\_i^2 / \sum (e_i^2 - \hat{\sigma}^2)^2\\ is asymptotically
chi-squared with degrees of freedom equal to the number of regressors
beyond the intercept. The implementation shares the validation helpers
used across the package to ensure that: (i) the model and data satisfy
minimum sample-size thresholds via
[rvalidateModelInputs()](https://diogoribeiro7.github.io/heteroTests/reference/rvalidateModelInputs.md)
and
[rvalidateDataInputs()](https://diogoribeiro7.github.io/heteroTests/reference/rvalidateDataInputs.md),
(ii) missing values are handled by
[rhandleMissingValues()](https://diogoribeiro7.github.io/heteroTests/reference/rhandleMissingValues.md),
and (iii) studentized-residual specific requirements registered in
[rvalidateTestRequirements()](https://diogoribeiro7.github.io/heteroTests/reference/rvalidateTestRequirements.md)
are met.

## References

Koenker, R. (1981). A note on studentizing a test for
heteroscedasticity. *Journal of Econometrics, 17*(1), 107–112.
<https://doi.org/10.1016/0304-4076(81)90062-2>

Davidson, R., & MacKinnon, J. G. (2004). *Econometric Theory and
Methods*. Oxford University Press. Section 16.7 discusses LM tests for
heteroscedasticity including studentized variants.

## See also

[`performBPTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md)
for the classical LM statistic and
[`performKoenkerTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md)
for the absolute-residual variant. The robust workflow in
[performBPTestRobust()](https://diogoribeiro7.github.io/heteroTests/reference/performBPTestRobust.md)
augments the studentized statistic with bootstrap diagnostics.

## Examples

``` r
data(mtcars)
mod <- lm(mpg ~ wt + qsec, data = mtcars)
performStudentizedBPTest(mod, mtcars)
#> [INFO] Running Studentized Breusch-Pagan test
#> 
#>  Studentized Breusch-Pagan test
#> 
#> data:  mod
#> X-squared = 3.0858, df = 2, p-value = 0.2138
#> alternative hypothesis: heteroscedasticity present
#> 

# Detect heteroscedasticity driven by a single regressor
set.seed(321)
x <- runif(180)
y <- 5 - 1.5 * x + rnorm(180, sd = 0.4 + 0.6 * x)
df <- data.frame(y, x)
performStudentizedBPTest(lm(y ~ x, data = df), df)
#> [INFO] Running Studentized Breusch-Pagan test
#> 
#>  Studentized Breusch-Pagan test
#> 
#> data:  lm(y ~ x, data = df)
#> X-squared = 9.6094, df = 1, p-value = 0.001936
#> alternative hypothesis: heteroscedasticity present
#> 
```
