# Robust Breusch–Pagan test with bootstrap and effect sizes

Enhances
[`performBPTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md)
by optionally studentising residuals, resampling the test statistic via
bootstrap, and reporting effect sizes together with power diagnostics.

## Usage

``` r
performBPTestRobust(
  model,
  data,
  studentized = TRUE,
  bootstrap = FALSE,
  B = 1000,
  ci_level = 0.95,
  parallel = FALSE
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

- studentized:

  Logical, use studentized residuals for the auxiliary regression
  (Koenker variant)? Defaults to `TRUE`.

- bootstrap:

  Logical, compute bootstrap diagnostics for the statistic and p-value?

- B:

  Number of bootstrap replications when `bootstrap = TRUE`.

- ci_level:

  Confidence level for reported intervals.

- parallel:

  Logical, allow parallel bootstrap evaluation when the `parallel`
  package is available.

## Value

An augmented `htest` object containing a `robust_details` element
describing the additional diagnostics.

## Details

Offers expanded reporting for the Breusch–Pagan family of tests
including bootstrap p-values and asymptotic confidence intervals. The
optional `studentized` argument toggles between the classic and Koenker
versions.

## References

Breusch, T. S., & Pagan, A. R. (1979). A simple test for
heteroscedasticity and random coefficient variation. *Econometrica,
47*(5), 1287–1294.

Koenker, R. (1981). A note on studentizing a test for
heteroscedasticity. *Journal of Econometrics, 17*(1), 107–112.

## See also

[`performBPTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md)
for the baseline test and
[`performStudentizedBPTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performStudentizedBPTest.md)
for the standalone studentised variant.

## Examples

``` r
data(mtcars)
mod <- lm(mpg ~ wt + qsec, data = mtcars)
performBPTestRobust(mod, mtcars, bootstrap = TRUE, B = 200)
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> [INFO] Running Studentized Breusch-Pagan test
#> 
#>  Studentized Breusch-Pagan test (robust Studentized )
#> 
#> data:  model
#> X-squared = 3.0858, df = 2, p-value = 0.2138
#> alternative hypothesis: heteroscedasticity present
#> 
```
