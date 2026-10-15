# Robust Breusch–Pagan test with bootstrap and effect sizes

Enhances
[`performBPTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md)
by optionally studentising residuals, resampling the test statistic via
bootstrap, and reporting effect sizes together with power diagnostics.

## Usage

``` r
performBPTestRobust(
  model,
  data = NULL,
  studentize = TRUE,
  bootstrap = FALSE,
  B = 999,
  ci_level = 0.95,
  parallel = FALSE,
  studentized
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
  variable needs `data`, as does a model with an
  [`offset()`](https://rdrr.io/r/stats/offset.html) term, and so may a
  fit whose `weights` or `offset` argument is a column of the data,
  which the model frame holds as `(weights)` or `(offset)`.

- studentize:

  Logical, use studentized residuals for the auxiliary regression
  (Koenker variant)? Defaults to `TRUE`. The name matches
  [`lmtest::bptest()`](https://rdrr.io/pkg/lmtest/man/bptest.html) and
  [`performHarveyTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performHarveyTest.md).

- bootstrap:

  Logical, compute bootstrap diagnostics for the statistic and p-value?

- B:

  Number of bootstrap replications when `bootstrap = TRUE`. Defaults to
  999, at which a bootstrap p-value of the form `(1 + #) / (B + 1)`
  gives an exact test at the 5% and 1% levels; it was 1000 before
  0.15.0. The value is recorded in `robust_details$bootstrap$B` whether
  or not the bootstrap runs.

- ci_level:

  Confidence level for reported intervals.

- parallel:

  Logical, allow parallel bootstrap evaluation when the `parallel`
  package is available. A seeded call gives the same result each time;
  see *Parallel replicates* in
  [`bootstrapTestStatistic()`](https://diogoribeiro7.github.io/heteroTests/reference/rbootstrap_test_statistic.md).

- studentized:

  Deprecated since 0.15.0: use `studentize`. It is still accepted, with
  a warning, and does what `studentize` does; it stops working in 1.0.0.

## Value

An augmented `htest` object containing a `robust_details` element
describing the additional diagnostics. Its `studentized` field records
the value of `studentize`.

## Details

Offers expanded reporting for the Breusch–Pagan family of tests
including bootstrap p-values and asymptotic confidence intervals. The
optional `studentize` argument toggles between the classic and Koenker
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
[`performKoenkerTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md)
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
