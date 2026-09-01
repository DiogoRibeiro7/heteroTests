# Szroeter test for ordered alternatives

Detects monotonic changes in variance when observations can be
meaningfully ordered—typically by time or another covariate—using the
cumulative weighting scheme proposed by Szroeter (1978).

## Usage

``` r
performSzroeterTest(model, data, order_by,
  alternative = c("greater", "two.sided", "less"))
```

## Arguments

- model:

  A fitted [stats::lm](https://rdrr.io/r/stats/lm.html) object
  representing the mean equation under study.

- data:

  A [base::data.frame](https://rdrr.io/r/base/data.frame.html) (or
  compatible tibble) containing the variables used to fit `model` and
  the ordering variable supplied via `order_by`.

- order_by:

  Character scalar naming the column that defines the ordering of
  observations prior to computing the statistic. The column must be
  numeric or coercible to an orderable vector.

- alternative:

  Character scalar specifying the alternative hypothesis: `"greater"`
  (the default) tests for variance increasing with `order_by`, `"less"`
  for variance decreasing, and `"two.sided"` for a change in either
  direction. Szroeter's test targets monotone alternatives, so the
  one-sided form is the usual choice.

## Value

An object of class `htest` reporting Szroeter's standardised statistic
`Q`, the sample size, the underlying rank-weighted statistic `h` in
`estimate`, and the p-value from the asymptotic normal reference
distribution.

## Details

After ordering the residuals \\\hat{e}\_{(i)}\\ by `order_by`, the test
forms the rank-weighted average of the squared residuals \$\$h =
\frac{\sum\_{i = 1}^n i \\ \hat{e}\_{(i)}^2}{\sum\_{i = 1}^n
\hat{e}\_{(i)}^2},\$\$ which is Szroeter's (1978) class of statistics
evaluated at the canonical weights \\h_i = i\\. Under homoskedasticity
\\h\\ is centred on the mean rank \\(n + 1) / 2\\ with variance
\\(n^2 - 1) / (6n)\\, so \$\$Q = \frac{h - (n + 1) / 2}{\sqrt{(n^2 - 1)
/ (6n)}}\$\$ is asymptotically standard normal. Variance that grows with
`order_by` shifts weight onto the high ranks and drives \\Q\\ upwards.

The implementation relies on the package validation helpers to align the
supplied data with the model residuals, check that the ordering variable
is present, and ensure sufficient sample size and variability in the
squared residuals.

## Validation

The statistic and its null variance are checked against an independent
reconstruction of Szroeter (1978) in
`tests/testthat/test-pass-a-reference.R`. Releases before 0.7.0 divided
the centred statistic by a further \\\sqrt{n}\\, shrinking \\Q\\ by a
factor of roughly \\2 / \sqrt{n}\\ and leaving the test with essentially
no power against any alternative; see `NEWS.md`.

## References

Szroeter, J. (1978). A class of parametric tests for heteroscedasticity
in linear econometric models. *Econometrica, 46*(6), 1311–1327.
<https://doi.org/10.2307/1913833>

Godfrey, L. G. (1988). *Misspecification Tests in Econometrics*.
Cambridge University Press. Section 5.4 outlines the Szroeter test.

## See also

[`performOrderedLMTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performOrderedLMTest.md)
for a regression-based alternative and
[`performGQTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performGQTest.md)
for split-sample diagnostics on ordered data.

## Examples

``` r
data(mtcars)
mod <- lm(mpg ~ wt + qsec, data = mtcars)
performSzroeterTest(mod, mtcars, order_by = "wt")
#> [INFO] Running Szroeter test
#> 
#>  Szroeter test for ordered heteroscedasticity
#> 
#> data:  mod
#> Q = -0.64937, n = 32, p-value = 0.742
#> alternative hypothesis: variance increases with 'wt'
#> sample estimates:
#>        h 
#> 15.00108 
#> 

# Detect ordered heteroscedasticity in simulated data with variance increasing
# in an index variable
set.seed(404)
n <- 150
x <- sort(runif(n))
y <- 2 + 0.5 * x + rnorm(n, sd = 0.4 + 0.8 * x)
df <- data.frame(y, x)
performSzroeterTest(lm(y ~ x, data = df), df, order_by = "x")
#> [INFO] Running Szroeter test
#> 
#>  Szroeter test for ordered heteroscedasticity
#> 
#> data:  lm(y ~ x, data = df)
#> Q = 4.2696, n = 150, p-value = 9.793e-06
#> alternative hypothesis: variance increases with 'x'
#> sample estimates:
#>        h 
#> 96.84732 
#> 

# Two-sided version when the direction of the change is not known in advance
performSzroeterTest(mod, mtcars, order_by = "wt", alternative = "two.sided")
#> [INFO] Running Szroeter test
#> 
#>  Szroeter test for ordered heteroscedasticity
#> 
#> data:  mod
#> Q = -0.64937, n = 32, p-value = 0.5161
#> alternative hypothesis: variance changes monotonically with 'wt'
#> sample estimates:
#>        h 
#> 15.00108 
#> 
```
