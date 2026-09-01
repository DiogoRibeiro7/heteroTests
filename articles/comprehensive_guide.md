# Comprehensive Guide to Heteroscedasticity Testing

## Introduction

This comprehensive guide covers the theory, implementation, and
practical application of heteroscedasticity tests in the `heteroTests`
package.

## Theoretical Background

### What is Heteroscedasticity?

Heteroscedasticity occurs when the variance of the error terms in a
regression model changes with the level of one or more predictors. This
violates the constant variance assumption of ordinary least squares and
can lead to biased standard errors.

### When Does It Matter?

Non-constant variance implies inefficient parameter estimates and may
invalidate hypothesis tests that rely on the usual standard errors. It
is therefore important to detect and correct for heteroscedasticity
before drawing substantive conclusions from a model.

## Test Catalog

### Regression-Based Tests

#### White’s Test

White’s test regresses the squared residuals on the original regressors,
their squares, and cross-products. The resulting $`R^2`$ multiplied by
the sample size follows a chi-squared distribution under the null of
homoscedasticity.

#### Breusch-Pagan Test

The Breusch–Pagan test regresses the squared residuals on the original
regressors only. A significant regression indicates that the error
variance is related to the predictors.

### Graphical Diagnostics

The package includes convenient functions for residual vs fitted plots,
scale–location plots, QQ plots with envelopes, and leverage diagnostics
to help visualise patterns in the residuals.

## Practical Examples

### Example 1: Financial Time Series

``` r

set.seed(1)
x <- rnorm(250)
y <- 0.5 + 0.3 * x + rnorm(250, sd = abs(x))
dat <- data.frame(x, y)
mod <- lm(y ~ x, dat)
performWhiteTest(mod, dat)
#> Warning: Residual outliers detected at rows 24 (|z| > 5). Inspect leverage
#> before running White.
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 70.9198 df = 2 p = 0
#> 
#>  White's test for heteroscedasticity
#> 
#> data:  mod
#> X-squared = 70.92, df = 2, p-value = 3.981e-16
#> alternative hypothesis: heteroscedasticity present
```

### Example 2: Cross-Sectional Data

``` r

data(mtcars)
fit <- lm(mpg ~ wt + hp, data = mtcars)
performBPTest(fit, mtcars)
#> [INFO] Running Breusch-Pagan test
#> 
#>  Breusch-Pagan test for heteroscedasticity
#> 
#> data:  mpg ~ wt + hp
#> X-squared = 1.0268, df = 2, p-value = 0.5985
```

### Example 3: Panel Data

``` r

dat <- simulate_hetero(n = 250, beta0 = 1, beta1 = 2, sigma_func = sigma_linear)
fit <- lm(y ~ x, data = dat)
performSzroeterTest(fit, dat, order_by = "x")
#> [INFO] Running Szroeter test
#> 
#>  Szroeter test for ordered heteroscedasticity
#> 
#> data:  fit
#> Q = 9.062, n = 250, p-value < 2.2e-16
#> alternative hypothesis: variance increases with 'x'
#> sample estimates:
#>        h 
#> 183.9943
```

## Remediation Strategies

Common remedies include transforming the response (e.g. log or square
root), modelling the variance via weighted least squares, or using
heteroscedasticity-consistent standard errors.

## Comparison with Other Packages

Functions in the `car` and `lmtest` packages implement many of the same
diagnostics. `heteroTests` aims to provide a unified interface and a few
additional tools such as automated remediation suggestions.

## References

White, H. (1980). A heteroskedasticity-consistent covariance matrix
estimator and a direct test for heteroskedasticity. *Econometrica*, 48,
817–838.

Breusch, T. S., & Pagan, A. R. (1979). A simple test for
heteroscedasticity and random coefficient variation. *Econometrica*, 47,
1287–1294.
