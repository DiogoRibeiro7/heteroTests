# Withdrawn Rice difference-based pseudo-test

Withdrawn in 0.7.2. The statistic cannot detect heteroscedasticity; the
function signals a migration error rather than returning invalid
inference.

## Usage

``` r
performRiceTest(model, ...)
```

## Details

The former implementation compared Rice's (1984) difference-based
variance estimator with the mean squared residual and referred the ratio
to an F distribution. For independent residuals \\E\[(e_i -
e\_{i-1})^2\] = \sigma_i^2 + \sigma\_{i-1}^2\\, so the numerator
estimates the mean of the variances and so does the denominator: the
ratio sits at one under any variance pattern, not only under
homoscedasticity.

In simulation the ratio averaged 1.00 under homoscedasticity, under
\\\sigma_i = 0.2 + 1.2 x_i\\, under \\\sigma_i = \exp(x_i)\\ (a
fiftyfold spread in scale) and under a twentyfold step change, and the
test rejected in 0.0% of homoscedastic samples. Supplying an ordering
argument does not repair this, because the insensitivity is structural.

Rice's estimator is a tool for estimating the error variance in the
presence of a smooth mean function, not a variance-heterogeneity
diagnostic.

For variance that trends with an ordering variable use
[`performSzroeterTest`](https://diogoribeiro7.github.io/heteroTests/reference/performSzroeterTest.md)
or
[`performGQTest`](https://diogoribeiro7.github.io/heteroTests/reference/performGQTest.md).
For variance related to the regressors use
[`performBPTest`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md),
[`performKoenkerTest`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md)
or
[`performWhiteTest`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTest.md).

## Arguments

- model:

  an object of class `lm`. Retained only for backward-compatible
  argument matching.

- ...:

  further arguments, ignored.

## Value

This function does not return a test result. It signals an error with
migration guidance.

## References

Rice, J. (1984). Bandwidth choice for nonparametric regression. *The
Annals of Statistics*, 12(4), 1215–1230.
[doi:10.1214/aos/1176346788](https://doi.org/10.1214/aos/1176346788)

## See also

[`performSzroeterTest`](https://diogoribeiro7.github.io/heteroTests/reference/performSzroeterTest.md),
[`performGQTest`](https://diogoribeiro7.github.io/heteroTests/reference/performGQTest.md)
