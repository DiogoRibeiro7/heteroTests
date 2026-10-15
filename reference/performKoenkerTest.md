# Perform Koenker studentized Breusch-Pagan test

Implementation of the Koenker version of the Breusch-Pagan test.

## Usage

``` r
performKoenkerTest(model, data)
```

## Details

Squared residuals are regressed on the regressors but the statistic \\n
R^2\\ uses a studentized form that is robust to non-normality.

## Weighted fits

`lm(..., weights = w)` states that the error variance is \\\sigma^2 /
w_i\\. On such a fit the test is computed from the Pearson residuals
\\\sqrt{w_i}\\ e_i\\, the residuals of the equivalent unweighted
regression, which have constant variance when the weights are right. The
null hypothesis is then that the weights are adequate; the variance
regressors stay on their original scale. Releases before 0.12.0 used the
raw residuals, whose variance differs across observations by assumption,
so a correctly weighted fit was rejected as often as an unweighted one.
A fit with a zero weight is refused.

The reference distribution takes the weights as known. When they were
estimated from the same data, as by
[`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md),
it is only approximate, and not more accurate in larger samples. With a
correctly specified variance function and normal errors, the Koenker
test rejected 8% of the time at the 5% level with 150 observations and
11% with 600, the White test 6% and 8%, and the Harvey test never,
because it repeats the regression the weights were estimated from
(`inst/validation/weighted-fits-size.csv`).
[`performVarianceFormTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performVarianceFormTest.md)
accounts for the estimation, held its level in the same designs, and is
the test to use on a
[`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md)
fit.

## Arguments

- model:

  an object of class `lm`.

- data:

  data frame used to fit `model`.

## Value

An object of class `htest` containing the test statistic, p-value and
degrees of freedom.

## References

Koenker, R. (1981). A note on studentizing a test for
heteroscedasticity. *Journal of Econometrics*, 17(1), 107–112.

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performKoenkerTest(m, mtcars)
#> [INFO] Running Koenker test
#> 
#>  Koenker studentized Breusch-Pagan test
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 3.0858, df = 2, p-value = 0.2138
#> 
```
