# Perform Breusch-Pagan test for heteroscedasticity

Implements the classical Breusch-Pagan (1979) test on a fitted linear
model, in which the scaled squared residuals are regressed on the
original regressors.

## Usage

``` r
performBPTest(model, data)
performBreuschPaganTest(model, data)
```

## Details

The test statistic is half the explained sum of squares from regressing
the scaled squared residuals \\e_i^2/\hat{\sigma}^2 - 1\\ (with
\\\hat{\sigma}^2 = \sum e_i^2 / n\\) on the explanatory variables. Under
the null hypothesis of homoscedasticity *and normal disturbances* it
follows a chi-square distribution with degrees of freedom equal to the
number of regressors, matching
`lmtest::bptest(..., studentize = FALSE)`. For the studentized \\n R^2\\
form that drops the normality assumption use
[`performKoenkerTest`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md)
or
[`performStudentizedBPTest`](https://diogoribeiro7.github.io/heteroTests/reference/performStudentizedBPTest.md).

## Arguments

- model:

  an object of class `lm`.

- data:

  data frame used to fit `model`.

## Value

An object of class `htest` containing the test statistic, p-value and
degrees of freedom.

## References

Breusch, T. S., & Pagan, A. R. (1979). A simple test for
heteroscedasticity and random coefficient variation. *Econometrica*,
47(5), 1287–1294. [doi:10.2307/1911963](https://doi.org/10.2307/1911963)

Koenker, R. (1981). A note on studentizing a test for
heteroscedasticity. *Journal of Econometrics*, 17(1), 107–112.
[doi:10.1016/0304-4076(81)90062-2](https://doi.org/10.1016/0304-4076%2881%2990062-2)

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performBPTest(m, mtcars)
#> [INFO] Running Breusch-Pagan test
#> 
#>  Breusch-Pagan test for heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 3.1348, df = 2, p-value = 0.2086
#> 
```
