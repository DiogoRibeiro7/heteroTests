# Perform Koenker studentized Breusch-Pagan test

Implementation of the Koenker version of the Breusch-Pagan test.

## Usage

``` r
performKoenkerTest(model, data)
```

## Details

Squared residuals are regressed on the regressors but the statistic \\n
R^2\\ uses a studentized form that is robust to non-normality.

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
