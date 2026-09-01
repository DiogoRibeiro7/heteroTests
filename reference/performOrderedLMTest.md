# Perform Ordered Lagrange Multiplier test

Runs a Breusch-Pagan regression on data ordered by a variable.

## Usage

``` r
performOrderedLMTest(model, data, order_by)
```

## Details

Observations are sorted and a cumulative form of the Breusch-Pagan
statistic is computed. The maximum value is compared to the chi-square
distribution.

## Arguments

- model:

  an object of class `lm`.

- data:

  data frame used to fit `model`.

- order_by:

  name of variable used for ordering.

## Value

An object of class `htest` containing the test statistic, p-value and
degrees of freedom.

## References

Harrison, M. J., & McCabe, B. P. M. (1979). A random walk test for
heteroscedasticity. *Journal of Econometrics*, 10(3), 219–229.

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performOrderedLMTest(m, mtcars, order_by = "wt")
#> [INFO] Running Ordered LM test
#> Warning: performOrderedLMTest() ignores `order_by`: reordering the rows and refitting leaves the statistic unchanged, and the result is identical to performKoenkerTest(). Use performSzroeterTest() or performGQTest() for a genuinely ordered alternative.
#> 
#>  Ordered Lagrange Multiplier test
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 3.0858, = 2, p-value = 0.2138
#> 
```
