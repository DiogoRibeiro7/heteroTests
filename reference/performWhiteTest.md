# Perform White's test for heteroscedasticity

Implements White's test on a fitted linear model.

## Usage

``` r
performWhiteTest(model, data, cross_products = TRUE, max_interactions = 10)
```

## Details

An auxiliary regression of \\e^2\\ on all regressors, their squares and
cross-products produces \\R^2\\. The statistic \\n R^2\\ follows a
chi-square distribution with degrees of freedom equal to the number of
regressors in the auxiliary model.

## Arguments

- model:

  an object of class `lm`.

- data:

  Data frame used to fit `model`.

- cross_products:

  Logical. Include cross-product terms in the auxiliary regression?

- max_interactions:

  Maximum number of cross-product terms admitted to the auxiliary
  regression. Guards the auxiliary design against growing quadratically
  with the number of regressors.

## Value

An object of class `htest` containing the test statistic, p-value and
degrees of freedom.

## References

White, H. (1980). A heteroskedasticity-consistent covariance matrix
estimator and a direct test for heteroskedasticity. *Econometrica*,
48(4), 817–838. [doi:10.2307/1912934](https://doi.org/10.2307/1912934)

Greene, W. H. (2018). *Econometric Analysis* (8th ed.). Pearson.

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performWhiteTest(m, mtcars)
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.8225 df = 5 p = 0.0373
#> 
#>  White's test for heteroscedasticity
#> 
#> data:  m
#> X-squared = 11.822, df = 5, p-value = 0.0373
#> alternative hypothesis: heteroscedasticity present
#> 
```
