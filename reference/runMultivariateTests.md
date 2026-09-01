# Run multivariate heteroscedasticity tests

Convenience wrapper for multivariate tests such as Box's M.

## Usage

``` r
runMultivariateTests(data, group, tests = c("box_m"))
```

## Arguments

- data:

  Numeric data frame or matrix with multiple variables.

- group:

  Factor defining groups.

- tests:

  Character vector of test names.

## Value

A named list of `htest` objects.

## Examples

``` r
runMultivariateTests(iris[,1:4], iris$Species)
#> [[1]]
#> 
#>  Box's M Test for Equality of Covariance Matrices
#> 
#> data:  
#> M = 135.22, df = 20, p-value < 2.2e-16
#> 
#> 
```
