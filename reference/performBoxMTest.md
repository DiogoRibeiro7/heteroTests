# Box's M Test for Equality of Covariance Matrices

Test whether multiple groups have equal covariance matrices.

## Usage

``` r
performBoxMTest(data, group)
```

## Arguments

- data:

  A numeric data frame or matrix.

- group:

  A factor indicating group membership.

## Value

An object of class `htest`.

## Examples

``` r
data(iris)
performBoxMTest(iris[,1:4], iris$Species)
#> 
#>  Box's M Test for Equality of Covariance Matrices
#> 
#> data:  
#> M = 135.22, df = 20, p-value < 2.2e-16
#> 
```
