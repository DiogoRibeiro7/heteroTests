# Box's M Test for Equality of Covariance Matrices

Test whether multiple groups have equal covariance matrices.

## Usage

``` r
performBoxMTest(data, group)
```

## Details

`performBoxMTest(data, group)` is the one test that does not take
`(model, data, ...)`: it tests the covariance matrices of the data
themselves, not the residuals of a fitted model.

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
#> data:  iris[, 1:4] by iris$Species
#> M = 135.22, df = 20, p-value < 2.2e-16
#> 
```
