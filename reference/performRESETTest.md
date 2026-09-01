# Ramsey's RESET test for nonlinearity

Adds powers of the fitted values and performs an F-test to check for
neglected nonlinearity.

## Usage

``` r
performRESETTest(model, power = 2:3)
```

## Arguments

- model:

  A fitted `lm` model.

- power:

  Numeric vector of powers to include.

## Value

An object of class `htest`.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
performRESETTest(m)
#> 
#>  RESET test for nonlinearity
#> 
#> data:  mpg ~ wt + qsec
#> F = 6.7306, df1 = 2, df2 = 27, p-value = 0.00425
#> 
```
