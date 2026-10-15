# Ramsey's RESET test for nonlinearity

Adds powers of the fitted values and performs an F-test to check for
neglected nonlinearity.

## Usage

``` r
performRESETTest(model, data = NULL, power = 2:3)
```

## Details

On a fit with weights the augmented model is fitted by weighted least
squares and both residual sums of squares are weighted. Releases before
0.12.0 refitted the augmented model without the weights, so the two
models were not nested.

## Arguments

- model:

  A fitted `lm` model.

- data:

  optional data frame the model was fitted to. When `NULL`, the default,
  the model frame of `model` is used. The statistic is computed from the
  response, fitted values and model matrix of `model`, so `data` does
  not change the result; it is accepted so that every test takes
  `(model, data, ...)`. Before 0.15.0 the second argument was `power`: a
  number passed there by position is still read as `power` in 0.15.x,
  with a warning that shows the new call, and stops working in 1.0.0.

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
