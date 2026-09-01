# Perform Glejser test for heteroscedasticity

Regresses absolute residuals on a transformation of a suspected
variable.

## Usage

``` r
performGlejserTest(model, data, variable,
                   transformation = c("abs", "sqrt", "inverse", "inverse_sqrt"))
```

## Details

A variety of transformations of the explanatory variable (e.g. absolute
value, square root) can reveal a relationship between scale and the
covariate. Significance of the slope parameter is assessed with a t
test.

## Arguments

- model:

  an object of class `lm`.

- data:

  data frame used to fit `model`.

- variable:

  name of the suspected variable.

- transformation:

  transformation applied to `variable`.

## Value

An object of class `htest` containing the t statistic, p-value and
degrees of freedom.

## References

Glejser, H. (1969). A new test for heteroskedasticity. *Journal of the
American Statistical Association*, 64(325), 316–323.

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performGlejserTest(m, mtcars, "wt")
#> [INFO] Running Glejser test
#> 
#>  Glejser test for heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec
#> t = -0.44953, df = 30, p-value = 0.6563
#> 
```
