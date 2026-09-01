# Perform Park test for heteroscedasticity

Regresses the log of squared residuals on the log of a suspected
variable.

## Usage

``` r
performParkTest(model, data, variable)
```

## Details

The slope coefficient from \\\log(e^2)\\ regressed on \\\log x\\ is
tested for zero using a t statistic. A significant coefficient indicates
scale depending on \\x\\.

## Arguments

- model:

  an object of class `lm`.

- data:

  data frame used to fit `model`.

- variable:

  name of the suspected explanatory variable.

## Value

An object of class `htest` containing the t statistic, p-value and
degrees of freedom.

## References

Park, R. E. (1966). Estimation with heteroscedastic error terms.
*Econometrica*, 34(5), 888.

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performParkTest(m, mtcars, "wt")
#> [INFO] Running Park test
#> 
#>  Park test for heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec
#> t = -0.6529, df = 30, p-value = 0.5188
#> 
```
