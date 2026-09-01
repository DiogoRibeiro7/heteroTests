# Perform Spearman rank correlation test

Computes Spearman's rho between absolute residuals and fitted values.

## Usage

``` r
performSpearmanTest(model)
```

## Details

The squared rank correlation coefficient times the sample size minus 1
approximates a chi-square distribution with one degree of freedom.

## Arguments

- model:

  an object of class `lm`.

## Value

An object of class `htest` containing the test statistic, p-value,
degrees of freedom and rho.

## References

Spearman, C. (1904). The proof and measurement of association between
two things. *American Journal of Psychology*, 15(1), 72–101.

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performSpearmanTest(m)
#> [INFO] Running Spearman rank correlation test
#> 
#>  Spearman rank correlation test for heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec
#> t = 1.4452, = 30, p-value = 0.1588
#> sample estimates:
#>      rho 
#> 0.255132 
#> 
```
