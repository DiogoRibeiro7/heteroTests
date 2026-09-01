# Perform Harvey test for multiplicative heteroscedasticity

Harvey's (1976) Lagrange multiplier test, which regresses
`log(residuals^2)` on a set of variance regressors.

## Usage

``` r
performHarveyTest(model, auxiliary = c("regressors", "fitted"),
                  studentize = FALSE)
```

## Details

Harvey (1976) models the error variance as \\\sigma_i^2 = \exp(z_i^\top
\gamma)\\ and tests \\\gamma = 0\\ through the auxiliary regression of
\\\log \hat{e}\_i^2\\ on \\z_i\\. Under the null the auxiliary error
behaves like a centred \\\log \chi^2_1\\ variate with variance \\\pi^2 /
2 \approx 4.9348\\, so the classical statistic is \\\mathrm{ESS} /
(\pi^2 / 2)\\, asymptotically chi-square with \\q\\ degrees of freedom.

Setting `studentize = TRUE` replaces the fixed constant \\\pi^2 / 2\\
with the auxiliary residual mean square and refers the overall F
statistic to an F distribution. The two forms are asymptotically
equivalent under normal errors; the studentized form is more reliable
when normality is doubtful, because \\\pi^2 / 2\\ is the null variance
of \\\log \hat{e}^2\\ only for Gaussian errors.

Degrees of freedom are taken from the realised rank of the auxiliary
design, so a rank-deficient variance model is not credited with degrees
of freedom for aliased columns.

## Arguments

- model:

  an object of class `lm`.

- auxiliary:

  character scalar choosing the variance regressors. `"regressors"` (the
  default) uses the model's own explanatory variables, the specification
  in Harvey (1976). `"fitted"` uses the fitted values and their square,
  which was the behaviour of releases before 0.7.0.

- studentize:

  logical; if `FALSE` (the default) the chi-square statistic
  \\\mathrm{ESS} / (\pi^2 / 2)\\ is reported, otherwise the auxiliary
  regression F statistic.

## Value

An object of class `htest` containing the test statistic, its degrees of
freedom and the p-value.

## Validation

The default statistic reproduces an independent reconstruction of Harvey
(1976) to within `1e-8`; see `tests/testthat/test-pass-a-reference.R`.
Simulated size and power are recorded in
`inst/validation/pass-a-size-power.csv`.

## References

Harvey, A. C. (1976). Estimating regression models with multiplicative
heteroscedasticity. *Econometrica*, 44(3), 461–465.
[doi:10.2307/1913974](https://doi.org/10.2307/1913974)

Greene, W. H. (2018). *Econometric Analysis* (8th ed.). Pearson. Section
9.5 derives the \\\mathrm{ESS} / 4.9348\\ form of the statistic.

## See also

[`performParkTest`](https://diogoribeiro7.github.io/heteroTests/reference/performParkTest.md),
[`performGlejserTest`](https://diogoribeiro7.github.io/heteroTests/reference/performGlejserTest.md),
[`performBPTest`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md)

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performHarveyTest(m)
#> [INFO] Running Harvey test
#> 
#>  Harvey test for multiplicative heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec; variance regressors: model regressors
#> X-squared = 2.4457, df = 2, p-value = 0.2944
#> alternative hypothesis: error variance is a multiplicative function of the variance regressors
#> 

 # Studentized variant, which does not assume normal errors
 performHarveyTest(m, studentize = TRUE)
#> [INFO] Running Harvey test
#> 
#>  Harvey test for multiplicative heteroscedasticity (studentized)
#> 
#> data:  mpg ~ wt + qsec; variance regressors: model regressors
#> F = 0.86808, df1 = 2, df2 = 29, p-value = 0.4304
#> alternative hypothesis: error variance is a multiplicative function of the variance regressors
#> 

 # Variance driven by the conditional mean rather than by the regressors
 performHarveyTest(m, auxiliary = "fitted")
#> [INFO] Running Harvey test
#> 
#>  Harvey test for multiplicative heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec; variance regressors: fitted values and their square
#> X-squared = 4.653, df = 2, p-value = 0.09764
#> alternative hypothesis: error variance is a multiplicative function of the variance regressors
#> 
```
