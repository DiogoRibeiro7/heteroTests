# Weighted Least Squares wrapper

Refit a model by feasible generalised least squares, weighting each
observation by the inverse of an estimated error variance.

## Usage

``` r
fitWLS(
  model,
  data = NULL,
  var_formula = NULL,
  form = c("exponential", "power")
)
```

## Arguments

- model:

  A fitted model of class `lm`.

- data:

  Optional [base::data.frame](https://rdrr.io/r/base/data.frame.html)
  holding the variables named in `var_formula`. It is needed only when
  they are not in the model frame, and is matched to the fitted rows by
  row name.

- var_formula:

  One-sided formula naming the variance regressors, for example `~ x`.
  `NULL`, the default, uses the regressors of `model`.

- form:

  Character scalar choosing the variance function: `"exponential"` (the
  default) or `"power"`. See the section on variance functions.

## Value

A new `lm` object fitted with weights. The estimated variances are
attached as the `"variance_model"` attribute, and the fitted variance
function (its form, regressors and coefficients) as the
`"variance_function"` attribute, which is what
[`performVarianceFormTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performVarianceFormTest.md)
reads.

## Details

The variance is *modelled*, not read off the residuals directly. A
single squared residual is a one-degree-of-freedom estimate of
\\\sigma_i^2\\ and far too noisy to invert: weighting by \\1/e_i^2\\
hands almost all of the weight to whichever observations the initial fit
happened to reproduce most closely. This function instead fits a
variance function to the squared residuals and takes \\\hat\sigma_i^2\\
from its fitted values, the standard feasible-GLS recipe; weights are
\\1/\hat\sigma_i^2\\.

## Variance functions

`form` chooses the shape of the variance function and `var_formula` the
variables \\z_i\\ it depends on. Both forms are linear in the logarithm
of the variance and carry their own constant.

- `"exponential"`:

  \\\log \sigma_i^2 = \gamma_0 + z_i^\top \gamma\\, the multiplicative
  model of Harvey (1976), estimated by regressing \\\log e_i^2\\ on
  \\z_i\\. The log scale keeps the fitted variances positive without
  constraining the auxiliary regression. This is the default, and with
  `var_formula = NULL` it is what `fitWLS()` has computed since 0.9.0.

- `"power"`:

  \\\log \sigma_i^2 = \gamma_0 + \sum_j \delta_j \log z\_{ij}\\, that is
  \\\sigma_i^2 \propto \prod_j z\_{ij}^{\delta_j}\\, the model of Park
  (1966). It is the exponential form in the logarithms of the
  regressors, which must be strictly positive. \\\delta = 2\\ is the
  textbook case of a standard deviation proportional to \\z\\.

Other log-linear shapes are written through `var_formula`, for example
`~ x + I(x^2)` or `~ sqrt(x)`. The additive model \\\sigma_i^2 =
\alpha_0 + z_i^\top \alpha\\ is not offered: nothing keeps its fitted
variances positive, and in the validation study it was not usable in a
material share of samples drawn from an additive model itself
(`inst/validation/README.md`).

Residuals that are numerically zero are floored before the logarithm is
taken, with a warning.

Weights estimated this way are consistent under a correctly specified
variance function, so standard errors from the returned fit are usable.
They were not before 0.9.0, when the weights were the raw inverse
squared residuals: the weighted residual sum of squares then collapsed
towards \\n\\ regardless of the data, and nominal 95% intervals covered
the truth about 10% of the time.

Whether the variance function *is* correctly specified can be tested:
pass the result to
[`performVarianceFormTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performVarianceFormTest.md).

If the variance function cannot be fitted, or yields no usable
variation, the function falls back to equal weights, which reduces the
result to the original OLS fit.

## References

Harvey, A. C. (1976). Estimating regression models with multiplicative
heteroscedasticity. *Econometrica, 44*(3), 461–465.
[doi:10.2307/1913974](https://doi.org/10.2307/1913974)

Park, R. E. (1966). Estimation with heteroscedastic error terms.
*Econometrica, 34*(4), 888.
[doi:10.2307/1910108](https://doi.org/10.2307/1910108)

## See also

[`performVarianceFormTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performVarianceFormTest.md)
to test the variance function that was fitted.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
wls <- fitWLS(m)
summary(wls)
#> 
#> Call:
#> lm(formula = mpg ~ wt + qsec, data = mtcars)
#> 
#> Weighted Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -3.6581 -1.7335 -0.2666  0.9215  6.0419 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  14.0833     4.4722   3.149  0.00378 ** 
#> wt           -4.7283     0.4273 -11.066 6.32e-12 ***
#> qsec          1.1942     0.2448   4.878 3.56e-05 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 2.397 on 29 degrees of freedom
#> Multiple R-squared:  0.8405, Adjusted R-squared:  0.8296 
#> F-statistic: 76.44 on 2 and 29 DF,  p-value: 2.742e-12
#> 

# Variance proportional to a power of one regressor
wls_power <- fitWLS(m, var_formula = ~ wt, form = "power")
attr(wls_power, "variance_function")$coefficients
#> (Intercept)     log(wt) 
#>   1.3406009  -0.9787352 
```
