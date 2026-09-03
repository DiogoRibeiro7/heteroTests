# Weighted Least Squares wrapper

Refit a model by feasible generalised least squares, weighting each
observation by the inverse of an estimated error variance.

## Usage

``` r
fitWLS(model)
```

## Arguments

- model:

  A fitted model of class `lm`.

## Value

A new `lm` object fitted with weights. The estimated variances are
attached as the `"variance_model"` attribute.

## Details

The variance is *modelled*, not read off the residuals directly. A
single squared residual is a one-degree-of-freedom estimate of
\\\sigma_i^2\\ and far too noisy to invert: weighting by \\1/e_i^2\\
hands almost all of the weight to whichever observations the initial fit
happened to reproduce most closely. This function instead regresses
\\\log e_i^2\\ on the model's own design matrix and takes
\\\hat\sigma_i^2 = \exp(\hat g_i)\\ from the fitted values, the standard
feasible-GLS recipe; weights are \\1/\hat\sigma_i^2\\.

The log scale keeps the fitted variances positive without constraining
the auxiliary regression, and residuals that are numerically zero are
floored before the logarithm, with a warning.

Weights estimated this way are consistent under a correctly specified
variance model, so standard errors from the returned fit are usable.
They were not before 0.9.0, when the weights were the raw inverse
squared residuals: the weighted residual sum of squares then collapsed
towards \\n\\ regardless of the data, and nominal 95% intervals covered
the truth about 10% of the time.

If the variance model cannot be fitted, or yields no usable variation,
the function falls back to equal weights, which reduces the result to
the original OLS fit.

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
```
