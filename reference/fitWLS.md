# Weighted Least Squares wrapper

Estimate heteroscedasticity-consistent weights from residuals and refit
the model.

## Usage

``` r
fitWLS(model)
```

## Arguments

- model:

  A fitted model of class `lm`.

## Value

A new `lm` object fitted with weights.

## Details

The weights are computed as the inverse squared residuals from the
initial fit.

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
#> -1.1287 -0.9888 -0.7718  0.9670  1.1505 
#> 
#> Coefficients:
#>             Estimate Std. Error t value Pr(>|t|)    
#> (Intercept) 19.50121    0.84014   23.21   <2e-16 ***
#> wt          -4.92654    0.05930  -83.08   <2e-16 ***
#> qsec         0.91885    0.05032   18.26   <2e-16 ***
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#> 
#> Residual standard error: 0.9788 on 29 degrees of freedom
#> Multiple R-squared:  0.9959, Adjusted R-squared:  0.9956 
#> F-statistic:  3480 on 2 and 29 DF,  p-value: < 2.2e-16
#> 
```
