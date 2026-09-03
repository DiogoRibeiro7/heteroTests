# Automatically compare remedial models

Fits several remedial models for heteroscedasticity and compares their
performance using AIC and residual RMSE. Currently evaluates weighted
least squares and robust regression against the original model.

## Usage

``` r
autoCompareRemediations(model, data = NULL)
```

## Arguments

- model:

  Fitted `lm` model or formula.

- data:

  Optional data frame if `model` is a formula.

## Value

A list with components `metrics`, `models`, and `best` indicating the
recommended method.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + qsec, mtcars)
autoCompareRemediations(m)
#> $models
#> $models$OLS
#> 
#> Call:
#> lm(formula = mpg ~ wt + qsec, data = mtcars)
#> 
#> Coefficients:
#> (Intercept)           wt         qsec  
#>     19.7462      -5.0480       0.9292  
#> 
#> 
#> $models$WLS
#> 
#> Call:
#> lm(formula = mpg ~ wt + qsec, data = mtcars)
#> 
#> Coefficients:
#> (Intercept)           wt         qsec  
#>      14.083       -4.728        1.194  
#> 
#> 
#> $models$Robust
#> Call:
#> rlm(formula = form, data = data)
#> Converged in 5 iterations
#> 
#> Coefficients:
#> (Intercept)          wt        qsec 
#>  20.6990369  -5.1045692   0.8756602 
#> 
#> Degrees of freedom: 32 total; 29 residual
#> Scale estimate: 2.6 
#> 
#> 
#> $metrics
#>        Method      AIC     RMSE Recommended
#> OLS       OLS 156.7205 2.471485       FALSE
#> WLS       WLS 151.6246 2.525729        TRUE
#> Robust Robust 156.9512 2.480412       FALSE
#> 
#> $best
#> [1] "WLS"
#> 
```
