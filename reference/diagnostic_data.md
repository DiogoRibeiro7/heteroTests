# Simulated dataset with collinearity and mild nonlinearity

This dataset contains two predictors with correlation and a response
including a quadratic term. It is useful for testing diagnostics beyond
heteroscedasticity.

## Usage

``` r
data(diagnostic_data)
```

## Format

A data frame with 150 rows and 3 variables:

- x1:

  first predictor

- x2:

  second predictor, correlated with `x1`

- y:

  response

## Source

Simulated via `set.seed(123)`.

## Examples

``` r
data(diagnostic_data)
head(diagnostic_data)
#>            x1          x2             y
#> 1 -0.56047565 -0.48170176 -2.124232e+00
#> 2 -0.23017749 -0.15327327 -6.463729e-01
#> 3  1.55870831  1.59192857  9.169449e+00
#> 4  0.07050839 -0.03032927  1.411932e-06
#> 5  0.12928774  0.11734247  1.181801e+00
#> 6  1.71506499  1.68702545  1.129311e+01
```
