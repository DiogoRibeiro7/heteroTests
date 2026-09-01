# Spread-Level plot for variance diagnostics

Plots the square root of the absolute residuals against fitted values. A
lowess smooth is added to highlight trends.

## Usage

``` r
plotSpreadLevel(model)
```

## Arguments

- model:

  A fitted model of class `lm`.

## Value

A `ggplot` object.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
plotSpreadLevel(m)
#> `geom_smooth()` using formula = 'y ~ x'
```
