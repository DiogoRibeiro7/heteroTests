# Plot residuals vs fitted values

Generates a simple scatter plot of residuals against fitted values from
a linear model. A horizontal reference line at zero is added.

## Usage

``` r
plotResidualsFitted(model)
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
plotResidualsFitted(m)
#> `geom_smooth()` using formula = 'y ~ x'
```
