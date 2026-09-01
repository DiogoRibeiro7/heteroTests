# Density plot of residuals

Shows the distribution of residuals with a kernel density estimate.

## Usage

``` r
plotResidualDensity(model)
```

## Arguments

- model:

  A fitted model of class `lm` or `glm`.

## Value

A `ggplot` object.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
plotResidualDensity(m)
```
