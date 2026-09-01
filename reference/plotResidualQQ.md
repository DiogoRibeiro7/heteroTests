# QQ plot of residuals

Visualises departure from normality using a QQ plot.

## Usage

``` r
plotResidualQQ(model)
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
plotResidualQQ(m)
```
