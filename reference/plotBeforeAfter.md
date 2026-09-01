# Compare residuals before and after remediation

Overlays residuals of two models on a single plot to visualise
improvement after applying a remediation method (e.g. WLS or robust
regression).

## Usage

``` r
plotBeforeAfter(original, remedied)
```

## Arguments

- original:

  The original `lm` or `glm` model.

- remedied:

  The model fitted after remediation.

## Value

A `ggplot` object with residuals of both models.

## Examples

``` r
data(mtcars)
m1 <- lm(mpg ~ wt, data = mtcars)
m2 <- fitWLS(m1)
plotBeforeAfter(m1, m2)
#> `geom_smooth()` using formula = 'y ~ x'
```
