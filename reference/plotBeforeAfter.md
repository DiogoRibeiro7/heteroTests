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

## Details

A weighted fit is shown through its Pearson residuals \\\sqrt{w_i}\\
e_i\\, so the plot shows whether the weighting flattened the spread. Its
raw residuals would look as heteroscedastic as the original ones however
good the weights were.

## Examples

``` r
data(mtcars)
m1 <- lm(mpg ~ wt, data = mtcars)
m2 <- fitWLS(m1)
plotBeforeAfter(m1, m2)
#> `geom_smooth()` using formula = 'y ~ x'
```
