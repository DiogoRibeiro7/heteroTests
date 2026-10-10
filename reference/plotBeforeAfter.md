# Compare residuals before and after remediation

Draws the residuals of two models against their fitted values in two
panels, side by side, to show what a remediation method (a weighted fit,
a transformation) did to the spread.

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

A `ggplot` object with one panel per model. Its data have the columns
`fitted`, `resid`, `model` (`"original"` or `"remedied"`) and `panel`.

## Details

A weighted fit is shown through its Pearson residuals \\\sqrt{w_i}\\
e_i\\, so the plot shows whether the weighting flattened the spread. Its
raw residuals would look as heteroscedastic as the original ones however
good the weights were.

Each panel has its own axes. Weighted residuals and the residuals of a
transformed response are not in the units of the original ones, and on a
common axis one of the two panels could be squeezed flat. The panels are
there to compare the shape of the two clouds, not their size.

## Earlier versions

Up to 0.12.0 the two sets of residuals were overlaid in one panel and
told apart by colour.

## Examples

``` r
data(mtcars)
m1 <- lm(mpg ~ wt, data = mtcars)
m2 <- fitWLS(m1)
plotBeforeAfter(m1, m2)
```
