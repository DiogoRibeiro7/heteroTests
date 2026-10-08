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

## Details

For a weighted fit the Pearson residuals \\\sqrt{w_i}\\ e_i\\ are
plotted, as in `plot.lm()`: those are the residuals that have constant
variance when the weights are right. The other residual plots in the
package do the same.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
plotResidualsFitted(m)
#> `geom_smooth()` using formula = 'y ~ x'
```
