# Plot residuals vs fitted values

Generates a simple scatter plot of residuals against fitted values from
a linear model. A horizontal reference line at zero is added.

## Usage

``` r
plotResidualsFitted(model, enhanced = FALSE)
```

## Arguments

- model:

  A fitted model of class `lm`.

- enhanced:

  Logical. `FALSE`, the default, draws the simple plot; `TRUE` the plot
  with influential observations marked.

## Value

A `ggplot` object.

## Details

For a weighted fit the Pearson residuals \\\sqrt{w_i}\\ e_i\\ are
plotted, as in `plot.lm()`: those are the residuals that have constant
variance when the weights are right. The other residual plots in the
package do the same.

With `enhanced = TRUE` the plot marks influential observations (Cook's
distance above \\4/n\\) in red, sizes the points by the absolute
residual and adds the standard-error band of the LOESS smooth. This is
what the deprecated
[`plotResidualsFittedEnhanced()`](https://diogoribeiro7.github.io/heteroTests/reference/plotResidualsFittedEnhanced.md)
drew.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
plotResidualsFitted(m)
#> `geom_smooth()` using formula = 'y ~ x'

plotResidualsFitted(m, enhanced = TRUE)
#> `geom_smooth()` using formula = 'y ~ x'
```
