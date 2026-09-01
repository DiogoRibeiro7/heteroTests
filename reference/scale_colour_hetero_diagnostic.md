# Colour scale for heteroscedasticity diagnostics

Provides a discrete palette used across diagnostic comparisons.

## Usage

``` r
scale_colour_hetero_diagnostic(...)

scale_fill_hetero_diagnostic(...)
```

## Arguments

- ...:

  Arguments passed to
  [`ggplot2::scale_colour_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html).

## Value

A ggplot2 scale.

## Examples

``` r
ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
  ggplot2::geom_point() +
  scale_colour_hetero_diagnostic()
```
