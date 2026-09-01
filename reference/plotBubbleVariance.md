# Bubble plot of residual variance by covariate

Displays residual magnitude against a predictor with bubble size
proportional to `|residual|`.

## Usage

``` r
plotBubbleVariance(model, variable = NULL)
```

## Arguments

- model:

  A fitted model of class `lm` or `glm`.

- variable:

  Optional name of a covariate from the model to plot against.

## Value

A `ggplot` object.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
plotBubbleVariance(m, "wt")
```
