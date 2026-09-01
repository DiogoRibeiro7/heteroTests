# Run registered diagnostic plots

Run registered diagnostic plots

## Usage

``` r
runDiagnosticPlots(
  model,
  plots = c("residuals_fitted", "spread_level", "density", "qq", "bubble_variance")
)
```

## Arguments

- model:

  A fitted `lm` or `glm` object

- plots:

  Character vector of plot names to generate

## Value

Named list of ggplot objects

## Examples

``` r
runDiagnosticPlots(lm(mpg ~ wt, mtcars))
#> $residuals_fitted
#> `geom_smooth()` using formula = 'y ~ x'

#> 
#> $spread_level
#> `geom_smooth()` using formula = 'y ~ x'

#> 
#> $density

#> 
#> $qq

#> 
#> $bubble_variance

#> 
```
