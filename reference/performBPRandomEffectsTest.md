# Breusch-Pagan test for random effects

Lagrange Multiplier test for heteroscedasticity in random effects
models.

## Usage

``` r
performBPRandomEffectsTest(model, data, id)
```

## Details

The statistic is calculated as \\n \sum \bar{e}\_i^2 / \hat{\sigma}^2\\,
where \\\bar{e}\_i\\ are average residuals by individual and
\\\hat{\sigma}^2\\ is the pooled error variance. It follows a chi-square
distribution with one degree of freedom.

## Arguments

- model:

  an object of class `lm`.

- data:

  data frame used to fit `model`.

- id:

  individual identifier column.

## Value

An object of class `htest`.

## References

Breusch, T. S., & Pagan, A. R. (1980). The Lagrange Multiplier Test and
Its Applications to Model Specification in Econometrics. *Review of
Economic Studies*, 47(1), 239–253.

## Examples

``` r
 df <- data.frame(id = rep(1:5, each = 4), time = rep(1:4, 5), x = runif(20), y = rnorm(20))
 m <- lm(y ~ x, data = df)
 performBPRandomEffectsTest(m, df, "id")
#> 
#>  Breusch-Pagan LM test for random effects
#> 
#> data:  y ~ x
#> LM = 1.7975, = 1, p-value = 0.18
#> 
```
