# Automatic transformation helper

Chooses between no transformation, log, square root and Box–Cox based on
the Breusch-Pagan test statistic.

## Usage

``` r
autoTransform(model)
```

## Arguments

- model:

  A fitted model of class `lm`.

## Value

A list with elements `model` (the transformed fit) and `method`
indicating the chosen transformation.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
autoTransform(m)
#> [INFO] Running Breusch-Pagan test
#> [INFO] Running Breusch-Pagan test
#> [INFO] Running Breusch-Pagan test
#> [INFO] Running Breusch-Pagan test
#> $model
#> 
#> Call:
#> lm(formula = formula_trans, data = df)
#> 
#> Coefficients:
#> (Intercept)           wt         qsec  
#>      4.4394      -0.5646       0.1015  
#> 
#> 
#> $method
#> [1] "sqrt"
#> 
#> $lambda
#> [1] NA
#> 
```
