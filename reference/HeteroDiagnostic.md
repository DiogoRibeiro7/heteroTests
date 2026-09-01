# Create a diagnostic object

Create a diagnostic object that exposes
[`test()`](https://diogoribeiro7.github.io/heteroTests/reference/test.HeteroDiagnostic.md),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
[`summary()`](https://rdrr.io/r/base/summary.html) methods. Formulas are
accepted for convenience.

## Usage

``` r
HeteroDiagnostic(model, data = NULL)
```

## Arguments

- model:

  A fitted lm or glm object, or a formula.

- data:

  Data used to fit the model when `model` is a formula.

## Value

An object of class `HeteroDiagnostic`.

## Examples

``` r
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 d <- HeteroDiagnostic(m, mtcars)
 test(d)
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 11.8225 df = 5 p = 0.0373
#> [INFO] Running Breusch-Pagan test
#> $white
#> 
#>  White's test for heteroscedasticity
#> 
#> data:  model
#> X-squared = 11.822, df = 5, p-value = 0.0373
#> alternative hypothesis: heteroscedasticity present
#> 
#> 
#> $breusch_pagan
#> 
#>  Breusch-Pagan test for heteroscedasticity
#> 
#> data:  mpg ~ wt + qsec
#> X-squared = 3.1348, df = 2, p-value = 0.2086
#> 
#> 
#> $vif
#>       wt     qsec 
#> 1.031487 1.031487 
#> 
#> $reset
#> 
#>  RESET test for nonlinearity
#> 
#> data:  mpg ~ wt + qsec
#> F = 6.7306, df1 = 2, df2 = 27, p-value = 0.00425
#> 
#> 
#> $influence
#> $influence$cooks_distance
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>        2.627038e-03        5.587076e-06        2.174253e-02        1.046036e-04 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>        5.288512e-04        5.101445e-02        1.927373e-02        2.178198e-02 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>        1.585198e-01        6.545298e-05        1.149236e-02        3.308687e-03 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>        1.797445e-06        1.115778e-02        2.598066e-03        3.367812e-02 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>        4.532124e-01        1.582375e-01        1.914813e-02        1.911855e-01 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>        1.021948e-01        1.061160e-02        2.016397e-02        1.116304e-02 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>        2.403811e-02        3.140692e-04        1.333611e-02        7.021574e-02 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>        2.048803e-02        1.368718e-03        7.784575e-04        5.703374e-03 
#> 
#> $influence$influential
#>          Merc 230 Chrysler Imperial          Fiat 128    Toyota Corolla 
#>                 9                17                18                20 
#> 
#> $influence$cutoff
#> [1] 0.137931
#> 
#> 
```
