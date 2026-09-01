# Identify influential observations

Uses Cook's distance to flag influential observations.

## Usage

``` r
performInfluenceDiagnostics(model, cutoff = NULL)
```

## Arguments

- model:

  A fitted `lm` model.

- cutoff:

  Cook's distance threshold; defaults to `4/(n - p)`.

## Value

A list with Cook's distances and indices of influential points.

## Examples

``` r
data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
performInfluenceDiagnostics(m)
#> $cooks_distance
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
#> $influential
#>          Merc 230 Chrysler Imperial          Fiat 128    Toyota Corolla 
#>                 9                17                18                20 
#> 
#> $cutoff
#> [1] 0.137931
#> 
```
