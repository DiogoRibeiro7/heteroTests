# Machine-learning residual analysis

Fits a GAM using `mgcv` and compares its residuals to the input linear
model.

## Usage

``` r
analyzeMLResiduals(model, data = NULL)
```

## Arguments

- model:

  A fitted `lm` object or a formula.

- data:

  Data frame used if `model` is a formula.

## Value

A list with the GAM model, residuals and RMSE reduction.

## See also

[`compareModelDiagnostics`](https://diogoribeiro7.github.io/heteroTests/reference/compareModelDiagnostics.md)

## Examples

``` r
 data(mtcars)
 analyzeMLResiduals(mpg ~ wt + qsec, mtcars)
#> $gam_model
#> 
#> Family: gaussian 
#> Link function: identity 
#> 
#> Formula:
#> mpg ~ wt + qsec
#> Total model degrees of freedom 3 
#> 
#> GCV score: 7.43738     
#> 
#> $lm_residuals
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>         -0.81510855         -0.04822401         -2.52727880         -0.18056924 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>          0.50388581         -2.96858808         -2.14342291          2.17288034 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>         -2.32371308         -0.18548760         -2.14300639          1.03101923 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>          0.02886576         -2.19041433          0.44870314          1.47572368 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>          5.74861230          5.66785310          1.59752172          4.92578455 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>         -4.39619858         -2.15289593         -3.28152953         -1.38091265 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>          3.02044258         -0.24021927          1.53885259          2.58792829 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>         -1.41749041         -0.46588119         -0.29121742         -1.59591510 
#> 
#> $gam_residuals
#>  [1] -0.81510855 -0.04822401 -2.52727880 -0.18056924  0.50388581 -2.96858808
#>  [7] -2.14342291  2.17288034 -2.32371308 -0.18548760 -2.14300639  1.03101923
#> [13]  0.02886576 -2.19041433  0.44870314  1.47572368  5.74861230  5.66785310
#> [19]  1.59752172  4.92578455 -4.39619858 -2.15289593 -3.28152953 -1.38091265
#> [25]  3.02044258 -0.24021927  1.53885259  2.58792829 -1.41749041 -0.46588119
#> [31] -0.29121742 -1.59591510
#> 
#> $rmse_reduction
#> [1] 0
#> 
```
