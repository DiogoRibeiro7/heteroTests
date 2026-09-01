# Run a suite of model diagnostics

Combines heteroscedasticity checks with tests for collinearity,
nonlinearity and influential observations.

## Usage

``` r
runDiagnostics(
  model,
  data = NULL,
  tests = c("white", "breusch_pagan"),
  power = 2:3,
  use_cache = TRUE,
  chunk_threshold_mb = 100,
  chunk_size = 10000,
  progress = interactive()
)
```

## Arguments

- model:

  A fitted `lm` model or a formula.

- data:

  Optional data frame if `model` is a formula.

- tests:

  Heteroscedasticity tests to run.

- power:

  Powers for `performRESETTest`.

- use_cache:

  Logical flag forwarded to
  [`runHeteroTests`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md)
  controlling whether cached diagnostic results may be reused.

- chunk_threshold_mb:

  Numeric threshold passed to
  [`runHeteroTests`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md)
  for enabling streaming diagnostics on large datasets.

- chunk_size:

  Integer chunk size supplied to
  [`runHeteroTests`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md)
  when streaming diagnostics.

- progress:

  Logical flag controlling whether textual progress indicators are
  displayed during long-running operations.

## Value

A list with the results of all diagnostics.

## Examples

``` r
data(mtcars)
runDiagnostics(mpg ~ wt + qsec, mtcars)
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
