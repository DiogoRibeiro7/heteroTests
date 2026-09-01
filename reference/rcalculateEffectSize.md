# Effect size calculations for heteroscedasticity diagnostics

Converts chi-squared statistics produced by heteroscedasticity tests
into interpretable effect sizes such as Cramer's V, the phi coefficient,
or an eta-squared analogue. The helper also provides qualitative
magnitude descriptors and a brief interpretation string that can be
surfaced to users.

## Usage

``` r
rcalculateEffectSize(
  test_result,
  model,
  data,
  type = c("cramers_v", "phi", "eta_squared")
)
```

## Arguments

- test_result:

  Result object from a heteroscedasticity test (typically of class
  `htest`).

- model:

  Fitted model supplied to the diagnostic.

- data:

  Data frame containing the variables referenced in `model`.

- type:

  Effect size metric to compute. Supported options are `"cramers_v"`,
  `"phi"`, and `"eta_squared"`.

## Value

A named list with elements `effect_size`, `magnitude`,
`practical_significance`, `interpretation`, and `type`.

## Examples

``` r
data(mtcars)
model <- lm(mpg ~ wt + cyl, data = mtcars)
result <- performWhiteTest(model, mtcars)
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 8.0275 df = 5 p = 0.1547
rcalculateEffectSize(result, model, mtcars)
#> $effect_size
#> [1] 0.2239913
#> 
#> $magnitude
#> [1] "small"
#> 
#> $practical_significance
#> [1] FALSE
#> 
#> $interpretation
#> [1] "Effect size 0.224 (cramers_v) suggests limited practical impact."
#> 
#> $type
#> [1] "cramers_v"
#> 
```
