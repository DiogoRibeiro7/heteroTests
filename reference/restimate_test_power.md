# Estimate test power from observed effect size

Uses the observed chi-squared statistic and effect size to approximate
the achieved power of a heteroscedasticity test and to suggest an
increased sample size required to attain the desired power threshold.

## Usage

``` r
restimate_test_power(
  test_result,
  model,
  data,
  alpha = 0.05,
  target_power = 0.8,
  max_multiplier = 5
)
```

## Arguments

- test_result:

  An object returned by `perform*Test()` functions with `statistic` and
  `parameter` components.

- model:

  Fitted model used in the diagnostic.

- data:

  Data frame supplied to the diagnostic.

- alpha:

  Significance level used in the test. Defaults to 0.05.

- target_power:

  Desired minimum power for the recommendation.

- max_multiplier:

  Upper bound multiplier relative to the observed sample size when
  searching for the recommended sample size.

## Value

A list with elements `power`, `recommended_n`, and `details` containing
auxiliary information (degrees of freedom, non-centrality parameter, and
a power curve over a grid of effect sizes).
