# Advanced simulation framework for test validation

Provides tools for validating heteroscedasticity tests under controlled
scenarios and estimating statistical power.

## Usage

``` r
simulate_type_I_errors(
  test_function,
  n_sims = 1000,
  alpha = 0.05,
  n_obs = 100,
  seed = 123
)

simulate_power_analysis(
  test_function,
  sigma_functions = list(sigma_linear),
  effect_sizes = c(0.1, 0.2, 0.5, 1),
  n_sims = 500,
  n_obs = 100,
  alpha = 0.05
)
```

## Arguments

- test_function:

  Function taking a model and data and returning an object with a
  numeric `p.value` component.

- n_sims:

  Number of simulation replications.

- alpha:

  Significance level for Type I error or power calculations.

- n_obs:

  Number of observations for generated datasets.

- seed:

  Optional integer seed for reproducibility.

- sigma_functions:

  List of variance functions generating heteroscedastic patterns.

- effect_sizes:

  Numeric vector of effect size multipliers.

## Value

A list summarising Type I error rate or a data frame of power estimates.
