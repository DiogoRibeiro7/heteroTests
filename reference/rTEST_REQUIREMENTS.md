# Test-specific sample size requirements

Central catalogue of the minimum sample sizes required by
heteroscedasticity diagnostics supported in the package. Each entry
either provides a requirements list with human-readable rationale or a
function that computes the minimum number of observations from
contextual arguments (e.g. lag lengths for dynamic tests).

## Usage

``` r
rTEST_REQUIREMENTS
```

## Format

A named list keyed by lowercase test identifiers.

## Details

Static entries are named lists with some combination of the following
components:

- `min_obs`:

  Overall minimum observation count.

- `min_obs_per_group`:

  Minimum number of observations that each group should contribute.

- `reason`:

  Short explanation describing why the requirement exists.

Dynamic entries are functions that receive additional arguments supplied
to
[`rvalidateSampleSize()`](https://diogoribeiro7.github.io/heteroTests/reference/rvalidateSampleSize.md)
and should return either a numeric minimum or a list containing
`min_obs` and, optionally, `reason`.

## Examples

``` r
heteroTests:::rTEST_REQUIREMENTS$white$min_obs
#> [1] 20

# Dynamic entry: ARCH LM depends on the lag order
heteroTests:::rTEST_REQUIREMENTS$arch_lm(lags = 3)
#> [1] 11
```
