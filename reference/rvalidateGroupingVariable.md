# Validate grouping variable structure

Ensures that grouping variables supplied to heteroscedasticity tests
satisfy minimum size and level requirements.

## Usage

``` r
rvalidateGroupingVariable(data, group_var, min_group_size = 3, min_groups = 2)
```

## Arguments

- data:

  Data frame providing the grouping column.

- group_var:

  Name of the grouping variable to inspect.

- min_group_size:

  Minimum number of observations required in each group. Defaults to
  `3`.

- min_groups:

  Minimum number of groups that must be represented. Defaults to `2`.

## Value

A list mirroring the structure of
[`rvalidateDistributionalAssumptions()`](https://diogoribeiro7.github.io/heteroTests/reference/rvalidateDistributionalAssumptions.md)
with information about the evaluated grouping variable.

## Examples

``` r
grp <- heteroTests:::rvalidateGroupingVariable(mtcars, group_var = "cyl")
grp$details$n_groups
#> NULL
```
