# What every test returns

Every exported `perform*Test()` function returns a base-R `htest`
object, so a result prints, subsets and composes like the result of
[`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) or
[`lmtest::bptest()`](https://rdrr.io/pkg/lmtest/man/bptest.html). This
page lists every field such a result can hold: the five every test
returns, the two optional `htest` fields some return, and the fields a
few tests add. `tests/testthat/test-return-contract.R` checks every
exported test against it.

## Fields of every test

- `statistic`:

  the test statistic: a numeric value of length one, named after the
  statistic (`"X-squared"`, `"F"`, `"t"`, `"GQ"`, `"LM"`, `"z"`, ...).

- `parameter`:

  the parameters of the null distribution: a numeric vector whose
  elements are named, usually degrees of freedom (`"df"`, or `"df1"` and
  `"df2"`). The resampling tests store the number of replications here
  (`"B"`, `"permutations"`), and the tests on ordered observations the
  sizes they use (`"n"`, `"m"`). It is `NULL` for
  [`performPesaranTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performPesaranTest.md),
  whose statistic is referred to the standard normal distribution.

- `p.value`:

  the p-value: a single number between 0 and 1.

- `method`:

  a character string naming the test. When an argument switches between
  two statistics, such as `studentize` in
  [`performHarveyTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performHarveyTest.md),
  the string says which one was computed.

- `data.name`:

  a character string describing what was tested: the formula of the
  model, followed for some tests by the variance regressors. For
  [`performBoxMTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBoxMTest.md),
  which tests data rather than a model, it names the data and the
  grouping.

## Optional htest fields

- `alternative`:

  a character string describing the alternative hypothesis, returned by
  the tests that have a direction or a stated alternative.

- `estimate`:

  estimates that accompany the statistic: a named numeric vector
  ([`performSzroeterTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performSzroeterTest.md),
  [`performSpearmanTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performSpearmanTest.md),
  [`performVarianceFormTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performVarianceFormTest.md)),
  or, in
  [`performQuantileRegressionTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performQuantileRegressionTest.md),
  a matrix of slope estimates with one column per quantile and named
  rows and columns.

## Fields some tests add

- `p.value_bootstrap`:

  [`performWhiteTestRobust()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestRobust.md)
  and
  [`performBPTestRobust()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTestRobust.md)
  with `bootstrap = TRUE`: the bootstrap p-value.

- `robust_details`:

  [`performWhiteTestRobust()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestRobust.md)
  and
  [`performBPTestRobust()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTestRobust.md):
  a list with the confidence interval, effect size, bootstrap results,
  power and sample size, and `method` or `studentized`, the setting that
  chose the statistic.

- `boot_statistics`:

  [`performWhiteTestBootstrap()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestBootstrap.md):
  the bootstrap replicates of the statistic.

- `bootstrap`:

  [`performWildBootstrapTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWildBootstrapTest.md):
  a list with the replicates, the number of usable ones, the multiplier
  distribution and the observed statistic.

- `permutation`:

  [`performRankPermutationTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performRankPermutationTest.md):
  a list with the replicates, the number of usable ones and the ordering
  variable.

- `projection`:

  [`performHighDimensionalTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performHighDimensionalTest.md):
  a list with the number of principal components kept, the share of the
  variance they explain and the threshold that chose them.

- `variance_function`:

  [`performVarianceFormTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performVarianceFormTest.md):
  a list with the form of the variance function, its regressors and the
  added terms that were tested.

- `quantiles`, `se_type`, `iid`:

  [`performQuantileRegressionTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performQuantileRegressionTest.md):
  the quantiles compared and the settings of the standard errors.

- `chunks_processed`, `chunk_size`, `r_squared`:

  [`performWhiteTestStreaming()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestStreaming.md),
  [`performBPTestStreaming()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTestStreaming.md)
  and
  [`performKoenkerTestStreaming()`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTestStreaming.md):
  how the data were read and the \\R^2\\ of the auxiliary regression.

- `res`:

  [`performSpatialHeteroTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performSpatialHeteroTest.md):
  the Monte Carlo replicates of Moran's I from
  [`spdep::moran.mc()`](https://r-spatial.github.io/spdep/reference/moran.mc.html),
  whose class `"mc.sim"` the result keeps.

## Attributes

[`performWhiteTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTest.md)
and
[`performWhiteTestRobust()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestRobust.md)
attach the attributes `auxiliary_regressors`, `original_regressors`,
`cross_products_included` and `r_squared_auxiliary`, which describe the
auxiliary regression.
[`runHeteroTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md)
adds attributes of its own to each result it returns.

## See also

[`runHeteroTests`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md),
[`t.test`](https://rdrr.io/r/stats/t.test.html) for the `htest` class
