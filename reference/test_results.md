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

## Inputs every test refuses

Since 0.15.0 every test on a model refuses four inputs, each with one
message: a `glm` that is not a least-squares fit, that is, of any family
but `gaussian` or with any link but the identity ("needs a model fitted
by stats::lm()"); a model with a coefficient that is not finite, such as
the `NA` of an aliased term ("Model coefficients must be finite");
`data` that lack rows the model was fitted to; and `data` that lack a
variable of the model ("The following variables are missing from"). The
model frame is accepted as `data` as it is. A `glm` with the Gaussian
family and the identity link is accepted and gives the result of the
`lm` up to rounding error, except that
[`performWhiteTestStreaming()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestStreaming.md),
[`performHarrisonMcCabeTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performHarrisonMcCabeTest.md)
and
[`performVarianceFormTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performVarianceFormTest.md)
refuse every `glm`. With positive prior weights it gives the result of
`lm(..., weights = w)`:
[`performGQTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performGQTest.md),
[`performRESETTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performRESETTest.md),
[`performWhiteTestBootstrap()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestBootstrap.md)
and
[`performWildBootstrapTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWildBootstrapTest.md),
which read the weights of an `lm`, read the prior weights of the `glm`,
and
[`performQuantileRegressionTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performQuantileRegressionTest.md),
which refuses a weighted `lm`, refuses it with the same message. A
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) fit is
refused by every test ("The model is a
[`survey::svyglm()`](https://rdrr.io/pkg/survey/man/svyglm.html) fit").
`tests/testthat/test-input-validation.R` gives every test these inputs.

The tests that read `data` also refuse `data` whose rows cannot be
confirmed to hold the observations of the fit ("cannot match the rows of
`data` to the observations of the model"). They match the rows of `data`
to the residuals by row name, and a tibble, a data.table or a data frame
whose row names were reset, as by
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html),
has row names that only number its rows. Before a test reads `data`, the
rows the row names pick are compared with the model frame of the fit,
row by row: every column `data` holds under its own name, value by
value, and the others (`log(x)`, `poly(x, 2)`, `offset(o)`, the weights
and an offset given as arguments) computed from `data` as the fit
computed them. A term computed from the rows it is given, such as
`I(x - mean(x))`, and a column that names a variable `data` does not
hold, are not compared; for a term of variables of `data`, the compared
columns must then tell the rows of the fit apart. Data that cannot be
confirmed so are refused, even the data of the fit: when no column can
be compared, as with `lm(d$y ~ d$x)`, or when the compared columns tie
on rows that differ in such a term. Pass the full data the model was
fitted on, or the variables as columns of `data`. Data without row names
of their own in another order than the fit, or with rows in front of
those of the fit, are refused, as are repeated row names, and a model
frame rebuilt for a fit with `model = FALSE` from data that changed
after the fit. Data with row names of their own are matched by name, in
any order, except by
[`performPesaranTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performPesaranTest.md),
[`performBPRandomEffectsTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPRandomEffectsTest.md)
and
[`performScatterDiagnostic()`](https://diogoribeiro7.github.io/heteroTests/reference/performScatterDiagnostic.md),
which read `data` by position and need its rows in the order of the fit.
`tests/testthat/test-data-alignment-guard.R` checks these refusals.

## See also

[`runHeteroTests`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md),
[`t.test`](https://rdrr.io/r/stats/t.test.html) for the `htest` class
