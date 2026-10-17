<!-- Hand-written page: tools/build-docs.R does not generate it. The table was measured by running every exported test on a fit of each class; update it by hand when a test changes what it accepts, with tests/testthat/test-model-eligibility.R and test-input-validation.R, which check it. -->

# Supported models

The tests of the package examine the residuals of a least-squares fit for a
variance that is not constant. They accept the models whose residuals are
those of a least-squares fit, and refuse the others when they are called,
before they compute anything, each with one message. This page says which
test accepts which class of model, and why; the rules, and the messages, are
in [What every test returns](reference/test_results.md), under *Supported
models* and *Inputs every test refuses*.

The table was measured, not written from the code: every exported test was
called as in `tests/testthat/helper-test-recipes.R`, on a fit of each class to
the 120 observations of `contract_data()`, on 0.15.0 and on the development
version. A cell in bold changed in 0.16.0.

## The classes, and why

One line for each class and each answer in the table.

| Class | Answer | Why |
| --- | --- | --- |
| `lm` | yes | Its residuals are least-squares residuals, the ones the null distributions of the tests are derived for. |
| `lm(..., weights = w)` | yes | `lm()` states that the error variance is $\sigma^2 / w_i$; the tests read the Pearson residuals $\sqrt{w_i}\, e_i$, the residuals of the unweighted regression of $\sqrt{w_i}\, y_i$ on $\sqrt{w_i}\, x_i$, which have constant variance when the weights are right. |
| | no: weighted | `performQuantileRegressionTest()` refits the model without the weights, which would test another model. |
| | no: not `fitWLS()` | `performVarianceFormTest()` tests the variance function `fitWLS()` estimated; weights given by hand have none to test. |
| An `lm` with a zero weight | no: zero weight | The observation is not part of the fit, and its residual is an artificial zero that the test would count as an observation. |
| | yes | `performRESETTest()` compares two weighted least-squares fits, which both leave the observation out: its value is that of the fit to the observations with positive weight. |
| Gaussian `glm`, identity link | yes | A least-squares fit: its coefficients, fitted values and residuals are those of `lm()`, up to rounding, and the tests give the result of the `lm`. |
| | no: any `glm` | `performWhiteTestStreaming()`, `performHarrisonMcCabeTest()` and `performVarianceFormTest()` refuse every `glm`, as they did before 0.15.0. This is a restriction of their code, not of their mathematics: a Gaussian `glm` with the identity link is a least-squares fit. |
| Gaussian `glm` with prior weights | yes | Its prior weights, `prior.weights`, are the weights of `lm(..., weights = w)`, and the tests read them. Its `weights` are the working weights of the last iteration of iteratively reweighted least squares, $w_i (d\mu_i / d\eta_i)^2 / V(\mu_i)$, which for this family and link are the prior weights; the tests do not read them. |
| Gaussian `glm` with a zero prior weight | no: zero weight | As for the `lm`. Before 0.16.0 most tests read no weights for a `glm` and tested the artificial zero. |
| Binomial, Poisson or any other `glm` | no: not least squares | The variance of its residuals depends on the mean by construction, so a test of constant variance tests nothing. |
| `survey::svyglm()` | no: survey | Its residuals are scaled by the sampling weights, which are not precision weights: a homoscedastic model is rejected whenever they vary with the regressors. `runSurveyHeteroTests()` fits the model by least squares to the data of the design instead. |
| `plm::plm()` | no: not `lm` | The panel tests, `performPesaranTest()` and `performBPRandomEffectsTest()`, take the pooled `lm` of the panel and the columns of `data` that name the units and the periods. A `plm` fit is not an `lm`. |
| A rank-deficient fit | yes | An aliased column, a linear combination of the others, does not change the column space of the design, so the residuals and fitted values are those of the model fitted without it. Since 0.16.0 the tests drop the aliased columns from the design they build and take their degrees of freedom from the rank: the value is that of the model fitted without them. |
| | no | A test that refuses the model without the aliased columns, for one of the reasons above, refuses the rank-deficient one with the same message. |
| | yes, not the reduced value | `performVarianceFormTest(form = "power")` takes the logarithms of every column of the design, the aliased ones included; the logarithm of an aliased column is not a combination of the logarithms of the others, so the value is not that of the model without the aliased columns. It does not depend on the order of the terms, as the value of that model does. With the exponential form, the default, the value is that of the model without them. |
| An `NA` from an empty cell | yes | The dummy of a cell of an interaction that no observation falls in is aliased: as for any rank-deficient fit. |
| | error, as reduced | The squares of the dummies are collinear, and `performWhiteTestStreaming()` stops, as it does on the model fitted without the aliased column; `performWhiteTest()` drops such terms. |
| `MASS::rlm()` | as today | An `lm` by class, so 37 tests accept it, as before 0.16.0, and test its M-estimation residuals as if they were least-squares residuals. Whether that is a test of anything is an open decision (below). |
| | no: weighted, no: not `fitWLS()`, no: refit differs | Its case weights, all 1 unless given, are read as weights; `bootstrapTestStatistic()` refits each replicate by least squares, which does not reproduce an M-estimate. |
| `fitWLS()` | yes | A weighted `lm` whose weights were estimated. The reference distributions take the weights as known, so they are approximate; `performVarianceFormTest()` accounts for the estimation. |

Every test also refuses a coefficient that is infinite, `NaN`, or `NA` without
the fit being rank-deficient, and `data` that lack rows or variables of the
fit, or whose rows cannot be shown to be those of the fit. Anything that is
not an `lm` or a `glm`, a `plm` fit among them, is refused by every test with
one message, which names the class received and points to the pooled `lm` the
panel tests take.

### Which model without the aliased columns

The model fitted without the aliased columns is the one `lm()` keeps: its
decomposition keeps a column unless it is a combination of the columns before
it, so of an aliased set the column that comes last, in the order of the
terms, is left out (in `y ~ x1 + x2 + x_sum`, with `x_sum = x1 + x2`, it is
`x_sum`; in `y ~ x_sum + x1 + x2`, it is `x2`). Most statistics depend only on
the space the columns span, and are the same whichever column is left out.
These depend on which:

- `performWhiteTest()`, and so `performWhiteTestRobust()`,
  `performWhiteTestStreaming()` and `performWhiteTestBootstrap()`, when the
  cross products are left out (`cross_products = FALSE`, `method = "reduced"`,
  or more regressors than `max_interactions`): the squares are those of the
  columns kept. With the cross products, the default, they span the same space
  either way.
- `performHighDimensionalTest()` when it keeps fewer principal components than
  there are columns.

### Offsets

An offset, given as `offset = o` or written as `offset(o)`, is part of the fit,
and most tests read the residuals of the fit, which include it. Four ignore it
today, so that on a model with an offset they do not give the value of the
same model fitted to the response minus the offset:

- `performRESETTest()` refits the augmented model without the offset;
- `performGQTest()` refits each segment without an offset given as an
  argument, and, on a weighted fit, without any offset;
- `performQuantileRegressionTest()` fits `quantreg::rq()`, which ignores an
  offset;
- the studentized Breusch-Pagan statistic of `performStudentizedBPTest()` and
  of `performBPTestRobust()`, and the registered `"student_bp"`, refits the
  design without the offset.

They are recorded in the roadmap; whether to correct them or refuse an offset
is open. The tests built on the fitted values (the Cook-Weisberg, Spearman,
spread-level and Davidian-Carroll tests, and Harvey's with
`auxiliary = "fitted"`) read $X\hat\beta$ plus the offset, as the fit defines
it.

## The table

| Test | lm | lm, weights | lm, a zero weight | Gaussian glm | Gaussian glm, weights | Gaussian glm, a zero weight | binomial glm | Poisson glm | svyglm, Gaussian | svyglm, binomial | plm | rank-deficient lm | rank-deficient lm, weights | rank-deficient Gaussian glm | NA from an empty cell | MASS::rlm | fitWLS() |
| --- | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| `performWhiteTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performWhiteTestStreaming()` | yes | yes | no: zero weight | no: any glm | no: any glm | no: any glm | no: any glm | no: any glm | no: survey | no: survey | no: not lm | **yes** | **yes** | no: any glm | **error, as reduced** | as today | yes |
| `performWhiteTestRobust()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performBPTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performBPTestStreaming()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performBreuschPaganTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performBPTestRobust()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performGQTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performKoenkerTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performKoenkerTestStreaming()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performParkTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performStudentizedBPTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performWhiteTestBootstrap()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performSzroeterTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performHarrisonMcCabeTest()` | yes | yes | no: zero weight | no: any glm | no: any glm | no: any glm | no: any glm | no: any glm | no: survey | no: survey | no: not lm | **yes** | **yes** | **no: any glm** | **yes** | as today | yes |
| `performWildBootstrapTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performQuantileRegressionTest()` | yes | no: weighted | no: weighted | yes | no: weighted | no: weighted | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | no: weighted | **yes** | **yes** | no: weighted | no: weighted |
| `performRankPermutationTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performHighDimensionalTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performSpatialHeteroTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performSpearmanTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performLeveneTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performGlejserTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performBrownForsytheTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performFlignerKilleenTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performBartlettTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performHarveyTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performArchLMTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performMcLeodLiTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performHartleyFmaxTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performOBrienTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performCookWeisbergTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performNCVTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performSpreadLevelTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performPesaranTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performBPRandomEffectsTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performDavidianCarrollTest()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performVarianceFormTest()` | yes | no: not fitWLS() | no: not fitWLS() | no: any glm | no: any glm | no: any glm | no: any glm | no: any glm | no: survey | no: survey | no: not lm | **yes** | **no: not fitWLS()** | no: any glm | **yes** | no: not fitWLS() | yes |
| `performRESETTest()` | yes | yes | yes | yes | yes | yes | no: not least squares | no: not least squares | no: survey | no: survey | no: not lm | **yes** | **yes** | **yes** | **yes** | as today | yes |
| `performScatterDiagnostic()` | yes | yes | no: zero weight | yes | yes | **no: zero weight** | **no: not least squares** | **no: not least squares** | no: survey | no: survey | no: not lm | yes | yes | yes | yes | as today | yes |
| `bootstrapTestStatistic()` | yes | yes | no: zero weight | no: any glm | no: any glm | no: any glm | no: any glm | no: any glm | no: any glm | no: any glm | no: not lm | **yes** | **yes** | **no: any glm** | **yes** | no: refit differs | yes |

`performBreuschPaganTest()`, `performStudentizedBPTest()` and
`rbootstrap_test_statistic()` are deprecated names; `bootstrapTestStatistic()`
was given `performKoenkerTest()`. On `MASS::rlm()` the studentized
Breusch-Pagan statistic of `performStudentizedBPTest()` and of
`performBPTestRobust()` refits the design by least squares, so it tests the
least-squares fit (9.04 in this design, the value of the `lm`), where
`performKoenkerTest()` tests the residuals of the M-estimate (8.74).

## The routes

The runners and the registry call the tests, and inherit what they refuse:

- `runHeteroTests()` and `runDiagnostics()` stop with the refusal of the first
  test. `data` whose rows cannot be matched to the fit are refused with no
  fallback. Since 0.16.0 every fallback refuses what the test it stands in
  for refuses because of the model or the rows and variables of `data`, so
  no name holds a test computed regardless of them. A test refused for a
  reason of its own, such as the quantile regression test on a weighted fit,
  still falls back on another test under its name, tagged
  `attr(, "extras")$status == "fallback"` (an open decision for 1.0.0, in the
  roadmap).
- The registry names, `cachedTest()`, `runHeteroTestsParallel()` and
  `test_factory$run_test()` call the same tests. `test_factory` leaves a
  rank-deficient fit to a function of the package, whoever registered it, and
  refuses it, as before 0.16.0, for a function defined outside the package,
  such as one a user writes.
- `runPanelTests()` and `runTimeSeriesTests()` refuse anything but an `lm` or a
  `glm` with the message of the tests, then call them.
- `runHeteroTests()` tests a tidymodels workflow on the model frame of its
  engine's fit, with `data = NULL`. Given `data`, such as the data the
  workflow was fitted to, it refuses them when they lack the variables of that
  fit, which names its response `..y`, and says to pass `data = NULL`.
- `runSurveyHeteroTests()` fits the model by least squares to the data of a
  design, and is never given a `svyglm()` fit.
- `runMultivariateTests()` runs Box's M test on data and a grouping; a test of
  a model run through it is given the data in place of a model, and refuses
  them.
- `checkModel()` checks the class of a model only, `lm` or `glm`: it accepts
  every class in the table but `plm`.

## Not covered

`performVIFDiagnostic()`, `performInfluenceDiagnostics()` and the plots are
diagnostics of the fit, not tests of its variance, and accept any `lm` or
`glm`.

## Open decisions

- `MASS::rlm()`: keep accepting it, as above, or refuse it as 0.15.0 refused
  a `glm` that is not a least-squares fit.
