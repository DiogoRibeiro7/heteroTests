<!-- Hand-written page: tools/build-docs.R does not generate it. It is the design of the model context (R/model_context.R), decided on 2026-10-18; update the status of the increments as they land. -->

# Design: the model context

The tests of the package read a fitted model and, often, the data it was
fitted to. Until 0.16.0 each test assembled what it read on its own: it
rebuilt the design matrix from `data`, dropped incomplete rows, matched the
rest to the residuals by row name or by position, and read the weights and the
offset again from the call of the fit. The model context replaces that with
one internal, validated object per call, built from the stored fit. This page
is its design: the contract the tests rely on, the representation, the plan
for moving the tests onto it, the tests that pin it, and the decisions behind
it.

The context is internal. No export, argument or default changes, as the
0.16.0 milestone requires ("no API change"); what changes for users is listed
under [Compatibility](#compatibility) and in the
[release notes](../releases.md).

**Status.** Increment 1 is in 0.16.0: the context itself, and
`performWhiteTest()`, `performBPTest()` (with its deprecated name
`performBreuschPaganTest()`), `performKoenkerTest()`,
`performRankPermutationTest()` and `performHighDimensionalTest()` built on
it. Increment 2 is in 0.16.0 too: the studentized Breusch-Pagan statistic
(`.studentized_bp_test()`), `performRESETTest()`, `performGQTest()` and
`performQuantileRegressionTest()` built on it, with the offset of the fit in
every refit. Increment 3 is in 0.16.0 too: `performGlejserTest()`,
`performParkTest()`, `performSzroeterTest()` with the registered `"szroeter"`,
`performHarrisonMcCabeTest()`, with the rank of the fit (decision 21),
`performHartleyFmaxTest()` and `performSpatialHeteroTest()` built on it.
Increment 4 is in 0.16.0 too: the five group tests, `performLeveneTest()`,
`performBrownForsytheTest()`, `performFlignerKilleenTest()`,
`performBartlettTest()` and `performOBrienTest()`, built on it, their copies of
the alignment gone. Increment 5 is in 0.16.0 too: the two panel tests,
`performPesaranTest()` and `performBPRandomEffectsTest()`, and so
`runPanelTests()`, and `performScatterDiagnostic()`, built on it, reading `id`,
`time` and `vars` at the confirmed rows (`rcontext_columns_at_fit()`), with the
repeated pairs of `id` and `time` (decision 13) and an unbalanced panel
(decision 28) refused by Pesaran's test; the pull request of N12 lifted the
second refusal, and the test computes the statistic of an unbalanced panel
from the periods each pair of units shares, as `plm::pcdtest()` does.
Increment 6 is in 0.16.0 too: the
tests computed from the model alone, `performSpearmanTest()`,
`performHarveyTest()`, `performArchLMTest()`, `performMcLeodLiTest()`,
`performCookWeisbergTest()` and `performNCVTest()`, `performSpreadLevelTest()`,
`performDavidianCarrollTest()` and `performVarianceFormTest()`, with
`fitWLS()`, built on it, matching `data` as every test does (decision 31), so
that it refuses a stale `model = FALSE` frame (N6); a second formula,
`var_formula` or `against`,
evaluated on the rows the alignment confirmed (B9), its variables never read
from the workspace (R3, decision 9; `rcontext_formula_rows()`). Increment 7 is
in 0.16.0 too: the three streaming tests, `performWhiteTestStreaming()`,
`performBPTestStreaming()` and `performKoenkerTestStreaming()`, built on it,
reading the stored design chunk by chunk (`rstreaming_design()`, in
`R/performance.R`). Increment 8, the first of 0.17.0, is in the development
version: the resampling tests, `performWhiteTestBootstrap()`, whose observed
statistic and replicates moved together, and `performWildBootstrapTest()`,
`bootstrapTestStatistic()` and the robust White and Breusch-Pagan tests,
`performWhiteTestRobust()` and `performBPTestRobust()`, built on it, each
replicate refitted by least squares on the stored design
(`rreplicate_lm()`, in `R/bootstrap_methods.R`), and a Gaussian `glm`
bootstrapped as the `lm` it is (N8). Increment 9, the second of 0.17.0, is in
the development version too: a fit with `na.action = na.exclude`, which the
tests refused and the panel tests and the scatter diagnostic read by
position, is read as the same fit with `na.omit`, the checks of the model and
every test reading its residuals, fitted values, weights and leverages without
the padding of `na.exclude` (`rmodel_unpad()`, `rcontext_unpad()`), and an
output with one value per row padded as `naresid()` pads it (B13).
Increment 10, the third of 0.17.0, is in the development version too: the
runners and the clean-up. A grouped run of a `fitWLS()` fit, which refitted
each group without the weights it estimated, is refused (B11, decision 10);
the data a tidymodels workflow, or a parsnip fit of `fit_xy()`, was fitted to
are mapped to the variables of its engine, a transformed outcome among them
(decision 35), and its model frame is tested, as without `data` (N5); `recommendRemediationStrategies()`,
`generateHeteroRecommendations()` and `generics::augment()` of the results of
the runner match the rows of `data` to the observations of the fit through
the alignment of the context (`rcontext_data_rows()`), `augment()` binding them
as broom does (decision 34), the results keeping the blueprint of a workflow
as the workflow keeps it (decision 36); and the preparation of
`data` the tests used before the context, `prepare_model_data_for_test()`,
`rdata_or_model_frame()`, `rcheck_data_rows()` and
`rcheck_rows_after_cleaning()`, is gone. Increment 11, the last, is in the
development version too, in two parts. In its first a `MASS::rlm()` fit, an
M-estimate that every test built on the context took for an `lm`, is refused
by the context, with its class, on every route, a grouped run and
`autoCompareRemediations()` among them (decision 4, R6 and N10;
`rrefuse_rlm()`). In its second, decided by the author on the survey of the
first (decisions 37 and 38), a penalised `mgcv::gam()` fit with smooth terms
is tested on its stored design, the basis of its smooths, by every test that
reads only its residuals, fitted values, columns of `data` or the columns of
its design as auxiliary regressors, the streaming tests included, and refused
by every test that fits it again by least squares or whose null distribution
is derived for least-squares residuals of its design
(`rrefuse_penalised_gam()`); a `gam` that is the least-squares fit of its
design, without smooth terms or with fixed ones, is taken by every test as the
`lm()` of that design (`rgam_least_squares()`). Every increment is done; see
[Migration plan](#migration-plan).

## The problem

Every test took `(model, data)`, and `data = NULL` stood for the model frame.
The tests then obtained the model in seven ways; the White, Breusch-Pagan,
Koenker, rank permutation and high-dimensional tests, for instance, rebuilt
the design with `model.matrix(model, data = data)`. Rebuilding re-evaluates
the formula and the call of the fit on `data`, which goes wrong in several
ways:

- A term computed from all the rows the fit saw, such as `I(x > median(x))`,
  is computed again on the rows of the fit only, so the test uses a design the
  fit never used (defect B5 below).
- `subset =` given by index is applied a second time to rows already subset
  (B15), and `cut(x, quantile(x))` computes other breaks on the rows it keeps,
  so the rebuild fails with "factor has new levels" (N7).
- The model frame, which `data = NULL` stands for, holds the terms of the
  model (`log(x)`), not the variables they are computed from (`x`), nor a
  column passed as `weights =` or `offset =`. A test that rebuilt from it
  stopped with "object 'w' not found" or "missing from input data" (B12, L1),
  and `runHeteroTests()` put a fallback test under the name of the one that
  stopped.
- Incomplete rows were dropped before the remaining rows were matched to the
  residuals, so a tibble renumbered them and the match failed or pointed at
  other rows (B14).

0.15.0 added a guard (`rcheck_data_rows()`, in `R/interface.R`) that compares
the rows of `data` that the row names pick with every column of the model
frame, and refuses data it cannot confirm, so that the misaligned cases stop
with an error instead of returning a wrong value. The model context keeps that
guard as its alignment step and removes the rebuilding behind it.

## The contract

- **Nothing is evaluated again.** The response, design, residuals, fitted
  values, weights, offset, rank and identifiers come from the stored fit.
  `subset =`, `weights =`, `offset =`, `na.action` and variables of the
  workspace cannot be applied twice or found elsewhere.
- **`data` is matched, never assumed.** Rows of `data` are read only once the
  guard has confirmed that they hold the observations of the fit, by row name,
  or for data that number their rows (a tibble, a data.table, a data frame
  whose row names were reset) by the original row numbers of the fit, or, when
  those numbers are absent and `data` has one row per observation, in the order
  of the fit (decision 18). Nothing is matched by position without the
  comparison, and nothing is dropped or renumbered before it. Rows taken in the
  order of the fit are confirmed only up to rows that tie on every compared
  column, which could be exchanged without a trace; so when a test reads a
  column of `data` beyond the model frame, such as the `order_by` of the rank
  permutation, Goldfeld-Quandt, Szroeter and Harrison-McCabe tests, the
  `variable` of Glejser's and Park's tests or the `group` of Hartley's and of
  the five group tests, the `id` and `time` of the panel tests or the `vars` of
  the scatter diagnostic, such data are refused if two rows of the fit tie so
  (a finding of the review of increment 1).
- **Named data in any order, numbered data in fit order** (decision 17). Data
  with row names of their own are matched by name in any order. Numbered data
  in another order than the fit, or with rows in front of those of the fit,
  are refused, as by the 0.15.0 guard (decision 3): their numbers say nothing
  about which row holds which observation.
- **Weights.** One rule for every kind of fit: the prior weights of an `lm`,
  of a Gaussian `glm` and of a `fitWLS()` fit (`rleast_squares_weights()`).
  A fit with a zero weight is refused where the tests refused it before;
  `performRESETTest()` keeps accepting one (decided 2026-10-17).
- **Offsets.** The context holds the total offset, the `offset` argument plus
  any `offset()` terms. A refit through the context carries it: on a model
  with an offset a test gives the value of the model fitted to the response
  minus the offset, and RESET the nested F test of the model with its offset
  against the model augmented with the powers of its fitted values
  $X\hat\beta + o$, fitted with the same offset and weights (decision 19).
- **Rank.** The rank of the fit, its aliased coefficients and the tolerance
  its rank was decided with (`1e-7` for an `lm`, `1e-11` for a `glm`); a
  rank-deficient fit is tested on the estimable part of its design, as since
  0.16.0. A test that decomposes the design takes the rank of the fit, at its
  tolerance (Harrison-McCabe, decision 21).
- **Refusals.** A test builds the context first. The context refuses a model
  that is not an `lm` or a least-squares `glm` and a `survey::svyglm()` fit
  (`rrefuse_non_gaussian_glm()`), a multivariate `lm`, since increment 11 a
  `MASS::rlm()` fit, an M-estimate whose residuals are not least-squares
  residuals (`rrefuse_rlm()`, decision 4), a model frame that
  `model = FALSE` rebuilt from data that changed after the fit, and `data` it
  cannot match. The test then runs its checks of the model, unchanged
  (`rvalidateModelInputs()`), and the check that the fit reproduces itself
  (`rvalidate_model_context()`). The messages are those the tests gave, and
  every message the test suite pins keeps its text; on a call with more than
  one defect, the first one reported may differ, since the rows of `data` are
  now matched before the checks of the model. A 12-observation fit with `data`
  that lack `x2` reports "missing from input data: x2" where it reported the
  minimum number of observations; a fit with a zero weight given `data` that
  lack its first row reports "Missing rows: 1" where it reported the zero
  weight; and an 80-observation fit given 10 rows reports "Missing rows" where
  it reported the minimum number of rows, which `runHeteroTests()` classifies
  as a data quality issue, as it does every "Missing rows" message.
- **An `mgcv::gam()` fit** is tested on its stored design, `model.matrix()`,
  its parametric columns and the basis of its smooths, by every test and the
  streaming tests (decision 37). A smooth term is penalised unless it is fixed
  (`s(x, fx = TRUE)`), and a penalised fit is not the least-squares fit of
  that design: the tests that fit it again by least squares (RESET,
  Goldfeld-Quandt, the quantile regression test, the studentized
  Breusch-Pagan statistic, the variance form test and `fitWLS()`), the
  bootstraps, whose replicates do (the White and wild bootstraps,
  `bootstrapTestStatistic()` and the robust tests with `bootstrap = TRUE`),
  and the tests whose null distribution is derived for least-squares
  residuals of the design, through its residual-maker (Harrison-McCabe and
  Szroeter), refuse it with the class of the context
  (`rrefuse_penalised_gam()`), final in the runners. A `gam` that is the
  least-squares fit of its design, decided from the fit as the refit of the
  design reproducing its residuals to `1e-8` of the spread of the response
  (`rgam_least_squares()`, `rleast_squares_reproduces()`), is taken by every
  test as the `lm()` of that design, the refitting and resampling tests
  included, the wild bootstrap reading its leverages from the design
  (`rmodel_leverage()`) (decision 38). Until increment 11 the tests of
  increment 2, the White bootstrap and the robust Breusch-Pagan test refused
  every `gam` with smooth terms, fixed ones included (decision 23),
  `bootstrapTestStatistic()` refused every `gam`, the wild bootstrap stopped
  on every `gam`, and the others took it as before 0.16.0, the streaming
  tests on the design of the variables of its formula. The residuals of a
  `gam` carry no names; its
  observations are those of the row names of its model frame
  (`rcontext_ids()`), and `data` is matched to them by those names (N17).
- **Refusals are final** (decision 6). The refusals of the context have class
  `heteroTests_model_context_error`, and those of `data` the class of the
  guard, `heteroTests_data_rows_error`; `runHeteroTests()` and
  `runDiagnostics()` run no other test under the name of one they refused, and
  keep the class.
- **Missing rows.** Every vector of the context has one value per fitted
  observation, without the padding of `na.exclude`. The warning "Removed N
  observations due to missing values in ..." that the tests gave on data with
  incomplete rows is kept, and nothing is dropped (decision 7). The five group
  tests, which left out the observations of the fit whose group is missing,
  still do, with the same warning and on the same rows, the rows the
  alignment confirmed (`rcontext_group_rows()`; decision 8: each test keeps
  its behaviour). A fit with `na.action = na.exclude` drops the rows `na.omit`
  drops, and its `residuals()`, `fitted()`, `weights()` and `hatvalues()`
  hold an `NA` at each of them (`naresid()`). Since increment 9 (B13) the
  checks of the model (`rvalidateModelInputs()`, through `rmodel_unpad()`) and
  every test read them without the padding, so such a fit gives what the same
  fit with `na.omit` gives on the same data, its twin: the same statistic,
  p-value, refusal and warnings, "Removed N observations" among them, and for
  the resampling tests the same draws of the generator. An output with one
  value per row is padded as `naresid()` pads it, in the order of the fit: of
  the functions built on the context, the estimated variances `fitWLS()`
  attaches to its fit (`"variance_model"`), whose `residuals()`, `fitted()`
  and `weights()` R pads already; the tests return none. Until increment 9 the
  checks refused such a fit ("Model residuals must be finite"), and so every
  test that runs them; RESET stopped on its padded fitted values ("NA/NaN/Inf
  in 'x'"), the spread-level and Davidian-Carroll tests left out the `NA`
  themselves, and the panel tests and the scatter diagnostic, which have no
  such check, read its padded residuals against the rows of `data` by
  position (decision 29).
- **No state.** The context is built for one call and dropped when the call
  returns. It is never stored, returned, exported or cached, and holds no
  global or mutable state. It is not the validation cache, whose checks the
  tests keep calling with the same keys.

## The representation

An internal list of class `ht_model_context`, built by `rmodel_context()` in
`R/model_context.R`:

| Field | Content | Source |
| --- | --- | --- |
| `model` | the fit | argument |
| `kind` | `"lm"`, `"glm_gaussian"`, `"gam"`, `"fitwls"`; until increment 11, which refuses a `MASS::rlm()` fit, `"rlm"` | `rcontext_kind()` |
| `frame` | the model frame | `stats::model.frame(model)`, after `rcheck_model_frame()`; for a `model = FALSE` fit whose data are gone, built from `data` and kept only if it gives the response and the fitted values of the fit (`rcontext_frame_from_data()`) |
| `obs_id` | the names of the observations, in the order of the fit | `names(model$residuals)`, or the row names of the frame for a fit whose residuals have none (`mgcv::gam()`) |
| `na_action` | `model$na.action` | the fit |
| `response` | the response, on the scale of the fit (`log(y)` for `log(y) ~ x`) | `model.response(frame, "numeric")` |
| `design` | the design of the fit, every column | `stats::model.matrix(model)` |
| `residuals`, `fitted` | one per fitted observation, without padding | `model$residuals`, `model$fitted.values` |
| `weights` | the prior weights, or `NULL` | `rleast_squares_weights()` |
| `offset` | the total offset, or `NULL` | `model.offset(frame)` |
| `rank`, `aliased`, `qr_tol` | rank, aliased coefficients, rank tolerance | `model$rank`, `raliased_coefficients()`, `model$qr$tol` |
| `variance_function` | the variance function of a `fitWLS()` fit, or `NULL` | `attr(model, "variance_function")` |
| `rows` | the row of `data` holding each observation, or `NULL` without `data` | the alignment step |
| `alignment` | `"model_frame"` (no `data`), `"row_names"`, `"row_numbers"`, `"in_order"` | the alignment step |
| `variables` | the extra variables a test names (a group, an ordering), in fit order | `rcontext_variables()` |

The functions of `R/model_context.R`:

| Function | Does |
| --- | --- |
| `rmodel_context(model, data, fn, test_label, need, reads)` | Builds the context: the refusals of the model, the parts of the fit, the alignment of `data`, the variables in `need`. `reads` names the columns of `data` the test reads itself, such as an `order_by`. |
| `rcontext_kind(model, fn)` | `rrefuse_non_gaussian_glm()`, then refuses a multivariate `lm` (M7) and, since increment 11, a `MASS::rlm()` fit (`rrefuse_rlm()`, R6); returns the kind. |
| `rrefuse_rlm(model, fn)` | Refuses a `MASS::rlm()` fit, an M-estimate whose residuals are not least-squares residuals, with the class of the context, naming the `lm()` fit of its formula to test instead (increment 11, decision 4); `bootstrapTestStatistic()` calls it before `test_function`. |
| `rcontext_align(ctx, data, fn, test_label, beyond)` | The alignment step below; `beyond` says whether the test reads columns of `data` beyond the model frame. |
| `rcontext_variables(ctx, data, need)` | Reads the variables a test names: from the rows the alignment confirmed, or without `data` from the model frame, when it holds them under their own names (M1 otherwise). |
| `rvalidate_model_context(ctx)` | One value per observation everywhere; distinct rows; and that the response is the design times the coefficients plus the offset plus the residuals (a `MASS::rlm()` fit was let through unchecked until increment 11 refused it before), and, but for a penalised `mgcv::gam()` fit, that the residuals are orthogonal to the design in the metric of the weights, each relative to the size of its terms (M8). The orthogonality allows $10^{-6} \lVert X_j \rVert_W \lVert e \rVert_W + \epsilon\, n\, \lVert X_j \rVert_W \lVert y \rVert_W$: a Gaussian `glm` computes its residuals as $y - \hat\mu$, which shifts them by about $\epsilon \lvert \bar y \rvert$, and the first term alone refused valid fits of a response near `1e11`. In a sweep of `lm` and Gaussian `glm` fits with 120 to 10,000 observations and responses up to `1e14`, the largest score of a valid fit was 0.04 of the second term. An edited coefficient leaves the residuals unchanged, so only the first check sees it, at `1e-7` of the largest term: an edit of 0.5 goes undetected once the response is above about `1e7`. M8 is a guard against accidental edits, not a proof that a fit is unedited. |
| `rcontext_frame_from_data(model, data)` | The model frame of a `model = FALSE` fit whose data are gone, from `data`, verified as `rcheck_model_frame()` verifies a rebuilt frame. |
| `rstop_model_context(message)` | The refusals of the context, with class `heteroTests_model_context_error`. |
| `rcontext_pearson(ctx)` | The Pearson residuals the tests read (`rpearson_residuals()`), unpadded; refuses a zero weight as before. |
| `rcontext_design(ctx)` | The design without its aliased columns. |
| `rcontext_rows(ctx, data)` | The confirmed rows of `data`, in fit order and under the names of the fit, or the model frame without `data`: what a test hands to its checks of the data and to a test it calls. |
| `rcontext_data_rows(model, data, fn, test_label)` | The rows of `data` that hold the observations of the fit, in its order and under its names, or `data` itself when it is those rows, for the functions outside the tests that read `data` row by row (increment 10): `recommendRemediationStrategies()` and `generateHeteroRecommendations()`. Of a grouped tibble, its rows with its groups (from the review of increment 10). |
| `rcontext_data_match(model, data, fn, test_label, beyond)` | The context built with `data`, every column of `data` read beyond the model frame, so data in the order of the fit whose rows tie are refused, for the functions of `rcontext_data_rows()` and `generics::augment()`: its refusals name the function, and data that lack the variables of the model are refused with a message that says what to pass (from the review of increment 10). |
| `rcontext_unpad(ctx, x)` | Removes the padding of `na.exclude`, of `residuals()`, `fitted()`, `weights()` and `hatvalues()` of the fit, which every test reads so since increment 9 (B13). |
| `rmodel_unpad(model, x)` | The same, from the fit, for the code that reads it before or without its context: the checks of the model (`rvalidateModelInputs()`, `rvalidateSampleSize()`) and `bootstrapTestStatistic()` (increment 9). |
| `rcontext_fitted(ctx)` | `fitted()` of the fit without the padding of `na.exclude`, what the tests computed from the model alone and RESET read (increment 9). |
| `rmodel_as_na_omit(model)` | A fit with `na.exclude` with the class of the `na.action` of `na.omit`, the same rows: what `rvalidate_against_reference()` gives `lmtest::bptest()` and `car::ncvTest()`, which read `weights()` and `residuals()` (increment 9). |
| `rwarn_incomplete_rows(data, model, also)` | The "Removed N observations" warning, dropping nothing; `also` names the other variables the test reads from `data`, such as the `order_by` of Goldfeld-Quandt. |
| `rgam_least_squares(ctx)` | Whether a fit is not a `gam`, or a `gam` that is the least-squares fit of its stored design: the refit of the design by least squares, with its weights and offset, gives its residuals to `1e-8` of the spread of the response (`rleast_squares_reproduces()`; increment 11, decision 38). |
| `rrefuse_penalised_gam(ctx, fn, why)` | Refuses a `gam` that is not that fit, a penalised one, with the class of the context, in the tests that fit it again (`why = "refit"`), the bootstraps (`"replicate"`), `fitWLS()` (`"fitwls"`) and the tests whose null distribution is derived for least-squares residuals of the design (`"null"`); its message names the smooths and says to fix them or to refit with `lm()` and a parametric term (increment 11, decision 37). It replaces `rrefuse_smooth_gam()`, which refused every `gam` with smooth terms, fixed ones included, in the tests of increment 2 (decision 23). |
| `rmodel_leverage(model, design)` | The leverages of the fit: `hatvalues()` without the padding of `na.exclude`, and for a `gam`, which has no QR decomposition of its own, the diagonal of the hat matrix of its stored design in the metric of its weights (increment 11; the wild bootstrap and `bootstrapTestStatistic()`). |
| `rcontext_mean(ctx)` | The fitted values, the linear predictor with the total offset, computed from the coefficients as `predict.lm()` computes it, in fit order: what `fitted()` holds (an `lm` up to rounding, as `y - e`), and so what `predict(type = "response")` returns, but on a `gam` with an `offset` argument, which `predict.gam()` leaves out. The registered `"szroeter"` orders by it (increment 3). |
| `rstop_missing_variable(ctx, values, variable, argument, test_label)` | Refuses an extra variable that is `NA` at an observation of the fit, with the "Missing rows" message of before, naming the variable (decision 8). |
| `rcontext_group_rows(ctx, data, group)` | The five group tests (increment 4): the group read as `rcontext_variables()` reads it (M1 without `data`), the Pearson residuals (`rcontext_pearson()`, which refuses a zero weight), and the confirmed rows of `data` in fit order, or the model frame, without the observations whose group or variable of the model is missing, left out by `rhandleMissingValues()` with its warning, as before (decision 8). |
| `rcontext_columns_at_fit(ctx, data, columns, fn, test_label)` | The panel tests and the scatter diagnostic (increment 5): the columns they read, `id`, `time` or `vars`, as `rcontext_variables()` reads them (M1 without `data`), and the Pearson residuals (`rcontext_pearson()`); for a fit with `na.exclude` the same, since increment 9, where its padded residuals were read against the rows of `data` by position, checked by `rcheck_data_in_fit_order()`, which goes with `rcheck_data_matches_model()`. |
| `rstop_repeated_panel_cells(id_values, time_values, id, time, fn)` | Pesaran's test (increment 5): refuses a pair of `id` and `time` that holds two observations of the fit, naming up to three pairs, with the class of the context (decision 13). |
| `rstop_unbalanced_panel(id_values, time_values, id, fn)` | Pesaran's test (increment 5): refused a panel whose units are not all observed at the same periods, naming a unit and its count of periods against the panel's, with the class of the context (N12, decision 28). Since the pull request of N12, on the positional reading of a fit with `na.exclude` only; it goes with that reading in increment 9, and on every route the statistic of an unbalanced panel is computed (`rpesaran_cd_unbalanced()`, `R/performPesaranTest.R`). |
| `rcontext_second_formula(model, data, formulas, fn, test_label, check_type)` | The functions that may evaluate a second formula (increment 6): the type of `data` checked first (`rcheck_data_argument()`, whose message the test suite pins; not by `fitWLS()`), then the context built with `data`, the variables of the formulas passed as `reads` (`.` reads every column). The tests computed from the model alone and RESET call `rcheck_data_argument()` and `rmodel_context(model, data, ...)` themselves (decision 31). |
| `rcontext_formula_rows(ctx, data, formula, arg)` | The rows a second formula is evaluated on (increment 6): the confirmed rows of `data` in fit order (`rcontext_rows()`), or the model frame without its terms; a variable of the formula that `data`, or without `data` the model frame, does not hold is refused, with the message of `data` or M1 (R3, decision 9). |
| `rformula_constants(formula, names)` | The names of a second formula that are constants (from the review of increment 6): not columns, and of a value, found from the environment of the formula as `eval()` finds it, that is an atomic vector of length one; `rcontext_formula_rows()` lets them through, and refuses every other name it cannot find in `data` or the model frame. |
| `rstop_missing_from_frame(ctx, absent, also)` | M1, for `rcontext_variables()` and `rcontext_formula_rows()`, which adds that the variables of the formula are not read from the workspace. |

The guard of 0.15.0 is split in two, so that the alignment can run its
comparison on rows found another way: `rdata_fit_rows()` picks the rows by
name or by number, and `rcheck_rows_at()` compares them with the model frame.
`rcheck_data_rows()` kept its signature and ran the two, its behaviour
unchanged; since increment 8, when the wild bootstrap, the last test that
called it, moved onto the context, no test called it, and increment 10
removed it.

### The alignment step

1. Without `data`, there is nothing to align: the variables a test names come
   from the model frame when it holds them under their own names and they are
   variables of the formula; otherwise M1.
2. `data` must be a data frame, holding the variables of the model or the
   columns of the model frame (the model frame itself is accepted as `data`),
   with today's messages (`rvalidateDataInputs()`).
3. `rdata_fit_rows()` picks the rows by row name; for numbered data, by the
   original row numbers of the fit. Repeated row names are refused.
4. If a name of the fit is absent, the data are numbered and they have
   exactly one row per observation, the rows are taken in the order of the fit
   (decision 18).
5. Otherwise the data lack rows of the fit: M2 or M4.
6. `rcheck_rows_at()` compares the rows with every column of the model frame:
   the columns `data` holds under their own names value by value, and the
   others (`log(x)`, `poly(x, 2)`, `offset(o)`, the weights and an offset given
   as arguments) computed from `data` as the fit computed them. A column that
   differs means refusal; so do columns that cannot be compared and leave rows
   of the fit that the compared ones cannot tell apart.
7. Rows taken in the order of the fit (step 4) are refused when the test reads
   columns of `data` beyond the model frame and two rows of the fit tie on
   every compared column.
8. Extra variables are read as `data[[v]][ctx$rows]`.

A follow-up of the review, recorded for a later step: try the rows in the
order of the fit also when the rows picked by number fail the comparison and
`data` has one row per observation, which would accept a fit to
`d[order(x), ]` given that same data frame numbered again, and character row
names that look like numbers.

Two rows can pass for each other only when they tie on every compared column
(response, regressors, weights, offset). They then have the same design row,
weight, offset and residual, and exchanging them moves only extra variables
between identical observations. Data that reproduce every column of the
model frame but are another data set, with another extra column, cannot be
seen by any alignment (N16).

### Messages

| Code | When | Text |
| --- | --- | --- |
| guard | rows that differ, repeated names, data that cannot be confirmed, a stale `model = FALSE` frame; rows in the order of the fit that tie, for a test that reads beyond the model frame | the messages of `rstop_data_rows_unmatched()`, unchanged, and for the ties "... rows of the fit tie on every column of the model frame that is compared (...)", class `heteroTests_data_rows_error` |
| M1 | an extra variable the model frame does not hold, without `data`; a variable of a second formula (increment 6, R3) | "The following variables are missing from the model frame of `model`: x1. It holds the terms of the model (log(ypos), log(x1), x2), not the variables they are computed from; pass the data the model was fitted to as `data`.", and for a second formula "Variables that are not part of the model must be supplied through `data`: those of `var_formula` are not read from the workspace.", whose first words are the message of 0.15.0 the test suite pins |
| M2, M4 | rows of the fit absent; another number of rows | `rstop_data_rows()`, today's text ("requires `data` to contain the rows used to fit the model. Missing rows: ...") |
| M7 | a multivariate `lm` | "performKoenkerTest() needs a model with one response; `model` has 2." |
| R6 | a `MASS::rlm()` fit (increment 11), on every route, a grouped run among them, before `data` is matched | "performKoenkerTest() cannot test a MASS::rlm() fit: an rlm fit is an M-estimate, whose residuals are not least-squares residuals, and the null distributions of the tests assume least squares. Test the lm() fit of the same formula instead, lm(y ~ x1 + x2, data = d).", the call built from that of the fit: its formula, and its `data`, `weights`, `subset`, `na.action`, `offset` and `contrasts`, without the arguments only `rlm()` takes (`psi`, `method`, `maxit`, `init`, `scale.est`, `k2`, `acc`, `test.vec`, `wt.method`, `lqs.control`, `c`); for an `rlm` of the matrix interface, which has no formula of two sides, "Test the lm() fit of the same model instead."; `fitWLS()` "cannot refit" it, "and fitWLS() fits its variance function to least-squares residuals. Pass the lm() fit of the same formula instead, fitWLS(lm(y ~ x1 + x2, data = d))."; `augment()`, `recommendRemediationStrategies()` and `autoCompareRemediations()` "cannot take" it |
| gam | a penalised `mgcv::gam()` fit (increment 11, decision 37), in the tests that fit it again by least squares, the bootstraps, `fitWLS()`, and Harrison-McCabe's and Szroeter's tests | "performGQTest() cannot test a penalised mgcv::gam() fit (s(x1)): its residuals are not those of the least-squares fit of its design, the basis of its smooths, which the test fits again. Fix its smooths, s(x, fx = TRUE), so that it is that fit, or refit the model with lm() and a parametric term such as splines::ns() in place of each smooth.", the smooths those of the fit; for a bootstrap "which each replicate of the bootstrap fits again", for `fitWLS()` "cannot refit ... which fitWLS() refits by weighted least squares, without the penalty", for Harrison-McCabe's and Szroeter's tests "for which the null distribution of the test is derived". It replaces the message of decision 23, "cannot test an mgcv::gam() fit with smooth terms (s(x1)): a smooth term is usually penalised, ...", and the errors of `bootstrapTestStatistic()` ("supports Gaussian `lm` models only") and of the wild bootstrap ("lm object does not have a proper 'qr' component") |
| M8 | the fit does not reproduce itself | "The fitted model does not reproduce its residuals from its design and coefficients: it was changed after the fit. Refit the model." / "The weights of the model are not those its coefficients were fitted with: ..." A guard against accidental edits, not a proof that a fit is unedited: an edited coefficient, which leaves the residuals unchanged, is seen only at `1e-7` of the largest term (an edit of 0.5 goes undetected once the response is above about `1e7`). |
| frame | a model frame that cannot be rebuilt, and `data` absent or not giving the response and the fitted values of the fit | "... cannot use the model frame of `model`: it cannot be rebuilt (...), and `data` does not give the response and the fitted values of the fit. ..."; since increment 7, for `data` that lack rows named as the observations of the fit, as the tibble of a fit to named data does, "..., and the rows of `data` cannot be matched to the observations of the fit: `data` has no rows named Mazda RX4, Mazda RX4 Wag, Datsun 710. ..." |
| R2 | in Pesaran's test, a pair of `id` and `time` that holds more than one observation (decision 13), on every route | "performPesaranTest() needs one observation of the fit for each pair of `id` and `time`, and 1 pair holds more than one: id = 1 and time = 5 (2 observations). The residuals are arranged in a matrix of periods by units, with one cell for each pair. Pass a panel with one row per unit and period.", naming up to three pairs, "and N more". Until increment 9, on the positional reading of a fit with `na.exclude`, the checks ran on the rows of `data`, so a refusal could name a row the fit dropped: with `d$time[2] <- 1L` and `d$x2[2] <- NA` this message counted row 2 among the "observations of the fit", and with `d$time[3] <- NA` and `d$x2[3] <- NA` the refusal of a missing period said "Missing rows: 3", a row the fit did not use. Since increment 9 such a fit is read at the rows it kept, as with `na.omit`: those two calls give a value, as their twins do |
| N12 | in Pesaran's test, an unbalanced panel, whose units are not all observed at the same periods (decision 28): on every route in increment 5; since the pull request of N12, which computes the statistic of an unbalanced panel, on the positional reading of a fit with `na.exclude` only; on no route since increment 9, which removes that reading | Increment 5: "performPesaranTest() needs a balanced panel, every unit observed at the same periods, and the panel of the fit is unbalanced: 1 of its 12 units is observed at fewer of its 10 periods, id = 1 at 5 of them. The statistic is computed here on a matrix of periods by units, which an unbalanced panel does not fill; plm::pcdtest() computes it for an unbalanced panel from the periods each pair of units shares." Since the pull request of N12: "performPesaranTest() reads a fit with na.action = na.exclude by position, on a matrix of periods by units, and needs a balanced panel, every unit observed at the same periods; the panel of the rows of `data` is unbalanced: 1 of its 12 units is observed at fewer of its 10 periods, id = 1 at 5 of them. Refit the model with na.action = na.omit: the statistic of an unbalanced panel is then computed from the periods each pair of units shares, as plm::pcdtest() computes it." And, on any route, a panel in which no pair of units shares two periods: "performPesaranTest() needs a pair of units observed together at two periods or more, and no pair of the 6 units of the fit shares more than one period. ..." |

M1, M7, M8, the frame message, R2, N12, R6 and the refusal of a penalised
`gam` have class `heteroTests_model_context_error`. Both panel refusals find the repeated pairs
and the periods of each unit in one pass over the observations (a number for
each pair, `duplicated()` and `tabulate()`): 0.02 s each on 120,000
observations of 4,000 units, where the first version of R2 counted each
repeated pair with a scan of every observation, 18 s on this machine (53 s in
the review of increment 5).

## Compatibility

For the functions built on the context, in increment 1:

- **What the change reaches.** The five functions, the deprecated name
  `performBreuschPaganTest()`, the base statistic of `performWhiteTestRobust()`
  and `performBPTestRobust(studentize = FALSE)`, which call them, the registry
  names and the default battery of the runners. `performWhiteTestBootstrap()`
  does not move: its observed statistic and its replicates keep the
  computation of 0.15.0, on the design rebuilt from the rows of the fit
  (`rwhite_test_rebuilt_design()`), so that both come from the same kind of
  design, until increment 8 moves both. `bootstrapTestStatistic()` and
  `performWhiteTestRobust(bootstrap = TRUE)` give their observed statistic
  through the migrated functions and refit their replicates from the formula
  until increment 8; a fit whose model frame cannot be rebuilt from `data`
  under the levels of the fit, as with `cut(x, quantile(x))`, whose replicates
  would compute other breaks, is refused by them ("`data` does not reproduce
  the fit"), as it was refused before with "factor has new levels". The
  streaming tests, which `runHeteroTests()` runs in their place only when a
  finite `chunk_threshold_mb` asks for them (decision 27; up to 0.15.0 the
  runner and the exact tests switched to them above 100 MB), rebuilt the
  design chunk by chunk until increment 7, which builds them on the context
  (below).
- **Values that change.** A term computed from all the rows of the data on a
  fit that dropped rows, such as `I(x1 > median(x1))` with `na.omit`, is
  tested on the design of the fit instead of a rebuilt one (B5). Without
  `data`, `runHeteroTests()` on a weighted fit, or a fit with an offset, whose
  weights or offset are a column of the data, ran the Cook-Weisberg test under
  the names `white` and `koenker`; it now runs White's and Koenker's tests
  (the runner part of B12).
- **Errors that become values**: weights or an offset taken from a column of
  the data, with `data` omitted or the model frame given (B12, L2);
  transformed variables and `offset()` terms without `data` (L1); the complete
  rows of an `na.omit` fit renumbered, the tibble with the rows it dropped, the
  tibble the model was fitted on, and the tibble in fit order of a fit to data
  with character row names (B14); `subset =` given by index (B15);
  `cut(x, quantile(x))` (N7).
- **A message**: a multivariate `lm` is refused with M7 instead of a coercion
  error (R4/N9).
- **New refusals**: a fit whose coefficients, residuals or weights were changed
  after it was fitted (M8); data in the order of the fit whose tied rows could
  be exchanged, for the rank permutation test ordered by a column beyond the
  model frame, which before stopped with "Missing rows"; and a `model = FALSE`
  fit whose data are gone, given `data` that do not give its response and
  fitted values, for which a value was computed unchecked. Given data that do,
  such a fit is tested on them, as before.
- **Warnings.** The warnings of the calls of the identity corpus are those of
  before; outside it, Koenker's test gives the warnings of the checks of the
  model (outliers, a large dataset) once, where it gave them twice.
- **`poly()` terms** (decision 1): the value equals, with `identical()`, what
  the code before the context computes on the fit's stored design (the model
  refitted with `x = TRUE`); against the design it rebuilt, which computes
  `poly()` again through its predvars, the differences are in the last bits
  (measured up to 1.3e-15 relative on the statistic).
- **Unchanged**: every other value on valid calls, every refusal of the guard,
  and every message the test suite pins. Numbered data in another order than
  the fit stay refused (decision 3). `performWhiteTestRobust()` and
  `performBPTestRobust()` keep their own preparation of the data until
  increment 8: they gain the fixes of the statistic they call, but still need
  `data` for a model with transformed variables. A fit with
  `na.action = na.exclude` is still refused, as before, until increment 9
  (below), which reads it as the same fit with `na.omit`. A
  Gaussian `mgcv::gam()` fit, a `glm` by class, is tested as before; without
  smooth terms it gives the value of the `lm`. Whether a penalised `gam` with
  smooth terms is an input the tests justify is a question of the contract,
  left to increment 11 with `MASS::rlm()`, which that increment refuses
  (below); the `gam` is its open second part.

For the functions built on the context in increment 2:

- **What the change reaches.** The studentized Breusch-Pagan statistic
  (`performStudentizedBPTest()`, `performBPTestRobust()` with
  `studentize = TRUE`, its default, the registered `"student_bp"`, its
  `test_factory` entry and `rvalidate_against_reference("studentized_bp")`),
  `performRESETTest()` and `runDiagnostics()`, which runs it,
  `performGQTest()`, and `performQuantileRegressionTest()` with the registered
  `"quantile_regression"`. `performBPTestRobust()` keeps its own preparation
  of the data until increment 8, and its bootstrap refits its replicates from
  the formula.
- **Offsets (B7, R5).** The refusals of #134 become values: Goldfeld-Quandt,
  the quantile regression test and the studentized statistic give the value of
  the model fitted to `y - o`, and RESET the nested F test of the model with
  its offset (decision 19). `rrefuse_offset()` and the class
  `heteroTests_offset_error` are gone.
- **Values that change.** Goldfeld-Quandt on an unweighted fit with a term
  that depends on the rows it is computed from, `ns()`, `bs()`,
  `I(x > median(x))`, `cut(x, quantile(x))` (N1, decision 20); `poly()`
  changes in the last bits. The quantile regression test on
  `cut(x, quantile(x))`, whose refit from the formula computed other breaks on
  the rows the fit kept (N1). Both on a term computed from all the rows on a
  fit that dropped rows (B5).
- **Errors that become values**, as for the five: B12, B14, B15, L1, L2, N7.
  A fit with `model = FALSE` whose data are gone, given `data` that give its
  response and fitted values: Goldfeld-Quandt and the studentized statistic
  stopped with "object not found" and are tested on `data` ("frame"). The
  registered `"student_bp"` called with `data = NULL` reads the model frame,
  where it stopped with "Expected a data.frame".
- **Messages.** M7 for a multivariate `lm`; Goldfeld-Quandt without `data`
  ordered by a variable the model frame does not hold under its own name, M1;
  RESET on a `model = FALSE` fit whose data are gone, the frame message of the
  context; a call with an offset on data the guard refuses, the guard's
  refusal where it stopped with the refusal of the offset.
- **New refusals** of calls that returned a value (decision 14). RESET on a
  model frame that `model = FALSE` rebuilt from data changed after the fit
  (N6), which it read; RESET on a multivariate `lm` without `data`, M7, where
  it returned an F statistic of neither response; an `mgcv::gam()` fit with
  smooth terms in the four (decision 23), on which the studentized statistic
  and RESET returned a value, and so did the routes that run them, the
  runner with `"student_bp"`, `runDiagnostics()`, `test_factory`, while
  Goldfeld-Quandt and the quantile test stopped on the smooth term; and the
  runner with `"student_bp"` and `"quantile_regression"` on a `model = FALSE`
  fit whose data are gone, without `data`, where it put the Cook-Weisberg
  fallback under their names.
- **RESET without `data`** (decision 24). RESET reads nothing from `data`: it
  builds its context without `data` and keeps the check of `data` of the tests
  computed from the model alone (`rcheck_data_argument()`), the variables of
  the model and the row names of the fit, until increment 6, so that its value
  on every form of `data` is that of 0.15.0. Increment 6 ends it: RESET, with
  the tests computed from the model alone, matches `data` through the guard,
  as every test built on the context does (decision 31, below).
- **Unchanged**: every other value on valid calls, the zero-weight refusals of
  Goldfeld-Quandt and the studentized statistic and RESET's acceptance of a
  zero weight, and every message the test suite pins.

For the functions built on the context in increment 3:

- **What the change reaches.** `performGlejserTest()`, `performParkTest()`,
  `performSzroeterTest()` and the registered `"szroeter"`, which orders by the
  fitted values with the offset, which it computes from the context
  (`rcontext_mean()`), the numbers `predict(type = "response")` gave but on a
  `gam` with an `offset` argument, `performHarrisonMcCabeTest()`,
  `performHartleyFmaxTest()`, `performSpatialHeteroTest()` with the registered
  `"spatial_hetero"`, their `test_factory` entries, and `runHeteroTests()` with
  these names. Glejser, Park, Szroeter, Harrison-McCabe and Hartley read the
  variable they name (`variable`, `order_by`, `group`) at the confirmed rows of
  `data` (`reads`, so data in the order of the fit that tie are refused), or
  without `data` from the model frame when it holds it under its own name (M1
  otherwise). The spatial test reads no column of `data`: its `listw` lists the
  observations of the fit in their order, after `subset` and `na.omit`, as
  before, so it passes no `reads` and the rule on ties does not apply to it.
- **Values that change.** Harrison-McCabe takes the rank of the fit, decided at
  its tolerance (decision 21, N4): on a Gaussian `glm`, or an `lm` fitted with
  a `tol` below `1e-7`, whose design holds a column that is a combination of
  the others to within `1e-7` but not to within its own tolerance, the null
  distribution has the rank of the residuals. With `x3 = x1 + 1e-9 z` (seed 5)
  on `contract_data()`:

  | Fit of `y ~ x1 + x2 + x3` | Rank | Before | After |
  | --- | --- | ---: | ---: |
  | `glm()`, `order_by = "x1"` | 4 at `1e-11` | 0.0022879 | 0.0022734 |
  | `glm()`, `order_by = "x2"`, `point = 1/3`, two-sided | 4 at `1e-11` | 0.91344 | 0.91217 |
  | `glm(weights = w)`, `order_by = "x1"` | 4 at `1e-11` | 0.79089 | 0.80233 |
  | `glm(weights = w)`, `order_by = "x2"`, `point = 1/3`, two-sided | 4 at `1e-11` | 0.58886 | 0.59106 |
  | `lm(tol = 1e-11)`, `order_by = "x1"` | 4 at `1e-11` | 0.0022879 | 0.0022734 |
  | `lm()`, `order_by = "x1"` | 3 at `1e-7` | 0.0021134 | 0.0021134 |

  The statistic does not change; each new p-value equals the test written out
  from the fit's own QR decomposition with its rank (`context_hmc_by_hand()`)
  to 1e-8 relative, the precision to which a column at 1e-9 of the others is
  fixed in either decomposition (to rounding on a well-conditioned fit). With
  `k = 4`, `point = 3` and `point = 117`, which gave p = 0.0891 and 0.1227 on
  the `glm`, stop: each part needs as many observations as the fit has
  coefficients. A `gam`, which has no QR decomposition, keeps the rank of its
  design at `1e-7`, as before. The registered `"szroeter"` on a `gam` with an
  `offset` argument (the review of increment 3; decided by the coordinator on
  2026-10-20): it ordered by `predict()`, which leaves that offset out, and
  orders by the fitted values with it, 0.800386 (p 0.2117) becoming 0.992131
  (p 0.1606) on the fit of `mtcars` in NEWS, the value of the same `gam` with
  an `offset()` term ("gam_offset"). And an `mgcv::gam()` fit given its data in another order (N17): its
  residuals carry no names, and the five tests that read a column of `data`
  matched `data` to them by position, returning a wrong value without an error
  (Glejser -0.322 for 3.626 on the `gam` without smooth terms, -0.429 for
  3.583 with `s(x1)`); they match by the row names of its model frame (approved
  on 2026-10-19). `runHeteroTests(tests = "spatial_hetero")`, on data the
  spatial test stopped on before (the model frame of a transformed fit as
  `data`, numbered data of B14), put the Breusch-Pagan fallback under its name,
  and runs the spatial test.
- **Errors that become values**: B14, L1 and L2 for these (an `offset()` term
  without `data`, the model frame as `data`; for the spatial test, which reads
  no variable, the transformed models too), "frame" for Harrison-McCabe, which
  read `model.matrix(model)` (the other five tested such a fit already), and
  the registered `"szroeter"` given `data = NULL` or numbered data in fit
  order, which it matched to its fitted values by row name ("null"). B15 gave
  values already for these six.
- **Messages.** M7 for a multivariate `lm`; M1 without `data` for a variable
  the model frame does not hold under its own name, and, with the model frame
  as `data`, the message of `data`, each naming only the variables the test
  still lacks; the refusal of ties for Hartley on the tibble of a fit to
  repeated rows, which stopped with "Missing rows"; the frame message; and,
  for the registered `"szroeter"`, the refusals of the guard on the data as
  given, where it matched them by row name first.
- **New refusals**, announced in bold (decision 14). A fit with
  `model = FALSE` whose data are gone, given numbered data in another order
  than the fit (a shuffled tibble), from which the six computed a value (a
  wrong one for the five that read a column: Szroeter -0.537 for 3.179 in
  fit order): the frame built from such data does not give the response of
  the fit, and the six refuse it with the frame message, directly, through
  the registered names, `test_factory` and the runner, as the first nine did
  ("frame"; the cell `class/model_false_tibble`, from the review of increment
  3). And, outside the corpus, a fit whose coefficients, residuals or weights
  were changed after it was fitted (M8), from which the six computed a value,
  is refused as by the first nine: `tests/testthat/test-pass-a-reference.R`,
  which set a residual of Park's test to zero by hand, makes it zero with an
  observation on the regression of the others instead. Harrison-McCabe on the
  nearly collinear `glm` with `point = 3` or `117` (above).
- **Unchanged**: every other value on valid calls, the zero-weight refusals on
  every route, with `data`, without and shuffled, a smooth `gam` tested as
  before (decision 23), and every message the test suite pins. The known limit
  of numbered data permuted within ties and matched by number (ROADMAP) is the
  same for these tests.
- **Still matched by position** (N17): the group tests until increment 4
  (below), Pesaran and the random-effects test (5), the three streaming tests
  (7) and the replicates of the White bootstrap (8) on a `gam` given data in
  another order; each is closed by its increment (the streaming tests, below).

For the functions built on the context in increment 4:

- **What the change reaches.** `performLeveneTest()`,
  `performBrownForsytheTest()`, `performFlignerKilleenTest()`,
  `performBartlettTest()` and `performOBrienTest()`. None is registered, in
  `test_factory` or in a runner; a diagnostic registered with
  `registerDiagnostic()` that calls one keeps the refusals of the context
  final in `runHeteroTests()`, as the registered names do (tested). Their
  copies of the alignment (`align_to_model()` in four of them, inline in
  O'Brien's) are gone: they read the group as Hartley's test does, at the
  confirmed rows (`reads`, so data in the order of the fit that tie are
  refused), or without `data` from the model frame (M1 otherwise), and the
  residuals and weights from the fit (`rcontext_group_rows()`).
- **Missing groups** (decision 8). The observations of the fit whose group,
  or a variable of the model, is missing in `data` are left out with the
  warning "Removed N observations due to missing values in ...", computed by
  `rhandleMissingValues()` on the rows of the fit, as before: a group missing
  at a row the fit dropped is not counted. Values and warnings are those of
  `main` on every such cell of the corpus, and the statistic equals the test
  computed without the package on the observations kept.
- **Values that change** (N17). A `gam`, with or without smooth terms or an
  `offset` argument, given its data in another order under their own row
  names: the five matched them by position and returned a wrong value
  (Bartlett 4.548 for 12.380 on the `gam` without smooth terms); they match by
  the row names of its model frame, the value of the call on the data in fit
  order, and of the test written out with `car::leveneTest()`,
  `stats::fligner.test()`, `stats::bartlett.test()` and O'Brien's
  transformation (`context_group_oracle()`, to `1e-10`).
- **Errors that become values**: the complete rows of an `na.omit` fit
  renumbered and the tibble in fit order of the character-named fit, which
  stopped with "Missing rows" although they hold the rows of the fit (B14,
  R7); a model of the group with an `offset()` term or a `log()` term without
  `data` (L1), and its model frame as `data` (L2). B12 and B15 gave values
  already: the group tests read no weights from `data` and matched the
  subset by row name.
- **Messages.** M1 without `data`, where they stopped with "missing from input
  data", naming the variables of the model too (R7: "ypos, x1, g3" for
  `log(ypos) ~ log(x1) + x2` grouped by a `g3` it does not use); numbered data
  in another order for a fit to character-named data, such as its tibble
  shuffled, the guard's refusal ("... y, x1, x2 differ from the model
  frame"), where they stopped with "Missing rows: obs1, obs2, obs3", rows the
  data hold (R7); the message of `data`
  naming the group alone with the model frame as `data`; the refusal of ties
  on the tibble of a fit to repeated rows, where they stopped with "Missing
  rows"; M7 for a multivariate `lm`; the frame message for a `model = FALSE`
  fit whose data are gone, without `data`, where they stopped with "Expected a
  data.frame"; M8 for an edited fit without `data` or with its model
  frame, where they lacked the group first; and, from the review of increment
  4, an `NA` or empty `group`, refused by the check of the argument of four of
  them as by O'Brien's ("`group` must be a single column name."), where an
  `NA` reached `rvalidateDataInputs()` as a required variable: its cache key,
  which drops the `NA`, was that of the check without variables that the
  alignment makes on data holding the columns of the model frame, so the
  later calls of the tests built on the context on the same data met the
  cached refusal (`performWhiteTest()` stopped with "missing from input data:
  NA"). The key itself is left to the pull request on the validation cache.
- **New refusals**, announced in bold (decision 14): a fit with
  `model = FALSE` whose data are gone, given a shuffled tibble ("frame"), from
  which the five computed the value of other rows (Levene 1.373 for 3.459);
  and a fit whose coefficients were changed after the fit (M8), from which
  they computed the value of the fit before the edit, given its data or its
  rows shuffled.
- **Unchanged**: every other value on valid calls, the zero-weight refusals
  with `data` and shuffled (without `data`, or with its model frame, the group
  the frame lacks is named first, as before), the refusals of the guard, a
  smooth `gam` tested as before with `data` (decision 23), and every message
  the test suite pins.

For the functions built on the context in increment 5:

- **What the change reaches.** `performPesaranTest()`,
  `performBPRandomEffectsTest()`, `runPanelTests()`, which runs the two, and
  `performScatterDiagnostic()`. None is registered or in `test_factory`, and
  `runPanelTests()` runs no other test in place of one that stops. They read
  the Pearson residuals and the weights from the fit, and `id`, `time` and
  `vars` at the confirmed rows (`reads`, so data in the order of the fit that
  tie are refused), or, for the panel tests without `data`, from the model
  frame (M1 otherwise); the scatter diagnostic and `runPanelTests()` need
  `data`, as before (`rcontext_columns_at_fit()`). The positional check of
  0.15.0, `rcheck_data_in_fit_order()`, is left to a fit with `na.exclude`
  (below), until increment 9 removes it.
- **Errors that become values** (B3, B4, B16): data with row names of their
  own in another order than the fit, rows after those of the fit, a fit to a
  subset given its data, the complete rows of an `na.omit` fit renumbered and
  its tibble (B14), the tibble in fit order of a fit to character-named data
  (B14), and the data of a fit that dropped rows. On the design of the 0.15.0
  release notes, shuffled with `set.seed(2)`, Pesaran -0.987, the random-effects
  test 1.187 and the scatter diagnostic 0.468, the values on the data in fit
  order (before 0.15.0, -0.392, 0.506 and -0.080, values of other rows). On a
  fit that dropped three rows of `contract_data()`, the random-effects test
  and the scatter diagnostic give the value of the fit to the complete rows
  (2.016); Pesaran's test does on a fit that drops every row of a unit, whose
  panel stays balanced (-0.0607, where it refused the full data with "requires
  `data` with 110 observations to match the fitted model, got 120").
- **Values that change** (N17): a `gam` given its data in another order under
  their own row names, which the three matched by position, as their check of
  the order of 0.15.0 needs the names the residuals of a `gam` lack. On
  `d[sample(120), ]` after `set.seed(3)`: Pesaran -0.570 for 0.264, the
  random-effects test 0.163 for 3.100, the scatter diagnostic 0.011 for 0.332
  (x1), and with `s(x1)` -0.456 for -0.036, 0.070 for 4.311 and -0.012 for
  0.308, each the value of the call on the data in fit order.
- **New refusals**, announced in bold (decision 14): Pesaran's test on a
  repeated pair of `id` and `time` (R2, decision 13), and so `runPanelTests()`:
  with the second observation of unit 1 moved to period 5 it returned 0.516
  where the panel gives 0.264; a fit with `model = FALSE` whose data are gone,
  given a shuffled tibble ("frame"), from which the four computed the values of
  other rows (Pesaran -0.570, the random-effects test 0.163, the scatter
  diagnostic 0.011); and an edited fit (M8), from which they computed the
  values of the fit before the edit (on `mtcars` with 0.5 added to the
  coefficient of `wt`: Pesaran 0.830, the random-effects test 16.70, the
  scatter diagnostic -0.141 and 0.345); and Pesaran's test on an unbalanced
  panel (N12, decision 28), and so `runPanelTests()`, on which it recycled the
  residuals of a unit and returned a wrong value: 0.234 on `contract_data()`
  without its first five rows, where `plm::pcdtest(model = "pooling")` gives
  -0.373.
- **Messages.** M1 without `data` for the panel tests, where they stopped with
  "`id` and `time` must be columns in `data`." (or "`id` must be a column");
  M7 for a multivariate `lm`, where they stopped on its number of rows; the
  frame message for a `model = FALSE` fit whose data are gone, without `data`,
  where they stopped with "`data` must be a data frame."; the refusal of ties
  on the tibble of a fit to repeated rows, where they stopped with "Missing
  rows: 1.1, 2.1, 3.1"; M8 for an edited fit given its rows shuffled, or
  without `data`; for Pesaran's test, the refusal of R2 on a fit to repeated
  rows, where it stopped on the unbalanced panel, and of a unit or a period
  missing at an observation of the fit (`rstop_missing_variable()`), where it
  stopped on its matrix; and Pesaran's test on the unbalanced panel of a fit
  that dropped some rows of some units, the refusal of an unbalanced panel,
  where it stopped on the complete rows of such a fit with "number of items to
  replace is not a multiple of replacement length" and refused the full data
  for their number of rows.
- **An unbalanced panel is refused (N12, decision 28).** Pesaran's test fills
  a matrix of periods by units, and on a unit observed in fewer periods than
  the panel holds it stopped, or, when that number divides the number of
  periods, recycled the residuals of the unit and returned a wrong value
  without an error. The alignment would have let more data reach it: the full
  data of a fit that dropped such rows, which it refused for their number. It
  refuses a panel whose units are not all observed at the same periods
  (`rstop_unbalanced_panel()`, class `heteroTests_model_context_error`), on
  every route, after the refusals of a missing unit or period and of a repeated
  pair, so the error of the assignment can no longer be reached; the message
  names a unit and its count of periods against the panel's, and points to
  `plm::pcdtest()`, which computes the statistic of an unbalanced panel on the
  periods each pair of units shares (Pesaran 2004, Section 9). A fit that
  drops every row of a unit keeps a balanced panel and is tested. The refusal
  comes off in the pull request of N12, which computes the statistic so (done:
  the refusal stays on the positional reading of a fit with `na.exclude`
  only, decision 28, until increment 9). `runPanelTests()`
  reports the refusal of one of its tests by stopping with it, its class kept:
  it returns no list, and runs no other test in its place.
- **Unchanged**: every other value on valid calls; the zero-weight refusals,
  which the scatter diagnostic still makes before it matches `data`; the
  refusals of the guard; the random-effects test with a unit missing at an
  observation of the fit, which it leaves out of its sums by unit, as before
  (decision 8); a fit with `na.action = na.exclude`, whose padded residuals the
  three still read against the rows of `data` by position (B13, increment 9,
  decision 29), with the refusals of Pesaran's test applied to the rows they
  read so, the rows of `data`, those the fit dropped included. They do not see
  that the panel of the fit itself is unbalanced: the matrix cell of
  `missing/na_exclude`, whose panel is balanced in `data` but not in the fit
  (`x2` missing in rows 3, 7 and 11), gives 0.713 as on `main`, computed with
  `NA` residuals at the dropped rows, where `plm::pcdtest()` gives 0.189 on
  the rows of the fit and the same fit with `na.omit`, refused in increment
  5, gives 0.189 since the pull request of N12; the value is left to
  increment 9, which gives 0.189 there, the value of the twin with `na.omit`;
  a smooth `gam`, tested as before (decision 23); and every message the test
  suite pins ("`id` must be a column in `data`.", "`id` and `time` must be
  columns in `data`." for data that lack them).

For the functions built on the context in increment 6:

- **What the change reaches.** The nine tests computed from the model alone,
  `performSpearmanTest()`, `performHarveyTest()`, `performArchLMTest()`,
  `performMcLeodLiTest()`, `performCookWeisbergTest()` and `performNCVTest()`,
  `performSpreadLevelTest()`, `performDavidianCarrollTest()` and
  `performVarianceFormTest()`, with `fitWLS()`; their registered names
  (`"cook_weisberg"`, `"ncv"`, a fallback of White's, Koenker's and the
  Breusch-Pagan tests in the runner, `"spread_level"`, `"variance_form"`),
  `runTimeSeriesTests()`, and the runners with them; and RESET, which builds
  its context the same way. They read the residuals, fitted values, design and
  weights from the fit, as before, and nothing from `data`, which they match
  to the observations of the fit through the guard, as every test built on
  the context does (`rmodel_context(model, data, ...)`, decision 31);
  `rcheck_data_argument()` keeps only its check of the type of `data`, whose
  message the test suite pins, and `rcheck_data_matches_model()` remains for
  the positional reading of a fit with `na.exclude` by the panel tests, until
  increment 9, which removes it. A second formula, the `var_formula` of Cook-Weisberg's test (and
  NCV), of the variance form test and of `fitWLS()`, and the `against` of the
  variance form test, is evaluated on the rows the alignment confirmed, the
  variables of the formula passed as `reads` (`rcontext_second_formula()`), or
  on the model frame without `data` (`rcontext_formula_rows()`);
  `rvariance_frame()` goes.
- **Decision 31: one rule for every test.** Decision 24 kept RESET's check of
  `data` of 0.15.0 "until increment 6", for the variables of the model and the
  row names of the fit; the first version of this increment kept it for the
  nine too, as an open question, and the author decided on 2026-10-26 that
  the tests that read nothing from `data` match it as every other test does.
  So they refuse what the guard refuses, numbered data in another order than
  the fit, rows in front of those of the fit and repeated row names, from
  which they computed the value of the fit; and take numbered data with one
  row per observation in the order of the fit, and the data of a `gam` that
  dropped rows, which the check of 0.15.0 refused by their row names or
  number. On `mtcars` with its row names reset, Spearman's test gave 1.445
  (p = 0.159) on `dplyr::as_tibble(d)[sample(32), ]` after `set.seed(1)`, on
  `rbind(d[1:5, ], d)` and on `d` with a repeated row name, and refuses them;
  on the complete rows of a fit that drops two rows, numbered again, it stopped
  with "Spearman rank requires `data` to contain the rows used to fit the
  model. Missing rows: 31, 32" and gives 0.951 (p = 0.35), the value on the
  full data. `fitWLS()` without `var_formula` ignored `data`, and matches it
  too, refusing a `data` that is not a data frame, a list or a matrix, with
  the message of the tests ("`data` in fitWLS() must be a data frame or
  NULL.", from the review of increment 6), where it returned the fit.
- **B9.** On numbered data in another order than the fit, rows in front of
  those of the fit, repeated row names, and the tibble of a fit to repeated
  rows when the formula names a variable beyond the model frame, `fitWLS()`
  and the variance form test read the variables of the second formula by
  position and returned another value without an error; they are refused by
  the guard (decision 3). On `mtcars` with its row names reset and the data as
  `dplyr::as_tibble(d)[sample(32), ]` after `set.seed(1)`: `fitWLS(var_formula
  = ~ hp)` 22.01, -4.953, 0.804 for 16.38, -4.587, 1.028; the variance form
  test 0.0203 for 0.0631 (`var_formula = ~ hp`) and 0.0890 for 0.00297
  (`against = ~ hp`). Cook-Weisberg's test refused such data already (the
  guard since 0.15.0).
- **R3** (decision 9). A variable of a second formula that neither `data` nor
  the model frame holds under its own name was found in the environment of
  the formula; it is refused, with M1 and a sentence on the workspace
  without `data`, and with the message of `data` with it. On `mtcars` with
  `hp2 <- rev(mtcars$hp)`: Cook-Weisberg 1.453, the variance form test 4.374
  (`var_formula`) and 0.266 (`against`), and `fitWLS()` a fit with those
  weights, now refused; `fitWLS()` and the variance form test with `data`
  that lack `hp2` too, which Cook-Weisberg's test refused already. A
  constant, a value of length one found from the environment of the formula
  as `eval()` finds it (`k <- 2` in `~ I(hp^k)`, `pi`), is not a variable and
  is evaluated as R evaluates it (`rformula_constants()`, from the review of
  increment 6): Cook-Weisberg 0.5496 on `lm(mpg ~ wt + hp, mtcars)` with
  `~ I(hp^k)`, `car::ncvTest(m, ~ I(hp^2))`, as before without `data`, and
  given `mtcars`, where it refused `k` as a variable `data` lacked.
- **N6.** A model frame that `model = FALSE` rebuilt from data changed after
  the fit is refused, with or without `data`, by the nine and `fitWLS()`: on
  `mtcars` reordered by `wt` after the fit, the variance form test returned
  5.817 for 1.765, Harvey's test 3.041 for 2.446 (its design), `fitWLS()`
  refitted the reordered response, and the others gave the value of the fit,
  which they do not read from the frame.
- **New refusals**, announced in bold (decision 14), besides B9, R3, N6 and
  the data the guard refuses (decision 31):
  an edited fit (M8) by the nine, not by `fitWLS()`, which is not a test and
  refits it as before; a fit with `model = FALSE` whose data are gone,
  without `data` or given a shuffled tibble ("frame"), from which
  Cook-Weisberg's, the NCV, spread-level and Davidian-Carroll tests computed
  the value of the fit unchecked; and a multivariate `lm` without `data` by
  Davidian-Carroll's test, which returned a statistic of neither response
  (M7).
- **Errors that become values**: a fit with `model = FALSE` whose data are
  gone, given `data` that give its response and fitted values ("frame"), for
  the five that stopped without a model frame, RESET, which refused it, and
  `fitWLS()`; numbered data in the order of the fit (B14) and the data of a
  `gam` that dropped rows (N17), which the nine and RESET refused by their row
  names or number, with or without a second formula (decision 31).
- **Values that change** (N17): Cook-Weisberg's test with `var_formula` on a
  `gam` given its data in another order under their own row names, which it
  read by position; the value of the data in the order of the fit.
- **Messages**: M1 or the message of `data` for a variable of a second
  formula the model frame does not hold, where the formula stopped on the
  object not found; M7; the frame message; the refusal of the guard before
  the refusal of a weighted fit or a zero weight, and of repeated row names,
  where the check of 0.15.0 said "Missing rows"; and
  decision 8 for a variable of `var_formula` missing at an observation of the
  fit, named by Cook-Weisberg's test, with the warning "Removed N
  observations due to missing values in ..." of before.
- **Unchanged**: every other value; the values on named data in any order and
  on numbered data in the order of the fit; the zero-weight refusals and
  RESET's acceptance of a zero weight; a smooth `gam` tested as before by the
  nine; and every message the test suite pins ("`data` in f() must be a data
  frame or NULL." among them).

For the functions built on the context in increment 7:

- **What the change reaches.** `performWhiteTestStreaming()`,
  `performBPTestStreaming()` and `performKoenkerTestStreaming()`, and
  `runHeteroTests()` and `runDiagnostics()` with a finite
  `chunk_threshold_mb`, which run them in place of the exact tests, and their
  fallbacks on one another (decision 27). The three build the context, read
  the Pearson residuals and the weights from the fit, and read the rows of the
  stored design chunk by chunk (`rstreaming_design()`, `R/performance.R`): the
  design of the fit without its aliased columns (`rcontext_design()`), as the
  exact tests read it whole. Up to 0.16.0 each chunk built its own design from
  the formula, on its rows of `data`, through the `predvars` of the terms.
  The design is held whole, as by the exact tests and as the model frame is;
  what the streaming spares is the auxiliary design, built chunk by chunk,
  with 2p + p(p - 1)/2 columns for White's test. They read no column of
  `data` beyond the model frame: they pass no `reads`, and data in the order
  of the fit whose rows tie are taken.
- **The accepted path is unchanged.** `chunked_regression_r2()` still
  accumulates the cross-products of each chunk in the order of the chunks and
  decides the rank of the auxiliary regression by `qr()` of the uncentred
  cross-products and fits it by `solve()`, so where the chunks built before
  the rows of the design the fit built, the value is `identical()` to before;
  the refused regression keeps the centred cross-products of #151
  (`rcentred_cross_fit()`). White's streaming test on collinear terms of its
  auxiliary regression, the squares of dummies or of an orthogonal `poly()`
  basis, still stops, which is N3, a pull request of its own (decision 22).
  That pull request (0.17.0) replaces the cross-products, on both paths, by a
  QR decomposition of each chunk of the auxiliary design of the centred
  regressors stacked on the factor of the chunks before it, and the rank by
  that of `qr()` of the factor, the rule of `performWhiteTest()`: White's
  streaming test drops the collinear terms, and `rcentred_cross_fit()` goes:
  used on the accepted path, with the centred regressors, it would count 31
  degrees of freedom for 30 on 30 subjects of two observations and one of
  three, and 54 for 60 on the basis of a smooth `gam`.
- **Values that change** (B6, B5). A term computed from the rows it is given,
  in chunks smaller than the data: on the term cell of the matrix,
  `y ~ I(x1 > median(x1)) + x2`, the Breusch-Pagan test gave 11.375 in chunks
  of 7 and 12.344 in chunks of 40, and Koenker's 8.165 and 8.861, where the
  fit gives 12.901 and 9.260; on a fit that dropped rows (`missing/median_full`),
  where the one chunk computed the median of the rows the fit kept, 13.345,
  12.299 and 5.746 in chunks of 10,000, 40 and 7 for 12.464, and 9.276, 8.549
  and 3.994 for 8.664. Each new value equals the same fit with the column
  computed beforehand, to `1e-10`, and the exact test, to `1e-8`. N17: a
  `gam` given its data in another order under their own row names, which each
  chunk read by position: White's streaming test 8.887 for 11.145, the
  Breusch-Pagan test 6.759 for 12.448 and Koenker's 4.910 for 9.044, the
  values of the data in the order of the fit.
- **Errors that become values**: `cut(x1, quantile(x1))`, which took the
  breaks of each chunk and of the rows of the fit, which dropped the row at the
  minimum of `x1`, and stopped with "factor ... has new levels" in chunks of
  any size (N7): the Breusch-Pagan test 12.425 and Koenker's 10.052 on 4
  degrees of freedom, the values of the exact tests; transformed variables and
  `offset()` terms without `data` (L1), the model frame of such a fit as
  `data` (L2), the complete rows of an `na.omit` fit renumbered and the tibble
  in fit order of a fit to character-named data (B14). B12 and B15 gave values
  already: the three read the weights from the fit, and matched the rows of a
  subset by name.
- **Messages.** M7 for a multivariate `lm`, where they stopped with "'length =
  4' in coercion to 'logical(1)'"; the frame message for a `model = FALSE` fit
  whose data are gone, without `data`, where they stopped with "Expected a
  data.frame for input data"; White's streaming test on `cut()`, the refusal
  of its collinear auxiliary terms, where it stopped on the new levels. And,
  for every test built on the context, a `model = FALSE` fit whose data are
  gone, to data with row names of their own, given numbered data that lack
  those names, such as the tibble of `mtcars`: the frame message says that
  the rows of `data` cannot be matched to the observations of the fit and
  names the rows `data` lacks (`rcontext_frame_from_data()`), where it said
  that `data` does not give the response and the fitted values of the fit,
  and the streaming tests said "Missing rows".
- **New refusals**, announced in bold (decision 14): a fit whose coefficients,
  residuals or weights were changed after it was fitted (M8), from which they
  computed the values of the fit before the edit (White's streaming test
  11.145 on the cell `class/edited`, 11.82 on `mtcars`), and a fit with
  `model = FALSE` whose data are gone, given a shuffled tibble ("frame"), from
  which they computed the values of other rows (8.887 for 11.145); and the
  runner that streams them, which keeps the refusals final (decision 6).
- **A `gam` with smooth terms.** Its stored design is the basis of its smooths
  (`model.matrix()` of a `gam`), on which the exact tests test it. The streaming
  tests built the design of each chunk from its `terms`, those of the
  variables of its formula, and so tested it on `[1, x1, x2]` for
  `y ~ s(x1) + x2`: they keep that design, built from its model frame, so
  that its values are those of before (Breusch-Pagan 11.185 and Koenker 8.537
  on 2 degrees of freedom on the cell `class/gam_smooth`, where the exact
  tests give 23.995 and 18.313 on 10), until increment 11 settles which tests
  take such a fit, as decision 23 provides for every test outside increment 2
  (the review of increment 7 agreed). Reading the stored design would give
  the exact tests' values for the Breusch-Pagan and Koenker tests and make
  White's streaming test refuse the collinear terms of the basis (N3), and
  refusing the fit, as the tests of increment 2 do, would be new refusals.
  Since the pull request of N3 White's streaming test would give the value of
  the exact test on the basis too, dropping its collinear terms: 46.05 on 60
  on `gam(y ~ s(x1) + x2)` in the design of `contract_data()`, as
  `performWhiteTest()`, where it keeps 10.61 on 5 on the variables of the
  formula, as before, to the last bits. Increment 11 decides between the two.
- **Unchanged**: every other value on valid calls; the zero-weight refusals
  and the refusals of the guard; a fit with `na.action = na.exclude`, refused
  as before ("Model residuals must be finite"), until increment 9 (below); and the
  refusal of a regression of squared residuals that do not vary, "requires
  variation in squared residuals", decided against the absolute threshold
  `.Machine$double.eps` on their total sum of squares, which refuses residuals
  of about `1e-5` that the exact tests take (outside the context, an open
  item of ROADMAP.md; with N3, in 0.17.0, the threshold is relative to the sum
  of squares of the squared residuals, and those residuals give the values of
  the exact tests).

For the functions built on the context in increment 8:

- **What the change reaches.** `performWhiteTestBootstrap()` and the
  registered `"white_bootstrap"`, `performWildBootstrapTest()` and the
  registered `"wild_bootstrap"`, `bootstrapTestStatistic()` and its deprecated
  name `rbootstrap_test_statistic()`, `performWhiteTestRobust()` and
  `performBPTestRobust()`, with and without their bootstrap, and so
  `runAdvancedDiagnostics()` and `runHeteroTests()` with the resampling tests.
  The five build the context and read the Pearson residuals, the fitted
  values, the weights, the total offset and the design from the fit; `data`
  is matched to the observations of the fit and read for nothing else, as by
  the tests computed from the model alone (decision 31). The White bootstrap
  computes its observed statistic on the stored design, with the checks
  `performWhiteTest()` makes, and refits each replicate, the fitted values
  plus the residuals drawn, by least squares on the stored design, every
  column of it, with the weights and the total offset of the fit
  (`stats::lm.fit()`, `stats::lm.wfit()`, and `rwls_replicate_residuals()` for
  the weights of a `fitWLS()` fit estimated again); its statistic is White's on
  that design, without the columns the refit aliased
  (`rreplicate_estimated_columns()`). `rwhite_test_rebuilt_design()`, which
  rebuilt the design from `data` for both, is gone. The wild bootstrap refitted
  its replicates on the stored design already; what changes for it is its
  preparation of `data`. `bootstrapTestStatistic()` keeps the rows of `data`
  in their order and the call of `test_function` on them, and builds each
  replicate as an `lm` from the stored design and the model frame of the fit
  in the rows of the replicate (`rreplicate_lm()`), what `lm()` returns from
  the formula when the design it rebuilds is the stored one; pairs resampling
  draws the rows of the stored design, with its contrasts (`rdesign_rows()`),
  and builds the design again from the terms of the fit on the rows drawn
  when a factor or a character regressor has a level or a value no row drawn
  holds, without them and without the contrasts matrix of such a factor, as a
  refit from the formula does (`rpairs_design()`, since the review of
  increment 8); a replicate that cannot be built is left out, and no draw
  stops the call. The match of `data` that refused a fit whose model frame
  could not be rebuilt from `data` (`rdata_reproduces_fit()`) goes, for the
  alignment of the context and the check that the least-squares refit of the
  stored design gives the residuals of the fit (`rleast_squares_reproduces()`),
  which kept refusing a `MASS::rlm()` fit with the message of before, until
  increment 11 refused it first, with the refusal of the context (below). The
  robust tests drop their own preparation of `data` (`prepare_model_data_for_test()`
  and `rdata_or_model_frame()`, which no test called any more, and increment 10
  removed) and hand the rows the alignment confirmed to the base statistic
  and to the bootstrap.
- **The random numbers.** Every call draws the random numbers it drew before,
  in the same order, the contract of the caches (#144, #146): each replicate
  draws its residuals, its multipliers or its rows from the session's
  generator as before, and the replicates of `bootstrapTestStatistic()` follow
  the order of `data`, as before, where those of the other four follow the
  order of the fit. So a seeded call returns what it returned wherever the
  stored design is the design rebuilt from `data`, and leaves the generator
  where it left it (the identity harness records its state after every call).
- **Values that change.** A term computed from all the rows on a fit that
  dropped rows (B5): on `missing/median_full`, `I(x1 > median(x1))` with three
  rows dropped, the White bootstrap gave 11.039 (p = 0.10, `B = 19`, seed 7),
  computed on the median of the rows it kept, and gives 10.336 (p = 0.10), the
  value of the fit with the column computed beforehand. A `gam` given its data
  in another order under their own row names (N17): the White bootstrap took
  the rows of `data` by position for its replicates, adding each draw to the
  fitted value of another row; on `class/gam_shuffled` p = 0.05 becomes 0.20,
  the value of the data in the order of the fit (statistic 11.145 both), and
  on `class/gam_offset_shuffled` 0.05 becomes 0.15. And pairs resampling, whose
  replicates computed a `poly()`, `ns()`, `I(x1 > median(x1))` or `cut()` term
  again on the rows they drew, keeps the columns of the fit (B5).
- **Errors that become values.** On the cells of B12, L1 and L2, a fit whose
  weights or offset are a column of the data, or with an `offset()` term or a
  transformed variable, without `data` or given its model frame: the White
  bootstrap stopped ("object 'w' not found", "The following variables are
  missing from input data: ypos, x1"), and gives the value with `data` (5.697,
  p = 0.35, on `fit/wls/none/none/null`), and so do the wild bootstrap and the
  robust tests, which stopped on the missing variables. `cut(x1, quantile(x1))`
  (N7): the White bootstrap stopped ("factor ... has new levels"), and
  `bootstrapTestStatistic()` and the robust bootstraps refused it ("`data` does
  not reproduce the fit"); they give the value of the fit with the factor
  computed beforehand (White 11.895, p = 0.20; Koenker's bootstrap 10.052,
  p = 0.15). On `missing/median_full` (B5) the bootstraps refused the fit too.
  The complete rows of an `na.omit` fit renumbered, its tibble and the tibble
  in fit order of a fit to character-named data (B14), and a fit to a subset
  (B15), on which the White bootstrap stopped, "could not align the design
  matrix with model residuals". A Gaussian `glm` (N8), refused by
  `bootstrapTestStatistic()` and the robust bootstraps ("supports Gaussian `lm`
  models only"): bootstrapped as the weighted `lm` it is, 1.754 (p = 0.40) on
  `fit/glm/none/none/data_frame` as on `fit/wls/none/none/data_frame`. A fit
  with `model = FALSE` whose data are gone, given them ("frame"), on which the
  wild bootstrap stopped ("object 'gone' not found"). A variable of the
  formula outside `data`, such as `offset(o)` with `o` in its environment,
  which `bootstrapTestStatistic()` refused under pairs resampling and on data
  in another order ("does not come from `data`", "does not reproduce the
  fit"): its replicates are those of the same model with the offset as a
  column (`test-resampling-refits.R`).
- **New refusals**, announced in bold (decision 14): a fit whose coefficients,
  residuals or weights were changed after it was fitted (M8), on which the
  White bootstrap returned 11.145 (p = 0.20) and the wild bootstrap 9.044
  (p = 0.05) on `class/edited`, the values of the fit before the edit, and a fit
  with `model = FALSE` whose data are gone, given a shuffled tibble ("frame"),
  on which the White bootstrap returned 8.887 (p = 0.10), the value of other
  rows. From the review of increment 8, M8 also refuses a fit whose terms
  rebuild a design of another number of columns than its coefficients, as
  those of `fitWLS()` of a `gam` with smooth terms (a column per variable of
  the formula for the coefficients of the basis), where the White and wild
  bootstraps returned 3.279 (p = 0.715) and 2.152 (p = 0.380) on
  `fitWLS(mgcv::gam(y ~ s(x) + z))` (`B = 199`, the data of NEWS.md) and the
  tests built on the context stopped with "(subscript) logical subscript
  too long". A value on the stored design is not defined there: the terms of
  the fit give no design of its coefficients.
- **Messages.** M7 for a multivariate `lm`, `bootstrapTestStatistic()`
  included since the review of increment 8, where they stopped with "'length =
  4' in coercion to 'logical(1)'"; the frame message for a `model = FALSE` fit
  whose data are gone, without `data`, where they stopped with "Expected a
  data.frame"; the White bootstrap on a `gam` with smooth terms, which it
  refuses with the message of the tests of increment 2, "cannot test an
  mgcv::gam() fit with smooth terms" (decision 23: each replicate refits the
  design by least squares), where its refit from the formula stopped on
  `s()`, "could not find function \"s\""; `bootstrapTestStatistic()` on a `gam`,
  which it refuses as before, with a message that says why, and on a Gaussian
  `glm` with a zero weight or with data the guard refuses, which it refused
  as a `glm` and refuses with the message of the weight or of the guard; and
  `bootstrapTestStatistic()` on a Gaussian `glm` whose rank at its tolerance,
  `1e-11`, is not that of `lm()` at `1e-7` (the nearly collinear `glm` of the
  cells of Harrison-McCabe's rank): the replicates are refitted at the
  tolerance of `lm()`, as the refits of the formula were, so such a fit is
  refused as an `lm` fitted with a smaller `tol` is, "`data` does not
  reproduce the fit". Refitting the replicates at the tolerance of the fit
  would give both values, a change of value for the `lm`, left to a later step
  (ROADMAP.md).
- **A new refusal outside the corpus.** `bootstrapTestStatistic()` matches
  `data` through the guard, which refuses data that lack a variable of the
  formula: a statistic of the caller's own that reads nothing from `data`, on a
  model with `offset(o)` and `o` a vector of the workspace, was bootstrapped on
  data in the order of the fit and is refused (the package's tests refused
  such data in their own call already). Given `o` as a column, it follows the
  rows drawn, in any order and under pairs resampling, which refused it.
- **Unchanged**: every other value on valid calls, among them every call of
  the wild bootstrap that gave a value, which read the stored design already;
  the wild bootstrap on an `mgcv::gam()` fit, on which it stops, as before,
  reading the leverages with `hatvalues()` (ROADMAP.md);
  the zero-weight refusals and the refusals of the guard; a transformed
  response under null-imposed resampling, refused by `bootstrapTestStatistic()`
  as before ("Null-imposed resampling needs a plain response variable"); a
  `MASS::rlm()` fit, refused by `bootstrapTestStatistic()` as before, until
  increment 11 (decision 4), which refuses it with the refusal of the context
  (below); a fit with `na.action = na.exclude`, refused as
  before, until increment 9 (below); `data = NULL` given to `bootstrapTestStatistic()`,
  which needs `data`, as before; and every message the test suite pins.
- **Warnings.** The White bootstrap no longer runs the checks of the model and
  of `data` on each replicate, which gave their warnings, the outliers of the
  residuals of a replicate among them; the observed statistic gives those of
  the fit, as before.

For a fit with `na.action = na.exclude`, in increment 9:

- **What the change reaches.** Every function built on the context, its
  registered names, `test_factory` entries and routes, and the runners,
  `runHeteroTests()`, `runDiagnostics()`, `runTimeSeriesTests()`,
  `runPanelTests()` and `runAdvancedDiagnostics()`; through the checks of
  the model, which they share, `rvalidate_against_reference()`,
  `rcalculateEffectSize()`, `restimate_test_power()`,
  `generateHeteroRecommendations()` and `generateDiagnosticReport()`; and,
  from the review of increment 9, the functions outside the context that read
  the padded vectors of such a fit once the checks let it through, which read
  them without the padding (`rmodel_unpad()`), to give what they give on the
  twin: the recommendations (`identifyVariancePatterns()`, which stopped with
  "incompatible dimensions"), the plots, `plotResidualsFitted()` with and
  without `enhanced`, `plotSpreadLevel()`, `plotResidualDensity()`,
  `plotResidualQQ()`, `plotBubbleVariance()`, `plotBeforeAfter()`,
  `plotDiagnosticSuite()` and their deprecated names (the bubble plot, and so
  the suite and the report, stopped with "replacement has 120 rows, data has
  117"; the others drew the `NA` with a warning of ggplot2), the RMSE of
  `autoCompareRemediations()` and of `analyzeMLResiduals()`, which was `NA`,
  `performVIFDiagnostic()` on a `gam`, whose padded design made every VIF
  `NA`, and `generics::augment()` of the results of the runner, whose rows
  are per observation (below). An `mgcv::gam()` fit with `na.exclude`, whose
  design `model.matrix.gam()` pads with `NA` rows, is read through the twin's
  design (`rmodel_as_na_omit()`), where the context refused it for parts of
  different lengths ("Internal error: the parts of the model context differ
  in length."). Such a fit drops the
  rows `na.omit` drops, and `residuals()`, `fitted()`, `weights()` and
  `hatvalues()` hold an `NA` at each of them (`naresid()`). The checks of the
  model read one residual per observation of the fit (`rmodel_unpad()`), and
  the tests read the Pearson residuals, the fitted values and the leverages
  without the padding (`rcontext_pearson()`, `rcontext_fitted()`,
  `rcontext_unpad()`): the tests computed from the model alone, RESET, the
  White bootstrap, which drew its residuals, the wild bootstrap and
  `bootstrapTestStatistic()`, which read the leverages, the fitted values and
  the residuals of the fit. The panel tests and the scatter diagnostic read the
  rows the alignment confirmed, as for any fit (`rcontext_columns_at_fit()`),
  where they read the padded residuals against the rows of `data` by position
  (decision 29); `rcheck_data_in_fit_order()`, `rcheck_data_matches_model()`
  and `rstop_unbalanced_panel()` go, and so do the messages of the guard on
  data read by position ("it reads `data` by position, the i-th row with the
  i-th observation ..."). `rvalidate_against_reference()` gives
  `lmtest::bptest()` and `car::ncvTest()` the same fit with the class of the
  `na.action` of `na.omit` (`rmodel_as_na_omit()`), as lmtest stopped on the
  padded weights of a weighted fit ("incompatible dimensions").
- **The reference: the twin.** A fit with `na.exclude` is the fit of
  `na.omit` on the same rows; their `na.action` holds the same rows, of class
  `"exclude"` or `"omit"`. Every call on such a fit gives what the same call
  gives on the same fit with `na.omit` and the same data, its twin: the same
  statistic, p-value and every other number, `identical()`, the same refusal,
  class and message, the same warnings, the warning "Removed 3 observations
  due to missing values in x2" of decision 7 among them, which the refused
  calls did not reach, and for the resampling tests the same draws of the
  generator, which they now make. A group, `id` or `time` missing only at rows
  the fit dropped is not read, as for the twin.
- **Outputs per row.** An output with one value per row is the twin's padded
  as `naresid()` pads it, in the order of the fit. The tests return none; of
  the functions built on the context, `fitWLS()` returns a fit whose
  `residuals()`, `fitted()` and `weights()` R pads already, through the
  `na.action` it keeps, and whose estimated variances, the attribute
  `"variance_model"`, had one value per observation of the fit: they are
  padded too. The record of its variance function (`"variance_function"`:
  the regressors, the fitted log-variance and the initial weights), which the
  bootstraps read by observation, keeps one row per observation, the twin's
  (decision 33). `generics::augment()` of the results of the runner binds
  the per-observation columns of the fit to the rows of `data`, or of the
  model frame: given the rows of the observations, as the model frame is, it
  pads them with a row of `NA` at each row the fit dropped, named as
  `naresid()` names them, so that the result is the twin's padded; given the
  data of the fit, its rows, as before; and data of another number of rows
  are refused with a message that says what it binds, where they stopped with
  "replacement has 120 rows, data has 117", for the twin too, or, when their
  number of rows was a multiple of the number of fitted values, had the fitted
  values recycled over them and bound to rows of other observations: a new
  refusal on a fit without `na.exclude` too, announced in bold (the re-review
  of increment 9). `recommendRemediationStrategies()`, which correlates the
  residuals with the columns of `data` row by row, refuses data of another
  number of rows than the observations of the fit, naming both, where it
  stopped with "incompatible dimensions", or, on a fit with `na.exclude`
  given the full data, read its padded residuals against the rows by
  position; matching such data to the rows of the fit is left to increment
  10 (N5), which matches them (below). Outside the
  context, `runDiagnostics()$influence` holds the Cook's distances of
  `performInfluenceDiagnostics()`, which `cooks.distance()` pads, and the
  positions of the influential ones in that padded vector (ROADMAP.md).
- **Errors that become values** (B13): every test that runs the checks of the
  model, refused with "Model residuals must be finite before running ...",
  and so the runners ("Diagnostic execution failed for White: ..."); RESET,
  which stopped with "NA/NaN/Inf in 'x'"; the resampling tests; the panel
  tests and the scatter diagnostic given data in another order than the fit,
  which they refused ("... are not in the order of the fit"), and Pesaran's
  test on an unbalanced panel of the rows of `data`, which it refused with the
  message of N12 on that reading; and, from the review, an `mgcv::gam()` fit
  with `na.exclude`, refused by the tests that run no checks of the model
  ("Internal error: the parts of the model context differ in length."), and
  `fitWLS()` of it, which stopped with "incompatible dimensions".
- **Values that change** (B13): the random-effects test and the scatter
  diagnostic, whose statistic and correlations were `NA`, and Pesaran's test,
  computed with the `NA` of the rows the fit dropped: on `missing/na_exclude`
  (`x2` missing in rows 3, 7 and 11) 0.713 becomes 0.189, plm's on the rows of
  the fit, the random-effects test `NA` 2.016 and the scatter diagnostic `NA`
  0.343 and -0.163, the values of the twin; RESET, which returned `F = NA`
  where the rows the fit dropped were the last ones of the data (its augmented
  design took the first rows of the padded fitted values, and its residual sum
  of squares was `NA`), and stopped elsewhere; and `fitWLS()`'s
  `"variance_model"`, padded.
- **Messages.** The refusals of the twin, where the checks of the model
  refused the fit first: of `data` by the guard, M1 without `data`, a
  variable of the workspace (R3) or one `data` lacks, the weights of the
  variance form test, a transformed response under null-imposed resampling,
  and the checks of `test_factory` and of the registered names, which refuse
  the full data of a fit that dropped rows ("Model reports 117 residuals but
  the supplied data has 120 rows") as they refuse the twin's.
- **Unchanged**: every call on a fit without `na.exclude`, `identical()`;
  the spread-level and Davidian-Carroll tests on such a fit, which left out the
  `NA` themselves and gave the value of the twin; and `fitWLS()`'s
  coefficients, weights and residuals.

For the runners and the functions outside the tests, in increment 10:

- **What the change reaches.** `runHeteroTests()` with grouped data, and so
  every route that reaches a grouped run (`generateDiagnosticReport()` and
  `generateHeteroRecommendations()` hand their `data` to it);
  `runHeteroTests()` and `runDiagnostics()` given a tidymodels workflow or a
  parsnip fit with `data`; `recommendRemediationStrategies()`,
  `generateHeteroRecommendations()` and `generics::augment()` of the results
  of the runner given `data`. The tests themselves are unchanged.
- **B11, decision 10: new refusals.** A grouped run refits the model within
  each group by evaluating its call on the rows of the group
  (`.ht_refit_group()`). The call of a `fitWLS()` fit is that of the model it
  refitted, without the weights it estimated, so each group was tested as the
  unweighted fit of its rows, a value of another model without an error. Such
  a run is refused (`rrefuse_grouped_fitwls()`, `R/runHeteroTests.R`), with
  the class of the context, before any group is refitted. On the grouped
  matrix, White's test of the `fitWLS()` fit grouped by `group` gave 5.271,
  7.538, 3.789 and 5.814, the values of `lm()` on each group. A grouped run of
  a weighted `lm` is unchanged: its weights are a column of the data, which
  the refit of each group reads at its rows (an `lm` whose weights are a
  vector of the workspace was refused already). `runDiagnostics()`, which
  ungroups the data and runs no grouped analysis, tests the fit as a whole, as
  before.
- **N5: errors that become values.** A workflow is tested on the fit of its
  engine, which names its response `..y` and its predictors as the
  workflow's preprocessing made them (`log(x1)`, the dummies of a factor, a
  column a recipe transformed), and the runners refused the data the workflow
  was fitted to ("`data` lacks ..y. Pass `data = NULL`"); a parsnip fit of
  `fit_xy()` names its response `..y` too, and the tests refused its data.
  The runners map such data to the variables of the engine by the workflow's
  own preprocessing, `hardhat::forge()` with its blueprint and the outcome,
  or the outcome of `fit_xy()` to `..y` from the column the fit records,
  confirm the rows by the alignment of the context (`rmodel_context()`), and
  test the model frame of the engine's fit, the value without `data`,
  `identical()` (`.ht_workflow_data()`, `.ht_parsnip_data()`,
  `R/ecosystem_integration.R`). The engine's fit of a workflow numbers its
  observations in the order of the data it was fitted to, so its rows are
  matched by number.
- **N5: a transformed outcome (decision 35).** A preprocessing that
  transforms the outcome, a recipe with `step_log()` of it or a formula
  preprocessor `log(y) ~ x + z`, maps the outcome of `data` to the outcome it
  makes, which is the response `..y` of the engine's fit; the rows are
  confirmed against every column of its model frame, and the value is that
  without `data`. The first version of the increment refused such data, the
  formula preprocessor with a wrong message ("the column `log(y)` of `data`
  is not the response").
- **N5: case weights, steps that drop rows, lazy tables** (from the review of
  increment 10). The case weights of a workflow are mapped under the name the
  engine's call gives them, so the alignment compares them as it compares the
  weights of a fit for the tests: other weights are refused, where the first
  version took them. A recipe step that drops rows at the fit and is skipped
  on new data, as `step_naomit()` and `step_filter()` are by default, numbers
  the rows it kept afresh: the rows of `data` complete in the variables of the
  engine are taken in their order when they are as many as the rows it kept,
  and confirmed in turn (`step_naomit()`); otherwise the data are refused with
  a message that names such a step (`step_filter()`), where the first version
  said to pass the data the workflow was fitted to, which the caller had done.
  A step applied to new data (`skip = FALSE`) maps the rows it keeps, which
  the alignment confirms. A lazy `dtplyr` table is collected first, where the
  first version read the names of the lazy object ("`data` lacks ..y, x, z").
- **N5 and `augment()`, from the second review of increment 10.**
  - *Rows a recipe reorders.* `augment()` took the mapped rows for the rows
    of `data` in their order whenever the preprocessing kept every row;
    `step_arrange()`, which a recipe applies to new data, sorts them, and each
    row of `data` was bound the fitted value and the residual of another row,
    by up to 8.08 on the reviewer's data, without an error. The mapped rows
    are now traced to the rows of `data` (`.ht_workflow_mapped()`): `data`
    with its rows rotated by one, and with its first two rows swapped, must be
    mapped to the mapped rows in that order. A preprocessing that maps each
    row on its own passes; one that reorders the rows, or makes a row from
    other rows, does not, and `augment()` refuses its data, with a message
    that says so. The runners, which test the model frame once the rows are
    confirmed, are unchanged: a recipe that sorts the rows of new data sorts
    the data it was fitted to as it sorted them at the fit, in any order.
    Forging each row on its own would also say which rows a step that drops
    rows of new data kept, but costs 2.75 ms a row (0.33 s for 120 rows, about
    14 s for 5,000, where forging the whole data takes 3 ms) and was not
    adopted: `augment()` refuses such data, as before (ROADMAP.md).
  - *A step skipped on new data.* `step_log(y, skip = TRUE)`, the idiom
    recipes recommends for the outcome, transforms it at the fit only, so the
    data the workflow was fitted to, mapped by its preprocessing, hold the
    outcome untransformed, and were refused with the advice to pass the data
    the workflow was fitted to, which they were. When `data` mapped with the
    skipped steps applied are confirmed, the refusal names the steps, says
    that they changed the outcome, a predictor or the order of the rows at
    the fit, and points to `data = NULL`.
  - *Case weights of the rows a recipe keeps.* The case weights were mapped
    only when the preprocessing kept every row of `data`, so that with a
    `step_filter()` it applies to new data other weights were accepted (White
    9.4702 on the reviewer's data, with the weights reversed). A recipe gives
    the weights of the rows it keeps, in the order it keeps them
    (`recipes::bake()`), and a formula keeps every row; weights that cannot
    be told for each mapped row are refused.
  - *What the results keep.* The runners kept the workflow, or the parsnip
    fit, on each result, whose fit and mold hold the data: the results of
    `runHeteroTests(wf)` with three tests on 5,000 rows took 2,794 KB
    serialized, where `main` gave 1,522 KB. They keep what `augment()` needs
    (`rtidymodels_map()`): the blueprint of the preprocessing, the number of
    rows it kept, the column of the case weights, or the outcome of a parsnip
    fit, about 2 KB a result (1,523 KB). The results of the runners on a
    workflow or a parsnip fit, with or without `data`, are those of `main` but
    for this attribute, and the identity harness compares the attributes of
    the results of the runners (`identity_runner_summary()`).
- **N5 and `augment()`, from the third review of increment 10.**
  - *The frames of a recipe written in a function (decision 36).* The
    expressions of the steps of a recipe keep the environment they were
    written in, and the attribute keeps the frame of the function a recipe
    was written in, every object of it: 49,347 KB for a frame that held a
    50 MB data frame, and 51,245 KB for the results, where `main` gave
    1,894 KB and `saveRDS()` of the workflow takes 50,272 KB. The third
    review pruned those environments and wrote the variables the expressions
    read into them as values (`.ht_prune_environments()`); the fourth found
    that this changed what some expressions mean or broke them (a variable a
    later step makes a column, a variable named as a column a step removed,
    a function called by its name as a string, `do.call("f", ...)` or
    `sapply(x, "f")`, a variable holding a symbol or a call, spliced in as
    code), kept named and attached environments whole (19,691 KB where
    `main` gave 28 KB), stopped at the namespace clone in which R CMD check
    runs the tests, and copied large values into every result (114,196 KB
    for a step reading one value of a local vector of 5,000,000 numbers,
    39,418 KB as the step keeps it). The author decided to keep the
    blueprint as the workflow keeps it: no expression is rewritten and no
    environment changed, the record holds no fit, and NEWS says in bold
    what a saved result carries.
  - *Steps that make a row from other rows.* `cumsum()`, `row_number()`, a
    mean over `lag()` in `step_mutate()`, `step_lag()`, and `step_arrange()`
    given data already in its order, give other values when `data` is mapped
    in another order, and `augment()` refuses their data, where `main` bound
    each row of data in the order of the fit its own values. The refusal is
    kept, as conservative (decision 14), and announced; a way to accept such
    data, by the columns the preprocessing leaves unchanged, is recorded in
    ROADMAP.md.
  - *The case weights of `fit_xy()`.* The fit records the column of its
    outcome and not that of its case weights, which `fit_xy()` takes as a
    vector, so the weights of `data` cannot be compared; such data were
    taken whatever their weights (White 14.2138 on the reviewer's data with
    the weights reversed), and are refused, as the data of a workflow whose
    weights cannot be told are. A parsnip fit of `fit()` with case weights is
    an `lm` whose weights are a vector of the workspace: its data are matched
    by the variables of its formula, and its weights are not compared.
  - *The skipped steps named.* The refusal named every step skipped on new
    data; it names those without which the rows are not confirmed, each left
    out of the others in turn (a `step_filter()` that keeps every row is
    not), or "one or more of" them when none is needed alone. A step that
    drops rows of new data is said to drop or add rows, as `step_sample()`
    with replacement can.
- **N5: messages.** Data a workflow cannot be matched to, such as its rows
  shuffled or a subset of them ("Missing rows: 101, 102, 103"), are refused
  with the class of the guard, a lead that says what was matched and the
  advice of a workflow; data that cannot be mapped with a message that says
  why: the preprocessing cannot map them (a column it needs is missing), the
  package hardhat is not installed, and the outcome of `fit_xy()` given as a
  vector, which the fit does not record. Grouped data given with a workflow
  are refused, as before, now with a message that says that only an
  ungrouped data frame is mapped.
- **The rows of `data` outside the tests.** `recommendRemediationStrategies()`
  correlates the residuals with the columns of `data` row by row,
  `generateHeteroRecommendations()` profiles `data` and hands it to the tests
  and to the advice, and `augment()` binds the columns of the fit to its rows.
  They read the i-th row of `data` with the i-th observation, and since the
  review of increment 9 refused data of another number of rows. They now match
  `data` to the observations of the fit by the alignment of the context
  (`rcontext_data_rows()`), every column of `data` read beyond the model frame,
  and read those rows, in the order of the fit: named data in any order,
  numbered data in the order of the fit and, for the advice, the full data of
  a fit that dropped rows give the value of the rows of the fit in its order,
  on a fit with `na.exclude` the value of its twin (decision 32).
  - Errors that become values: the full data of a fit that dropped rows,
    refused by the count since the review of increment 9 (the advice), or by
    the checks of `generateHeteroRecommendations()` ("Model reports 117
    residuals but the supplied data has 120 rows"), and rows after those of
    the fit; for `augment()`, the full data of a fit with `na.omit` (below).
  - Values that change: named data in another order than the fit, from which
    the advice, the report and `augment()` computed the values of other rows:
    on the cell `terms/ols/none/log/shuffled` the advice "No dominant variance
    pattern detected." becomes "Variance is strongly associated with x1 higher
    values.", that of the rows in order; `augment()` bound the fitted values of
    observations 1, 2, ... to the rows of `data` in their order; and the
    profile of the report, whose skewness of `id` was `-1.45e-20` for `0` in
    the order of the rows.
  - New refusals, announced in bold (decision 14): what the alignment
    refuses, numbered data in another order than the fit, repeated row names,
    data whose rows tie on every compared column in the order of the fit (the
    tibble of a fit to repeated rows), and a `model = FALSE` fit whose frame
    cannot be rebuilt from `data` or was rebuilt from data changed after the
    fit, from which they computed a value; data that lack the variables of the
    model, which the alignment compares, and which they read by position
    (`recommendRemediationStrategies(res, m, mtcars[, c("hp", "disp")])`), now
    refused with a message that names the function and says what to pass; and
    a model the context refuses by kind, a binomial, Poisson or Gaussian `glm`
    with another link than the identity, on which the advice said "No
    dominant variance pattern detected." and `augment()` bound its values,
    refused as by every function built on the context (decisions 14 and 31),
    each from the review of increment 10.
  - The report: the checks, the tests and the advice, which read the
    residuals, read the rows of the fit; the profile of the data, their size
    and missing values, describes `data` as given (from the review of
    increment 10, where the first version profiled the rows of the fit, so its
    missing values were always none and its advice on missing data never
    fired). Grouped data keep their groups on the rows of the fit, so that the
    analysis of a grouped tibble is grouped whether or not the fit dropped
    rows, where the first version returned them ungrouped when it did.
  - Messages: the refusal of the guard, naming the function, where the
    report stopped with the refusal of a test inside the runner, or the
    advice and `augment()` with the count of rows (rows in front of those of
    the fit); and M7 for a multivariate `lm`, on which they stopped with a
    coercion error or the count.
- **`augment()` binds as broom does (decision 34).** The rows of `data` are
  matched to the observations as the tests match them and bound in the order
  of `data`, each with its own fitted value, residual and standardised
  residual (`raugment_bind()`): on a fit with `na.omit` the rows the fit used,
  so its full data give 117 rows of 120, where they were refused; on a fit
  with `na.exclude` those rows and the rows it dropped, with `NA` there, every
  row of its full data, as `broom::augment()` binds the full data in their
  order, so that it is its twin padded. Its full data shuffled under their own
  row names had the values of other observations bound to them by position,
  and get each row its own. Given the rows of its observations only, as its
  model frame without `data`, a fit with `na.exclude` binds them in the order
  of the fit with a row of `NA` at each row it dropped, as in increment 9.
  The data of a workflow, or of a parsnip fit of `fit_xy()`, are mapped as the
  runners map them, from what the runners keep on their results (from
  the review of increment 10, where the first version refused them: "missing
  from input data: ..y"); a preprocessing that drops rows of new data does
  not say which rows of `data` it kept, nor one that reorders them which row
  of `data` each observation comes from, and their data are refused by
  `augment()` (from the second review of increment 10, above).
- **The clean-up.** `prepare_model_data_for_test()`, `rdata_or_model_frame()`,
  `rcheck_data_rows()` and `rcheck_rows_after_cleaning()` go, with the refusal
  only the last could give ("numbers the rows that are left afresh") and the
  `label` argument of `rstop_data_rows_unmatched()`, which only it passed. No
  test called them since increment 8; the assertions of the test suite on
  them that a public function still makes are made on that function: the rows
  a test reads of named data in another order and the "Missing rows" refusal
  (`test-additional-tests.R`), a test given the data of the fit reading what it
  reads without them (`test-property-based.R`), the rows picked by the row
  numbers of the fit (`rmodel_context()`, `test-model-context.R`), and data
  of which no column can be compared (`performKoenkerTest()` on a fit whose
  every column is computed by a function that is gone,
  `test-data-alignment-guard.R`).
- **Unchanged**: every test, every other call of the runners, a grouped run of
  any other fit, and the calls of the three functions on data that are the
  rows of the fit in its order, `identical()`.

For a `MASS::rlm()` fit, in the first part of increment 11:

- **What the change reaches.** Every function built on the context, through
  `rcontext_kind()`: every test, the robust and resampling tests,
  `bootstrapTestStatistic()` and its deprecated name, the streaming, group and
  panel tests, the scatter diagnostic, `fitWLS()`, their registered names,
  `test_factory` entries and `cachedTest()`, the runners (`runHeteroTests()`,
  `runDiagnostics()`, `runTimeSeriesTests()`, `runPanelTests()`,
  `runAdvancedDiagnostics()`), `rvalidate_against_reference()`,
  `generateHeteroRecommendations()`, and `recommendRemediationStrategies()` and
  `generics::augment()` given `data`, which they match through the context.
  From the review of increment 11: a grouped run of `runHeteroTests()`, which
  refuses the fit before any group is refitted, beside
  `rrefuse_grouped_fitwls()` (with MASS attached `main` refitted each group as
  an `rlm` and returned its values, White's 5.65, p = 0.342, in group `a`);
  `autoCompareRemediations()`,
  which refuses it with the same message; and, through the runners,
  `compareModelDiagnostics()`, whose row of the fit is `NA`, with a warning,
  where it gave White's 11.01 and Koenker's 8.74, and `compareTestResults()`,
  `summary()` and `test()` of a `HeteroDiagnostic` and
  `generateDiagnosticReport()`, which stop with the refusal.
- **R6, decision 4: new refusals.** A `MASS::rlm()` fit is an `lm` by class
  and an M-estimate: its coefficients minimise a robust loss by iteratively
  reweighted least squares with weights that depend on the residuals, so its
  residuals are orthogonal to its design only in the metric of those weights,
  and are not least-squares residuals, for which the null distributions of
  the tests are derived. On the design of the *Supported models* page
  (`contract_data()`), 37 of the 39 tests of `helper-test-recipes.R` tested
  it as an `lm` (White 11.01 on 5, Koenker 8.74 on 2, the Breusch-Pagan test
  12.42 on 2), the quantile regression and variance form tests refusing it as
  a weighted fit, its case weights being all 1; and the runners ran their
  batteries on it. It is refused by the context (`rrefuse_rlm()`), with its
  class, before `data` is matched, on every form of `data`; the message says
  why and names the `lm()` fit of the formula to test instead. The refusal is
  final in the runners (decision 6): `runHeteroTests(tests =
  "quantile_regression")`, where the quantile test refused the fit as a
  weighted one, which is not final, put the rank permutation test under its
  name (0.341), and now stops with the refusal.
- **N10.** The studentized Breusch-Pagan statistic
  (`performStudentizedBPTest()`, `performBPTestRobust()` by default, the
  registered `"student_bp"`) refitted the design of such a fit by least
  squares and tested that fit instead: 9.04, the value of the `lm()` of the
  same formula, where Koenker's test, the same statistic on the residuals of
  the fit, gave 8.74. Both are refused.
- **Messages.** The quantile regression and variance form tests, the robust
  bootstraps and `bootstrapTestStatistic()` refused it already ("does not
  support weighted fits", "weights that did not come from fitWLS()", "`data`
  does not reproduce the fit"), and refuse it with the message of the
  context; `bootstrapTestStatistic()` before it calls `test_function`, where
  it computed the observed statistic first.
- **Outside the context.** `fitRobust()` returns such a fit, and its help page
  says that the tests refuse it. `autoCompareRemediations()` compares an `rlm`
  fit of its own with the fit it was given by their AIC and residuals, and
  tests nothing; given an `rlm` fit itself, it refuses it, where it compared
  `fitWLS()` of it and recommended it (`best` "WLS", AIC 396.5). `augment()`
  of a result around such a fit, which the runners no longer return, binds the
  columns of the fit to its model frame without `data`, reading nothing
  through the context, as before.
- **The message** names the `lm()` call to test instead, built from the call
  of the fit: its formula and the arguments `lm()` shares with `rlm()`
  (`data`, `weights`, `subset`, `na.action`, `offset`, `contrasts`), without
  `psi`, `method`, `maxit` and the other arguments only `rlm()` takes (from
  the review: the first version named the formula alone, `lm(y ~ x1 + x2)`,
  which fits another model when the fit has weights or a subset). An `rlm` of
  the matrix interface, `rlm(X, y)`, has no formula of two sides, and the
  message says "the lm() fit of the same model".
- **Unchanged**: every call on any other fit, `identical()`, with its warnings
  and the state of the generator.

For a penalised `mgcv::gam()` fit, in the second part of increment 11
(decisions 37 and 38), on the design of the *Supported models* page
(`contract_data()`, `gam(y ~ s(x1) + x2)`, whose smooth has a basis of 9
columns; its stored design has 11, and the variables of its formula 3):

- **What the change reaches.** The tests that fit the design again, the
  resampling tests and `bootstrapTestStatistic()`, the robust tests with
  `bootstrap = TRUE`, the variance form test and `fitWLS()`, the
  Harrison-McCabe and Szroeter tests, with the registered `"szroeter"`,
  `"variance_form"` and `"wild_bootstrap"`, the three streaming tests, and the
  runners that call them. Every other test reads only the residuals, the
  fitted values, columns of `data` or the columns of the design as auxiliary
  regressors, and is unchanged.
- **Decision 37: new refusals.** The variance form test and `fitWLS()`
  refitted the basis by weighted least squares without the penalty and tested
  that fit (1.053 on 49 and 60 degrees of freedom, p = 0.42; `fitWLS()`
  returned an `lm` of 11 coefficients, which every test then refused, M8);
  Harrison-McCabe's exact null distribution and Szroeter's null moments are
  those of the least-squares residuals of the design, through its
  residual-maker, which the residuals of the `gam` are not (they differ from
  those of the least-squares refit by up to 0.66, and are not orthogonal to
  the basis, 0.082), so their p-values were those of another fit's residuals
  (0.315, p = 0.0025, and 3.029, p = 0.0012, with `order_by = "x1"`; the
  registered `"szroeter"`, ordered by the fitted values, 1.634, p = 0.051).
  Each refuses the fit now with the class of the context
  (`rrefuse_penalised_gam()`), final in the runners.
- **The bootstraps.** `bootstrapTestStatistic()` refused every `gam`, with or
  without smooth terms, with its own message ("supports Gaussian `lm` models
  only"), the robust bootstraps with it; the wild bootstrap stopped on every
  `gam`, reading its leverages with `hatvalues()` ("lm object does not have a
  proper 'qr' component"), and since that error was not a refusal of the
  context, `runHeteroTests(tests = "wild_bootstrap")` put White's test under
  its name (46.05 on 60, tagged as a fallback). Each refuses a penalised fit
  with the class of the context, and the runner keeps the refusal final.
- **The streaming tests** read the stored design chunk by chunk, as the exact
  tests read it, where they read the design of the variables of the formula
  (decision 23): the Breusch-Pagan test 23.995 on 10, where it gave 11.185 on
  2, Koenker's 18.313 on 10 for 8.537 on 2, and White's 46.05 on 60 for 10.61
  on 5, each the value of the exact test, in chunks of any size. So
  `runHeteroTests()` gives the same values with and without
  `chunk_threshold_mb`.
- **Decision 38: refusals become values.** A `gam` that is the least-squares
  fit of its design, with fixed smooths (`s(x1, fx = TRUE)`) or none, is
  taken by every test as the `lm()` of its design: RESET, Goldfeld-Quandt,
  the quantile regression test, the studentized Breusch-Pagan statistic and
  the White bootstrap refused a fixed smooth as a penalised one;
  `bootstrapTestStatistic()`, the robust bootstraps and the wild bootstrap
  refused or stopped on every `gam`. On `gam(y ~ s(x1, fx = TRUE) + x2)` the
  studentized statistic gives 14.70, the value of the `lm()` of its basis,
  and the wild bootstrap reads its leverages from that basis
  (`rmodel_leverage()`). The harness verifies each such call against the
  same call on the `lm()` of the basis (below).
- **Which `gam` is the least-squares fit** is decided from the fit, not from
  the arguments of its smooths (`rgam_least_squares()`): the refit of its
  design by least squares, with its weights and offset, reproduces its
  residuals to `1e-8` of the spread of the response, the check
  `bootstrapTestStatistic()` makes of every fit (`rleast_squares_reproduces()`).
  A penalised fit whose smoothing parameter is near zero passes: on this fit,
  `sp = 1e-10` leaves residuals within `1.9e-9` of the spread of those of the
  least-squares refit and is taken, `sp = 1e-8` within `1.9e-7` and is
  refused; REML chooses `sp = 1.30`.
- **`runDiagnostics()`** refuses a penalised fit as a whole, through RESET,
  whose refusal is final, as since 0.16.0, although White's and Koenker's
  tests, which it also runs, take the fit; `runHeteroTests()` returns them.
- **Known limit.** `fitWLS()` of a `gam` that is the least-squares fit of its
  design refits that design, but returns an `lm` whose terms rebuild the
  variables of the formula, not the basis of its smooths, so the tests refuse
  it (M8), as since increment 8; a `gam` without smooth terms is not
  affected.
- **Unchanged**: every call on any other fit, and on a penalised `gam` every
  test that does not need least squares, `identical()`, with its warnings and
  the state of the generator.

## Migration plan

Each increment is one pull request from `main`, in this order: the
foundation first, then the corrections with the strongest pins, then the
migrations that keep every value, then the riskier resampling and
`na.exclude` changes, then the clean-up.

| # | Functions | Closes | Status |
| --- | --- | --- | --- |
| 1 | `R/model_context.R`; the guard split in two; `performWhiteTest()`, `performBPTest()` and `performBreuschPaganTest()`, `performKoenkerTest()`, `performRankPermutationTest()`, `performHighDimensionalTest()`, and through them the base statistic of `performWhiteTestRobust()`, `performBPTestRobust(studentize = FALSE)`, the registry names and the default battery | B5, B12, B14, B15, N7, R4/N9, R7 for these; L1, L2 | **Done, 0.16.0** |
| 2 | `.studentized_bp_test()` (`performStudentizedBPTest()`, `performBPTestRobust()`, `student_bp`), `performRESETTest()`, `performGQTest()` on the stored design, `performQuantileRegressionTest()` on `y - offset`; `rrefuse_offset()` goes | B7, N1, N6 (RESET), R5; B5, B12, B14, B15, L1, L2, N7, R4 for these | **Done, 0.16.0** |
| 3 | `performGlejserTest()`, `performParkTest()`, `performSzroeterTest()` with the registered `"szroeter"`, `performHarrisonMcCabeTest()` (rank at the fit's tolerance), `performHartleyFmaxTest()`, `performSpatialHeteroTest()` | B14, L1, L2, M1 for `variable`, `order_by`, `group`; N4; N17 for these (B15 gave values already) | **Done, 0.16.0** |
| 4 | The group tests (Levene, Brown-Forsythe, Fligner-Killeen, Bartlett, O'Brien); their copies of the alignment go; the observations with a missing group left out with their warning, as before (`rcontext_group_rows()`) | the character-named fit with its tibble and the complete rows renumbered (B14); R7; L1, L2, M1 for `group`; N17 for these (B12 and B15 gave values already) | **Done, 0.16.0** |
| 5 | `performPesaranTest()`, `performBPRandomEffectsTest()`, `performScatterDiagnostic()`, `runPanelTests()`: `id`, `time` and `vars` read at the confirmed rows (`rcontext_columns_at_fit()`); repeated pairs of `id` and `time` refused | B3, B4, B16, R2 (decision 13); N17 for these. Pesaran's test refuses an unbalanced panel, as that of a fit that dropped some rows of some units, until the pull request of N12 (decision 28); `na.exclude` is read as before until 9 (decision 29, done) | **Done, 0.16.0** |
| 6 | The tests computed from the model alone (the context applies `rcheck_model_frame()` to them), and the check of `data` RESET keeps until then; Cook-Weisberg with `var_formula`; `performVarianceFormTest()`; `fitWLS(data, var_formula)`: `rcontext_second_formula()`, `rcontext_formula_rows()`; `rvariance_frame()` goes. The tests that read nothing from `data`, RESET included, match it through the guard, as every test does (decision 31): the check of 0.15.0 goes, but for its check of the type of `data` | B9, R3, N6; M7, M8 and "frame" for these; decision 31 | **Done, 0.16.0** |
| 7 | The three streaming tests, chunk by chunk from the stored design (`rstreaming_design()`) | B6; B12, B14, L1 for these, with B5, L2, N7, N17 and R4 (B12 and B15 gave values already); a smooth `gam` kept on the design of its variables, as before (decision 23) | **Done, 0.16.0** |
| 8 | The resampling tests and `bootstrapTestStatistic()`, replicates from the stored design (`rreplicate_lm()`): `performWhiteTestBootstrap()`, whose observed statistic and replicates moved together, and `rwhite_test_rebuilt_design()`, which kept them on the rebuilt design until then, is gone; `performWildBootstrapTest()`; `bootstrapTestStatistic()` and the robust bootstraps, which no longer refuse a fit whose frame cannot be rebuilt from `data`, a Gaussian `glm` bootstrapped as its `lm`; the robust White and Breusch-Pagan tests, whose own preparation of `data` goes | B12 for replicates, N8, N17 for the White bootstrap; B5, B14, B15, L1, L2, N7 for these; M7, M8 and "frame" for these | **Done, development version (0.17.0)** |
| 9 | `na.exclude`: unpadded residuals in the checks (`rmodel_unpad()`) and in every test (`rcontext_pearson()`, `rcontext_fitted()`, `rcontext_unpad()`), padded outputs per observation (`fitWLS()`); the positional reading of the panel tests and the scatter diagnostic goes, with `rcheck_data_in_fit_order()`, `rcheck_data_matches_model()` and `rstop_unbalanced_panel()` | B13; N12 on the positional reading | **Done, development version (0.17.0)** |
| 10 | The runners and the clean-up: grouped `fitWLS()` refused (decision 10, `rrefuse_grouped_fitwls()`), workflows with `data` (N5, mapped by the workflow's preprocessing, a transformed outcome among them, decision 35); `prepare_model_data_for_test()`, `rdata_or_model_frame()`, `rcheck_data_rows()`, `rcheck_rows_after_cleaning()` and the copies of the old preparation go; with N5, `recommendRemediationStrategies()`, `generateHeteroRecommendations()` and `generics::augment()` match the rows of `data` to the observations of the fit (`rcontext_data_rows()`, `rcontext_data_match()`), where since increment 9 they refuse data of another number of rows, `augment()` binding as broom does (decision 34) and tracing the rows a workflow's preprocessing maps to the rows of `data` (from the second review, `.ht_workflow_mapped()`); the results keep what `augment()` needs of a workflow (`rtidymodels_map()`) | B11, N5 | **Done, development version (0.17.0)** |
| 11 | `MASS::rlm()` refused in `rcontext_kind()` (decision 4, `rrefuse_rlm()`), on every route, a grouped run among them, the runners keeping it final; and the same question for a penalised `mgcv::gam()` with smooth terms, settled for every test together (decisions 37 and 38): tested on its stored design by the tests that read only its residuals, fitted values, columns of `data` or its design as auxiliary regressors, the streaming tests included; refused by those that fit it again by least squares or whose null distribution is derived for least-squares residuals (`rrefuse_penalised_gam()`); a `gam` that is the least-squares fit of its design taken by every test (`rgam_least_squares()`) | R6, N10; decision 23 | **Done, development version (0.17.0)** |

Outside the context, as separate pull requests: White's degrees of freedom
when the auxiliary columns span the constant (N2, done) and White streaming on
collinear auxiliary terms (N3, done in 0.17.0) (decision 22), and N11 to N15.

### How each increment shows `identical()`

`scripts/model-context-identity.R` (in `.Rbuildignore`) runs a corpus of calls
in two R processes: against `main`, installed with
`R CMD INSTALL --library=<private library>`, never into the default library,
and against the branch, with `pkgload::load_all()`. The corpus is every recipe
of `tests/testthat/helper-test-recipes.R` under `set.seed()` on every cell of
the test matrix below, the default battery of `runHeteroTests()` on every
cell, and the examples of the help pages of the migrated functions; increment 1
adds `performBPTestRobust(studentize = FALSE)`,
`bootstrapTestStatistic(performKoenkerTest)` and
`performWhiteTestRobust(bootstrap = TRUE)`. It lists `identical()` per call;
every call that differs must be in an allow-list of the cells of the defects
the increment closes, a message may change only to the test's own documented
refusal, and on a `poly()` cell the branch must equal `main` on the stored
design: the fit refitted with `x = TRUE`, with the five functions traced on
`main` so that a model they are given without its design, as the refits of a
bootstrap are, gets its own first. Where `runHeteroTests()` falls back on
another test on the stored design too, `main` has no stored-design value of
the test it names, and the rules of the runner decide. No stored values are
committed.

Increment 1: 11,271 calls on 262 cells, of which 10,680 identical, 591
allowed changes and 0 unexpected; no call that returned a value now stops. The
66 `poly()` calls with a value are each `identical()` to `main` on the stored
design, and 27 of them differ from `main`'s rebuilt design, by at most 1.3e-15
relative on the statistic. `performWhiteTestBootstrap()` is `identical()` to
`main` on every cell. The warnings of the calls of the corpus are those of
`main`; outside it, Koenker's test gives the warnings of the checks of the
model once, where it gave them twice. The studies in
`inst/validation/` that the increment touches (the full sweep, Pass A, blocks
1 to 3 of the weighted fits, the survey designs, the offset refits of
Koenker's and White's tests, and the White bootstrap on a transformed
response) use data in fit order and no `poly()` terms, and their rows for the
five functions are reproduced byte for byte.

Increment 2 adds the routes of its four tests to the corpus (the registry,
`test_factory`, `rvalidate_against_reference("studentized_bp")`,
`performBPTestRobust(bootstrap = TRUE)`, `runDiagnostics()$reset`, and
`runHeteroTests()` with the registered `"student_bp"` and
`"quantile_regression"`), the examples of their help pages, and 13 cells: an
`lm` and a Gaussian `glm` with a zero weight and a `model = FALSE` fit whose
data were reordered after the fit, and, from the review of increment 2, the
classes whose answer differs between the tests (the multivariate `lm`, the
`gam` with a smooth, the zero weights, the `model = FALSE` fit whose data are
gone) without `data` and with their rows shuffled. Every allowed change to a
value is also verified against a reference (`identity_verify()`), on every
number of the result, its estimates, replicates and robust details included:
the same call on the cell it must reproduce, itself unchanged from `main` or
verified in turn; the test a route computes on the same cell; the fit with
the columns of its terms computed beforehand; the model of `y - o`; for
RESET, the nested F test written out with `anova()`; or `main`'s value on the
stored design. An allowed change that is not verified counts as unexpected. Goldfeld-Quandt on
an unweighted fit refitted its segments from the formula on `main`, so `main`
has no stored-design value of it on a `poly()` term; there the branch must
equal `main` to `1e-12` relative.

Increment 2: 14,037 calls on 275 cells, of which 13,071 identical, 966 allowed
changes (655 errors that become values, 53 values that change, 234 messages,
24 new refusals) and 0 unexpected; the 708 changes to a value are each
verified against a reference. The 171 `poly()` calls of the functions built on
the context that `main` computes on the stored design are `identical()` to
it; Goldfeld-Quandt's 7 `poly()` calls on an unweighted fit differ from `main`
by at most 8.1e-15 relative. The warnings are those of `main` but for 43
calls: 39 that stopped and now return a value with the warning of
`quantreg::anova.rqs()` on the same design, and four calls of
`performStudentizedBPTest()` on a multivariate `lm` and on a `model = FALSE`
fit whose data are gone, without `data`, which it now refuses before its
deprecation warning. The
rows of the four tests in the full sweep, Pass A, block 1 of the weighted fits
and the Goldfeld-Quandt rows of the Harrison-McCabe study use no offset and no
term that depends on its rows, and are reproduced byte for byte. One cell is
added to `offset-refits-size.R` (decision 19): RESET on the fit with an offset
rejects 0.0492 of the time under its null at 5000 replications, inside the
release gate, [0.042, 0.058].

Increment 3 adds the routes of its tests to the corpus (the registered
`"szroeter"` and `"spatial_hetero"`, their `test_factory` entries, and
`runHeteroTests()` with each name, the spatial test given its weights as
`attr(data, "listw")`), the examples of their six help pages, a cell (the `gam`
without smooth terms given its rows shuffled, N17), and the five cells of
Harrison-McCabe's rank (`context_rank_cells()`), run through every recipe,
whose reference is the test written out with the rank of the fit from the
fit's own QR decomposition (`context_hmc_by_hand()`), to `1e-6` (they agree to
1e-8 on a column at 1e-9 of the others). From the review of increment 3, two
cells of the class block: a `gam` with an `offset` argument, with the same
`gam` with an `offset()` term as its reference ("gam_offset", for the
registered Szroeter test), and a fit with `model = FALSE` whose data are gone,
given them as a shuffled tibble ("frame"). A missing argument has class `getvarError` in the
byte code of the installed package and `evalError` under
`pkgload::load_all()`; the harness leaves both out.

Increment 3, with the cells of its review: 16,212 calls on 279 cells, the 5
cells of Harrison-McCabe's rank and the examples of sixteen help pages, of
which 15,618 identical, 594 allowed changes (229 errors that become values, 35
values that change, 322 messages, 8 new refusals) and 0 unexpected; the 264
changes to a value are each verified against a reference: 240 against the
call on the cell they reproduce, 3 through two cells, 18 through the
registered spatial test on the same cell, and the 3 of Harrison-McCabe's rank
against the test written out from the fit's own decomposition, to `1e-6`. The
8 new refusals are those of the `model = FALSE` fit given a shuffled tibble:
five of the six tests (Harrison-McCabe stopped already), the registered
spatial test, its `test_factory` entry and the runner with it. The 35 values
that change are the 3 of N4, the 10 of N17, the 2 of the registered Szroeter
test on the `gam` with an `offset` argument, directly and through the runner,
and 20 calls of
`runHeteroTests(tests = "spatial_hetero")` on the model frame as `data` and on
numbered data in fit order, where the spatial test stopped and the runner put
its Breusch-Pagan fallback under its name. The 256 `poly()` calls that `main`
computes on the stored design are `identical()` to it. The warnings are those
of `main` but for 3 calls of Hartley's test that stopped and now return a
value with its warning on unequal groups. The rows of the six tests in Pass A
(75,000 p-values of Park's, Glejser's and Szroeter's tests at 5000
replications), the Glejser study (440,000 at 5000) and the Harrison-McCabe
study (blocks 2 to 5 at 5000 replications, 280,000 p-values, and the null
distribution of block 1 at 100,000 samples) are `identical()` to `main`,
replication by replication, and reproduce the committed tables row for row.

Increment 4 adds the examples of the five help pages to the corpus, and cells
in every block the five read differently: a group missing at observations of
the fit, in the data frame, shuffled, as a tibble, in fit order and shuffled,
on a fit that dropped rows given the full data, shuffled and as its complete
rows renumbered, and in a model of the group without and with `data` (the
block `group/`); a model of the group with a `log()` term
(`log(ypos) ~ log(x1) + group`) or an `offset()` term, in every form of data,
for L1 and L2; and, from the lessons of increments 1 to 3, the classes run in
every form of data (the data frame, `NULL`, shuffled with its own row names,
a shuffled tibble, the model frame): the multivariate `lm`, the `gam` with
smooth terms, the `gam` with an `offset` argument, the zero weight of an `lm`
and of a Gaussian `glm`, the `model = FALSE` fit whose data are gone (but its
model frame, which cannot be built), and a fit whose coefficient was changed
after the fit (M8). None of the five is registered, so their calls are the
direct ones; the runner keeps a refusal of the context final for a diagnostic
registered by the user that calls one (`test-model-context.R`).

Increment 4: 19,523 calls on 337 cells, the 5 cells of Harrison-McCabe's
rank and the examples of twenty-one help pages, of which 19,148 identical,
375 allowed changes (55 errors that become values, 15 values that change, 290
messages, 15 new refusals) and 0 unexpected, every one of them a call of the
five; the 70 changes to a value are each verified against a reference, the
call on the cell they reproduce: the data frame of the fit, the fit to the
complete rows, the data of a `gam` in its order, or a model of the group
given its data. The 15 values that change are those of N17, a `gam` with and
without smooth terms or an `offset` argument given its data shuffled; the 15
new refusals the `model = FALSE` fit given a shuffled tibble ("frame") and the
edited fit given its data or its rows shuffled (M8). The 304 `poly()` calls
that `main` computes on the stored design are `identical()` to it. The
warnings are those of `main` but for 5 calls that stopped and now return a
value with the warning on the missing group, the warning of the same call on
the full data. The rows of the five in Pass B (`pass-b-size-power.R`, five
scenarios at 5000 replications: 125,000 p-values of the five, and 25,000 of
Hartley's test), in the full sweep (6,000 p-values at 400 replications) and in
block 1 of the weighted fits (2,000 at 400) are `identical()` to `main`,
replication by replication; they reproduce the committed tables row for row,
the table of Pass B byte for byte. Pass B took 603 s against 516 s on `main`,
the two run side by side on a shared machine.

Increment 5 adds the routes of its four functions to the corpus: the recipes
of the two panel tests, which `helper-test-recipes.R` holds and the earlier
increments ran unchanged, the scatter diagnostic on `x1` and `x2`, and
`runPanelTests()` with both of its tests, with the random-effects test alone,
without `time`, and with Pesaran's test alone (none of the four is
registered); the examples of their four help pages, each expression that
calls the function of its page, as the scatter diagnostic and the runner
return no `htest`; and 19 cells: the block `panel/`, a fit that drops every
row of a unit, by an `lm`, a weighted `lm`, a weighted Gaussian `glm` and a
`gam`, in fit order, shuffled and, for the `lm`, as a tibble, each against
the fit to the complete rows, an unbalanced panel, unit 1 observed at 5 of the
10 periods, fitted to those rows and dropped from the full data (N12), and a
repeated `(id, time)` in fit order and shuffled; and the fits to the complete rows of the fit with an indicator
computed beforehand and of the model of a group missing at two rows, the
references of the panel tests on those cells (`context_dropped_references`).
The allow-list gains B3 and B4 (errors that become values, on data shuffled
under their own row names, rows after those of the fit and the data of a
`model = FALSE` fit given shuffled), B16 (errors that become values on the
data of a fit that dropped rows), N12 (the refusal of an unbalanced panel by
Pesaran's test and `runPanelTests()`, decision 28: new refusals where it
recycled the residuals of a unit, and messages where it stopped on such a
panel or refused the full data of a fit that dropped rows for their number)
and R2 (new refusals of Pesaran's test and of `runPanelTests()` on a repeated
pair, and messages on a fit to repeated rows), each scoped to the functions they
concern, and N17, "frame", M8 and the rule on ties for these;
`identity_m1_panel()` allows M1 without `data` where the panel tests said
"`id` and `time` must be columns in `data`.". Since `main` holds N2 and its
follow-up, a call of White's test identical to `main` counts as identical
rather than being checked against the reference of N2 (`identity_n2_mode()`):
the first run against such a `main` counted White's identical refusals of an
exact fit as allowed changes, and counted as unexpected the runner on the
saturated design of the follow-up (`runner | runHeteroTests |
white/saturated`), whose fallback on White's test without cross products is
the correct one (decision 25) and is `identical()` to `main`'s: a false alarm
the rule removes.

Increment 5, with the refusal of an unbalanced panel (decision 28): 22,542
calls on 356 cells, the 5 cells of Harrison-McCabe's rank, the 8 cells of the
follow-up of N2 and the examples of twenty-five help pages, of which 21,976
identical, 566 allowed changes (334 errors that become values, 18 values that
change, 192 messages, 22 new refusals) and 0 unexpected, every one of them a
call of the four or of a route of `runPanelTests()`; the 352 changes to a
value are each verified against a reference: 325 against the call on the
cell they reproduce (the data frame of
the fit, the fit to the complete rows, the data of a `gam` in its order), 9
through two cells, and 18 against the fit with the factor of `cut()` computed
beforehand, directly or through the data frame of its cell. The 18 values that
change are those of N17, a `gam` with and without smooth terms or an `offset`
argument given its data shuffled; the 22 new refusals are the 6 of R2, the 3
of an unbalanced panel on which Pesaran's test recycled the residuals of a
unit and returned 0.234 (N12: the test, `runPanelTests()` and its option
`tests = "pesaran"`), the 6 of the `model = FALSE` fit given a shuffled tibble
("frame") and the 7 of the edited fit (M8), the scatter diagnostic on the
model frame among them. Of the 192 messages, 82 are M1, 66 the refusal of an
unbalanced panel (N12), where Pesaran's test stopped with "number of items to
replace is not a multiple of replacement length" (12) or refused the full data
of a fit that dropped rows for their number (54), 23 M7, 8 M8, 6 the refusal
of R2, 5 the refusal of ties and 2 the frame message. No call of the branch
gives the error of the assignment, which 15 records of `main` gave. The 328
`poly()` calls that `main` computes on the stored design are `identical()` to
it. The warnings are those of `main` but for 54 calls on the 18 cells of a fit
that dropped some rows of some units: the random-effects test, which stopped
and now returns a value with its warning on unequal panel lengths, directly
and through `runPanelTests()`, and `runPanelTests()` with both tests, which
gives that warning before Pesaran's test refuses the panel. The rows of the two panel tests in the full sweep (1,600 p-values
at 400 replications) and in block 1 of the weighted fits (800 at 400) are
`identical()` to `main`, replication by replication, and their rates are those
of the committed tables.

Increment 6 adds the routes of its functions to the corpus: `fitWLS()`, as the
vector of its coefficients and estimated weights, without and with
`var_formula = ~ z` and with `var_formula = ~ x1, form = "power"`;
Cook-Weisberg's and the NCV tests with `var_formula = ~ z` and the variance
form test with `var_formula = ~ z` and `against = ~ z`, `z` being a variable no
model frame of the matrix holds; the same three with a variable of the
workspace, `zw`, one value per observation (R3); the registered
`"cook_weisberg"`, `"ncv"`, `"spread_level"` and `"variance_form"`;
`runTimeSeriesTests()`; `runHeteroTests()` with the three registered names
it runs without a deprecation warning; and the examples of the eleven help
pages. And five cells: a `model = FALSE` fit whose data changed after the
fit, given its data (N6); a `model = FALSE` fit whose data are kept, with and
without `data`; and `z` missing at an observation of the fit, and at a row
the fit dropped (the block `variance/`). The allow-list gains B9 (new
refusals by the guard, and its messages, on data it refuses, for the calls of
a second formula: `identity_defects()` tags the cells the guard refuses and
the tibble of a fit to repeated rows), R3 (new refusals and messages, M1 or
the message of `data`, for those calls on every cell) and decision 8 (the
message of Cook-Weisberg's test on a missing `z`), each scoped to those calls,
N6, "frame", M8 (not for `fitWLS()`) and N17 (errors that become values
and values that change, for a second formula on a `gam`) for the functions of
the increment, and decision 31 for the calls that read nothing from `data`
(the nine, their registered names, RESET, `fitWLS()` without `var_formula`
and the runner with the registered names), on every cell with `data`: new
refusals and messages, the guard's, and errors that become values, each
verified. From the review of increment 6, the calls of a second formula with
a constant (`rformula_constants()`): Cook-Weisberg's test with
`var_formula = ~ I(x1^k)`, `k <- 2` in the recipe, and with `~ I(x1^2)`, its
reference, the variance form test with `~ I(x1^k)` and `fitWLS()` with
`~ I(pi * x1)`, with "constant" for the errors that become values of
Cook-Weisberg's test given `data`, which refused `k` as a variable `data`
lacked, each verified against the same formula written with the number.

Increment 6, with decision 31 and its review, against `main` with #158
(0fb5138): 30,344 calls on 361 cells, the 5 cells of Harrison-McCabe's rank,
the 8 cells of the follow-up of N2 and the examples of thirty-six help
pages, of which 26,254 identical, 4,090 allowed changes (341 errors that
become values, 9 values that change, 2,674 new refusals, 1,066 messages) and 0
unexpected, every one of them a call of the functions of the increment; the
350 changes to a value are each verified against a reference: 121 against
the call on the cell they reproduce (the data frame of the fit, the fit to
the complete rows, the data of a `gam` in its order), 14 through two cells,
180 against the same formula written with the number (the constant `k`), and
35 through the test a route computes and one or two cells. Of the 2,674 new
refusals, 1,613 are of the tests computed from the model alone, their routes
and `fitWLS()` (1,491 of data the guard refuses, decision 31, on the cells of
numbered data in another order, rows in front of those of the fit and the
shuffled tibbles of the classes; M8 75, N6 32, "frame" 14, M7 1), 485 of the
calls of a second formula of `z`, `x1` or a constant (B9 on the data the
guard refuses, M8, N6, "frame"), and 576 of those of the variable of the
workspace (R3, or a refusal that comes first). The 341 errors that become
values are 189 of Cook-Weisberg's test with the constant `k` and `data`
("constant"), 90 of decision 31 (numbered data in the order of the fit and
the data of a `gam` that dropped rows, six cells through the fifteen calls
that read nothing from `data`), 18 of a `model = FALSE` fit whose data are
gone given its data ("frame"), and 44 of a second formula ("frame", B14 and
N17); the 9 values that change are those of N17. Of the 1,066 messages, 207
are of decision 31, where the guard now refuses first data on which the test
refused another defect (weights, a zero weight, a smooth term) or repeated
row names, which it refused as missing rows. The calls with a constant
change only in those categories, as the calls of `z` do: no value of `main`
changes, and Cook-Weisberg's test given `data` gives the value of the same
formula with the number, where it refused `k`.
The 530 `poly()` calls with a value that `main` computes on the stored design
are `identical()` to it. The warnings are those of `main` on every call.
Before decision 31 the counts were 27,107 identical and 1,741 allowed, of
28,848, against 217c093, and at decision 31 25,319 and 3,529.

The pull request of N12, outside the increments, lifts the refusal of an
unbalanced panel by Pesaran's test (decision 28): the calls that `main`
refused for it now give a value, which must be that of
`plm::pcdtest(test = "cd")` on the Pearson residuals of the fit, a pseries
indexed by the units and periods of the rows of `data` that carry their
names (`identity_plm_cd()`), to `1e-10` relative on the statistic and the
p-value, on the cell or on the cell it reproduces; the allow-list gains N12
among the errors that become values, scoped to Pesaran's test and
`runPanelTests()`, and a change of N12 not verified against plm is
unexpected. N12 leaves the new refusals, and its message, for N12 and B16,
is only the refusal of the positional reading of a fit with `na.exclude`,
which names it (from the review of the pull request). The corpus is that of increment 6, with plm's value recorded on
each unbalanced cell; against `main` with #159 (e05a568): 30,344 calls, of
which 30,272 identical, 72 allowed changes and 0 unexpected. The 72 are
errors that become values, Pesaran's test, `runPanelTests()` and
`runPanelTests(tests = "pesaran")` on the 24 unbalanced cells of the matrix
(the fits that dropped rows of some units, given their data in every
accepted form, the `cut()` term on the unweighted and the weighted fit, the
missing groups, `z` missing at a row the fit dropped, and the panel of unit 1
at 5 of its 10 periods), each verified against plm: 54 on the cell, 18
through the cell they reproduce (the tibbles and the renumbered rows, whose
rows do not carry the names of the residuals). Every other call is
`identical()` to `main`, those of a balanced panel among them, and the 536
`poly()` calls on the stored design too; the warnings are those of `main`.
Against `main` before #159 (0fb5138), on the corpus of increment 5: 22,542
calls, 22,473 identical, 69 allowed, the same changes on 23 cells, and 0
unexpected.

Increment 7 adds the routes of the three streaming tests to the corpus: their
recipes of `helper-test-recipes.R`, in one chunk of 10,000 observations,
which the earlier increments ran unchanged, the same tests in chunks of 7 and
of 40 (`identity_streaming_chunked`), and `runHeteroTests()` with the three
asked for on data of any size, `chunk_threshold_mb = 0`, in chunks of 10,000
and of 7 (`identity_streaming_runners`), on every cell, among them reordered
data, rows after those of the fit, the `gam` fits, the Gaussian `glm`, the
weighted fits and the `model = FALSE` fits; and the examples of their three
help pages. No cell is added: the cells of B6 are the term cells of
`I(x1 > median(x1))` and `cut(x1, quantile(x1))`, tagged `B6`
(`context_cell_defects()`), and `missing/median_full`. The allow-list gains B6
for these calls (values that change, and errors that become values), and N17,
"frame" and M8 are scoped to them; the defects of the cells that concern
every test built on the context (B5, B12, B14, B15, L1, L2, N7, R4) apply to
them as to the others. Three rules are added. White's streaming test, on a
cell of collinear terms (`context_white_streaming_collinear`) where it
stopped before its auxiliary regression (L1, L2, N7, B6), may stop on those
terms (N3, outside the context; `identity_defects()`). On a `poly()` term
main has no stored-design value of the streaming tests, whose chunks computed
the basis again by the recurrence of the `predvars` of the terms, where the
design of the fit holds the basis `poly()` computed by its QR decomposition,
so a value can change in its last bits; the branch must equal main to `1e-12`
relative, as Goldfeld-Quandt's segments, where both ran the same tests
(`identity_same_tests()`). Of the 128 such calls, 8 of Goldfeld-Quandt and
120 of the streaming tests, 77 stop as on `main`, 72 of them refusals of
collinear terms by White's streaming test, and the 51 with a value, 48 of the
streaming tests, are `identical()` to `main`; in 621 calls of the review of
this increment, on other designs, 38 `poly()` values changed, by at most
9.9e-14 relative (`performBPTestStreaming(lm(y ~ poly(x1, 2) + x2, d), d)`
156.20252285478642 on `main` and 156.20252285478654). And a runner entry
that runs the test it names where `main` ran a fallback carries the data.name
of that test, the formula of the model (`identity_data_name_change()`).

Increment 7, against `main` with increment 6 (e05a568): 33,339 calls on 361
cells, the 5 cells of Harrison-McCabe's rank, the 8 cells of the follow-up of
N2 and the examples of thirty-nine help pages, of which 32,577 identical, 762
allowed changes (378 errors that become values, 184 values that change, 145
messages, 55 new refusals) and 0 unexpected, every one of them a call of the
three streaming tests or of the runner that streams them; of the 4,117
streaming calls, 3,355 are `identical()` to `main`. The 562 changes to a value
are each verified against a reference: 432 against the call on the cell they
reproduce, 26 against the fit with the column of the term computed
beforehand, and 104 through both. The 184 values that change are 66 of the
tests themselves, B6 and B5 on the cells of the median in chunks of 7 and 40
and on the fit that dropped rows, and N17 on the `gam` fits given their data
shuffled, and 118 of the runner, which put a fallback under the names of the
tests where they stopped (White's test without cross products, the NCV test
under `breusch_pagan` and `koenker`) and runs them; the 55 new refusals are
those of the edited fit (M8, 44) and of the `model = FALSE` fit given a
shuffled tibble ("frame", 11). Of the 145 messages, 81 are the refusal of
collinear terms by White's streaming test (N3), 55 M7 and 9 the frame
message. The 536 `poly()` calls that `main` computes on the stored design are
`identical()` to it, and the warnings are those of `main` on every call.
Against `main` before increment 6 (0fb5138), on 356 cells: 25,497 calls,
24,735 identical, the same 762 allowed (386 errors that become values, 176
values that change) and 0 unexpected; the 8 calls that differ in kind are the
runner on numbered data in the order of the fit (B14), which stopped there
and, with decision 31, puts the NCV fallback under the names of the tests.

The pull request of N3, outside the increments (decision 22), changes how
the three streaming tests fit their auxiliary regression: from the triangular
factor of the centred design, chunk by chunk, where they solved the normal
equations of uncentred cross-products. The allow-list gains N3
(`identity_n3_verdict()`), scoped to the streaming tests, the runner that
streams them and their help pages: a value that changes must equal `main` to
`1e-12` relative on every number, or the exact test of the same call to `1e-8`
on the statistic and the p-value with its degrees of freedom, and be nearer to
it than `main` was; an error that becomes a value must carry the message of N3
or of the threshold on squared residuals and equal the exact test. The rule of
increment 7 that let White's streaming test stop on collinear terms goes. Each
run also records the exact test of the examples of the streaming help pages.
Against `main` (7493afd, the release of 0.16.0): 33,339 calls, of which 30,683
identical, 2,656 allowed changes and 0 unexpected. All 2,656 are calls of the
streaming tests or of the runner that streams them: 481 equal the exact test of
the same call, to `1.3e-11` (261 errors that become values, 249 of them
White's on collinear terms; and 220 values that change, 33 of White's streaming
test, where `main`'s normal equations lost digits, and 187 of the runner, which
put a fallback under the name of a test that stopped), and 2,175 differ from
`main` in their last bits only, by
at most `5.1e-13` relative on any number. The 536 `poly()` calls on the stored
design are `identical()` to `main`, and no call's warnings differ.

Increment 8 adds the routes of the resampling tests to the corpus, all seeded
(`set.seed(7)` before each call, `B = 19`; `identity_resampling_recipes`): the
wild bootstrap with the Mammen multipliers besides the Rademacher ones of its
recipe; `bootstrapTestStatistic()` of White's, the Breusch-Pagan and Glejser's
statistics besides Koenker's, and of Koenker's with pairs resampling; the
robust Breusch-Pagan bootstrap without studentizing; the White and wild
bootstraps and `bootstrapTestStatistic()` of a `fitWLS()` fit of the model of
each cell, whose weights each replicate estimates again; the runner with the three
resampling tests the package registers, at their 999 replicates, on 89 cells
of every block and on the references of their changes; and the examples of
four more help pages. Each run records the state of the generator after
every call (`identity_rng_state()`), and a call `identical()` to `main` must
leave it as `main` left it. The workers of the socket cluster of a parallel
example load the package from `R_LIBS`, which both runs set to the private
library of `main`. The allow-list gains, for these calls: B12, L1, L2, B14,
B15, N7 and B5 among the errors that become values of the bootstraps
(`identity_bootstrap_allowed`), B5 for pairs resampling on a term computed
from the rows (poly(), ns(), the median, cut()), N17 for the White bootstrap
and the runner, N8 for the bootstraps on a Gaussian `glm` (a value verified
against the same call on the weighted `lm` of its cell, to `1e-8`; the
message of N8 on the cells of a `gam` only, so that a refusal of a valid
Gaussian `glm` is unexpected; and on a `gam` or a `glm` each refusal of the
context that now comes first on the cells where it applies, a zero weight on
the cells of a zero weight, the guard on the cells of the guard and the refit
of the nearly collinear `glm` on the cells of Harrison-McCabe's rank), "gam"
for the White bootstrap and
the runner, and M8 and "frame" for the five. Four rules are added. On a cell
where White's count changed (N2), a message or a new refusal of one of these
calls is judged by the other rules, and a value where `main` stopped is
verified by the chain. The bootstraps whose
replicates follow the order of `data`, on a `poly()` cell shuffled under its
own row names, have no stored-design value on `main`, whose refits computed
the basis again on the rows in the order of `data`: the branch must equal
`main` to `1e-12` relative (`identity_reordered_replicates()`). In the runner
with the resampling tests, a test that ran where `main` ran a fallback drew
random numbers `main` did not, and the rules of the runner decide the tests
after it. And the bootstraps of a `fitWLS()` fit on the class block, which
`fitWLS()` refits where the tests refuse the fit, reproduce the call on the
cell of the class in the data frame of the fit. The review of increment 8
adds four cells of the coding of a factor of four levels, two of them on two
rows each (`identity_coding_cells()`): the factor, the same values as a
character column, the factor coded by `contrasts = list(f = "contr.sum")` and
the factor with `contr.sum(4)` as its contrasts attribute, through every
recipe and the runner with the resampling tests; each reproduces the factor,
to `1e-10`. On the factor coded by `contrasts =`, `main` refitted the
replicates from the formula without the contrasts of the fit, on another
basis of the same columns, and the branch must equal `main` to `1e-12`
relative ("coding"). M8 is allowed for the bootstraps of a `fitWLS()` fit of
a `gam` with smooth terms, which the corpus no longer skips.

Increment 8 with its review, against `main` at the merge of N3 (961431f), the
base of increment 8: 37,194 calls on 361 cells, the 5 cells of
Harrison-McCabe's rank, the 8 cells of the follow-up of N2, the 4 cells of the
coding of a factor and the examples of forty help pages, of which 35,965
identical, 1,229 allowed changes (810 errors that become values, 93 values
that change, 304 messages, 22 new refusals) and 0 unexpected, every one of
them a call of the resampling tests, the robust tests or the runner with the
resampling tests: of the 6,524 such calls, 5,295 are `identical()` to `main`.
The 903 changes to a value are each verified against a reference: 607 against
the call on the cell they reproduce, 76 through it and the fit with the
columns of the terms computed beforehand, 30 against that fit directly and 28
against it given the rows in the same order (`precomputed_shuffled`), 138
against the weighted `lm` of a `glm` (N8), 13 against `main` on the stored
design and 11 through two cells or a cell and the stored design. Counted by
the most specific of their defects, the 810 errors that become values are 258
of L2, 172 of L1, 137 of N8, 120 of N7, 66 of B14, 26 of B5, 21 of B12, 6 of
B15 and 4 of "frame"; the 93 values that change are 20 of B5 (18 of pairs
resampling), 4 of N17 (the White bootstrap on the three `gam` cells shuffled
and the runner), 16 of the `poly()` cells, equal to `main` on the stored
design or to its last bits, 6 of N2, 11 of the factor coded by `contrasts =`
("coding", within 7.0e-13 relative of `main`) and 36 of the runner with the
resampling tests, which put White's test under the names of the White and
wild bootstraps where they stopped (B12, L1, B14, B15, N7, "frame"); the 22
new refusals are those of the edited fit (M8, 12), of the White and wild
bootstraps of a `fitWLS()` fit of a `gam` with smooth terms in four forms of
data (M8, 8), of the `model = FALSE` fit given a shuffled tibble ("frame", 1)
and of the runner on a `gam` with smooth terms ("gam", 1). Of the 304
messages, 189 are those of `bootstrapTestStatistic()` and the robust
bootstraps on a `glm` or a `gam` (N8: 98 the refusal of the `gam`, 55 the
guard, 20 a zero weight, 16 the refit of the nearly collinear `glm`), 71 M7
(25 of them of `bootstrapTestStatistic()`, which stopped with "'length = 4'
in coercion to 'logical(1)'"), 12 the frame message, 4 the refusal of a `gam`
with smooth terms by the White bootstrap, 4 M8 (3 of `bootstrapTestStatistic()`
on a `fitWLS()` fit of a `gam` with smooth terms, which stopped with
"(subscript) logical subscript too long", and the runner on the edited fit)
and 24 the refusals of the bootstraps themselves, the transformed response
under null-imposed resampling among them, reached where the calls stopped
before them (L1, L2, B12). Every call `identical()` to `main`, 35,965 of
35,965, leaves the generator as `main` left it; so do 493 of the 1,229
allowed changes, 59 of the 93 changes to a value. The 588 `poly()` calls that
`main` computes on the stored design are `identical()` to it; of the 184 that
are judged to the last bits, the 12 order-dependent bootstraps on the shuffled
`poly()` cells among them, 125 give a value, 113 `identical()` to `main` and
12 within 8.4e-14 relative. The warnings are those of `main` but for 57
calls: 33 of the White bootstrap and the runner, whose replicates no longer
run the checks of the model and give their warnings on the residuals of a
replicate ("Residual outliers detected at rows ..."), 12 that stopped and give
a value with the warning of the same call on the data frame of the fit, and
12 on the factor with a contrasts attribute, on which `main` warned
"contrasts dropped from factor fs" once for each model frame its refits from
the formula rebuilt, and under pairs resampling also "... due to missing
levels" on a replicate that left a level out (11 of the 12 calls leave none
out), where the branch rebuilds no model frame and gives no warning.

Increment 9 adds to the corpus the 30 cells of a fit with `na.exclude` of the
matrix (`missing/na_exclude` and the block `exclude/`, two `gam` fits among
them from its review) and the 16 twins they do not share with the other
blocks, run through every recipe, route and runner;
each run records the `na.action` of each such fit and its number of
observations (`na_action`). The reference of every call on such a cell is the
same call on its twin, the same fit with `na.omit` given the same data
(`identity_twin()`): the same value, `identical()`, verified as the reference
of the cell (`identity_verify()`), the weights of the `fitWLS()` recipes padded
as `naresid()` pads them (`identity_padded_twin()`), or the same refusal,
class and message; the same state of the generator; and the same warnings,
which the first version of the harness counted and did not enforce (from the
review of increment 9). The allow-list gains
B13 for the calls of the functions built on the context
(`identity_affected`), as an error that becomes a value, a value that
changes, a message and a new refusal, each allowed only when the call gives
what its twin gives, and a call on such a cell that does not, changed or
identical to main, is unexpected. Each side runs from a private installation
of its own code, and the workers of the socket cluster of a parallel example
load it through `R_LIBS`: the first run of this increment set `R_LIBS` to
main's library for both sides, so that the workers of the branch ran main's
code (from the review of increment 9).

Increment 9, with the fixes of its review, against `main` with the follow-up
of increment 8 (e20efcc), each side installed privately and its workers
loading it through `R_LIBS`: 41,604 calls on 406 cells, the 5 cells of
Harrison-McCabe's rank, the 8 cells of the follow-up of N2 and the examples
of forty help pages, of which 39,014 identical, 2,590 allowed changes (2,202
errors that become values, 89 values that change, 299 messages, 0 new
refusals) and 0 unexpected, every one of them a call on the 30 cells of a fit
with `na.exclude`. Of the 2,940 calls on those cells, 350 are `identical()` to
`main`, and all 2,940 give what the same call gives on the twin: 2,364 values,
110 the weights of a `fitWLS()` fit padded, 463 refusals and 3 skipped; all
2,940 leave the generator as the twin left it, and all 2,940 give the twin's
warnings, which the harness now enforces. The 2,291 changes to a value are
each verified against the twin: 2,052 as the reference of the cell, 231
through the test a route computes, and 8 against the weights of the twin
padded (`fitWLS()` of a `gam`). Of the 2,202 errors that become values, 2,095
were the refusal of the checks of the model ("Model residuals must be finite
before running ..."), directly or through a runner ("Diagnostic execution
failed for White: ..."); 25 RESET's "NA/NaN/Inf in 'x'"; 52 the refusals of
the positional reading of the panel tests and the scatter diagnostic, data in
another order than the fit or an unbalanced panel of the rows of `data` (24),
and the complete rows of the fit, which lack rows of `data`, the full data of
a subset and a unit missing at a row the fit dropped (28); 19 the refusal of a
`gam` whose design `model.matrix.gam()` padded, by the tests that run no
checks of the model ("Internal error: the parts of the model context differ
in length."); and 11 `fitWLS()` of such a `gam` and the bootstraps of it,
which stopped with "incompatible dimensions" on that design. The 89 values
that change are 87 of the panel tests, the scatter diagnostic and
`runPanelTests()` with its options, whose statistics were `NA` or computed
with the `NA` of the dropped rows (Pesaran's 0.713 for 0.189 on
`missing/na_exclude`), and 2 of RESET, which returned `F = NA` on the fits
that dropped the last ten rows, where its augmented design kept the first rows
of the padded fitted values. The 299 messages are the refusals of the twin
where the checks of the model refused the fit first: of `data` by the guard,
M1, R3, the variables `data` lacks, the weights of the variance form test, a
transformed response under null-imposed resampling, a `gam` with smooth terms
(decision 23), and the checks of `test_factory` and the registered names,
which refuse the full data of a fit that dropped rows ("Model reports 117
residuals but the supplied data has 120 rows") as for the twin. Every call
identical to `main`, 39,014 of 39,014, leaves the generator as `main` left
it; so do 2,119 of the 2,590 allowed changes, all 89 changes to a value among
them, the other 471 being resampling calls that now draw where `main` refused
before drawing. The 642 `poly()` calls that `main` computes on the stored
design are `identical()` to it, and the 125 of the 184 judged to the last bits
that give a value are `identical()` to `main`. The warnings are those of
`main` but for 1,087 calls, all on the cells of a fit with `na.exclude`, each
the twin's: the warning "Removed 3 observations due to missing values in x2"
and the others of calls that stopped before them. The first run of this
increment, against 974bd53 before the review, gave 40,816 calls, 38,412
identical, 2,404 allowed and 0 unexpected; it set `R_LIBS` to main's library
for both sides, and its counts are replaced by these.

Increment 10 adds to the corpus `recommendRemediationStrategies()`, with a
fixed list of test results, `generateHeteroRecommendations()` and
`generics::augment()` of a result built around the model of the cell, its
columns unnamed (`identity_rows10_recipes`), on every cell; for each cell
whose `data` is a data frame that is not already the rows of the fit in its
order, the reference of item 3, as main computes it: the same call on those
rows, written out from the names of the observations (`identity_fit_rows()`,
a grouped tibble keeping its groups), and for the report, since its review,
the report composed from main's own functions with the tests and the advice
on those rows and the profile on `data` as given
(`identity_generate_composed()`); the block `grouped/` (the grouped matrix of
`helper-model-context.R`: an `lm`, a weighted `lm`, the `fitWLS()` fit of the
`lm` and the model as a formula, on a tibble grouped by one column and by
two) through `runHeteroTests()` and `runDiagnostics()`; the block `tidy/`
(the tidy matrix: sixteen workflows, four of them from its second review,
and three parsnip fits, each without
`data`, with its data, as a tibble, shuffled and without the outcome) through
the two runners and, since the review, `augment()` of the runner's result;
the block `item3/`, from the review (a binomial, a Poisson and a Gaussian
`glm` with the log link, data that lack the variables of the model, of a fit
that dropped rows or not, and grouped data of a fit that dropped rows and of
one that did not), through the three functions of item 3; and the example of
`runDiagnostics()`. These calls are judged by a rule of their own
(`identity_increment10_verdict()`): a change to a value must be main's value
of the same call on the rows of the fit, for `augment()` each row bound to the
values main binds to it there, by name, the rows the fit dropped with `NA`
(`identity_augment_matches()`, decision 34), and for N5 main's value without
`data`, for `augment()` the columns of the fit main binds to the model frame;
a new refusal, or a message that changes, must be main's on the rows of the
fit, a refusal of the alignment where the cell calls for it (the guard on the
cells it refuses, or that the twin's cell expects, the refusal of tied rows),
the refusal the cell expects (of the context by kind, of data that lack the
variables of the model, of the tidy block by `context_tidy_expect()`, since
the review, where any refusal with the lead of N5 passed), or B11's on the
cells of a `fitWLS()` fit. A call on a fit with `na.exclude` must give what its
twin gives, `augment()` the rows of its twin with the rows the fit dropped
bound with `NA` in the columns of the fit (by name, or, given the rows of the
observations only, at the positions naresid() pads, `identity_augment_padded_at()`).
Since the second review, each result of a runner is recorded with the names
of its attributes and what it keeps of a tidymodels object
(`identity_runner_summary()`, `identity_tidymodels_summary()`), so that a
change to the object returned cannot pass as identical; a result of a runner
on a workflow or a parsnip fit without `data` must be main's but for that
attribute, which must be the record of `rtidymodels_map()` and no more
(`identity_tidymodels_kept()`); and each value of `augment()` of the tidy
block on a form of data must bind each row its own values, the engine's
prediction of that row alone, mapped by the preprocessing, and its outcome
less that prediction (`context_tidy_own()`, recorded as `own`), changed or
identical to main, where the rule compared the columns of the fit with those
of the model frame in their order.
Each side runs from its own private installation, its workers loading it
through `R_LIBS`.

Increment 10 with its four reviews, against `main` at the merge of increment 9
(8f55598), each side installed privately and its workers loading it through
`R_LIBS`, both run again with the code of decision 36 and the harness and the
cells of the fourth review, the harness recording what `main` returns too: 43,226 calls on the 406 cells
of the matrix, the 5 cells of Harrison-McCabe's rank, the 8 cells of the
follow-up of N2, the 4 cells of the coding of a factor, the 8 grouped cells,
the 7 cells of the block `item3/` and the 105 cells of the tidy block, and the
examples of forty-one help pages, of which 42,370 identical, 856 allowed
changes (245 errors that become values, 98 values that change, 252 new
refusals, 261 messages) and 0 unexpected, every one of them a call of the three functions
of item 3, of the grouped runs or of the runners and `augment()` on the tidy
block. The 41,605 other calls, every call of a test, a route or a runner of
the earlier increments among them, the names of the attributes of every
result of a runner compared, are `identical()` to `main` and leave the
generator as it left it: the clean-up changes no value. The 343 changes to a
value are each verified: 219 against `main`'s value of the same call on the
rows of the fit, 82 against `main`'s value without `data`, and 42, the
results of the two runners on the 21 tidymodels objects without `data`,
against `main`'s value but for `attr(, "tidymodels")`, which is the record of
`rtidymodels_map()` and no more: exactly its fields, no fitted model or
workflow, and the blueprint `identical()` to the workflow's (decision 36),
whose size is recorded and not judged. Of the changes, for
`recommendRemediationStrategies()`, `generateHeteroRecommendations()` and
`augment()`: 64, 64 and 43 errors that become values (the full data of a fit
that dropped rows, rows after those of the fit, and for `augment()` the full
data of a fit with `na.omit`); 3, 3 and 42 values that change (named data in
another order, and the full data of a fit with `na.exclude` shuffled under
their names, which `augment()` bound by position); 105, 1 and 106 new
refusals (numbered data in another order, repeated row names, tied rows in
the order of the fit, the frames of `model = FALSE` fits, data that lack the
variables of the model and a model the context refuses by kind; the report
refused most of them already, through the tests it runs); and 14, 122 and 15
messages. The grouped block gives 16 calls, 14 `identical()` and the 2
refusals of B11; the tidy block 315, 43 `identical()`, and, through each of
the two runners, 35 errors that become values, 24 values that change (21
without `data` and the 3 forms of the data of a parsnip fit of `fit()`, which
`main` took, each only by the attribute) and 45 messages, and through
`augment()` 4 errors that become values, 2 values that change (the data of a
`fit_xy()` fit shuffled under their names), 38 new refusals (data a
workflow's preprocessing cannot map, reorders or drops rows of, or the guard
refuses, which `augment()` bound by position, and the data of a `fit_xy()`
fit with case weights) and 20 messages. Of the 84
calls of `augment()` on the tidy block given a form of data, the branch gives
26 values, each binding every row its own values (`context_tidy_own()`), and
refuses 58; `main` gave 60 values, 22 of which pass that check: the others
bound rows the values of other rows (the shuffled data of every object it
took, the data of a recipe that sorts the rows) or cannot be checked row by
row (data without the outcome, the outcome of a step skipped on new data, the
outcome of `fit_xy()` given as a vector). Of the 3,030 calls on
the 30 cells of a fit with `na.exclude`, 2,978 are `identical()` to `main`,
and all give what the same call gives on the twin: 2,419 values, 139 outputs
per row padded, 469 refusals and 3 skipped; all leave the generator as the
twin left it and give its warnings. Every one of the 856 changes leaves the
generator as `main` left it. The 642 `poly()` calls that `main` computes on
the stored design are `identical()` to it, and the warnings are those of
`main` on every call. The 196 results of the runners on workflows keep the
blueprint of the workflow, `identical()`, and none of the 236 results on
tidymodels objects holds a fit. `main` took 1,533 s and the branch 1,452 s, on
a shared machine; the verdict on every call is that of the run of the third
review. The first run of
the increment, before its review, gave 42,980 calls, 42,371 identical, 609
allowed and 0 unexpected, the run of its first review 43,136, 42,398 and 738,
that of its second 43,196, 42,366 and 830, and that of its third, with the
pruning that decision 36 removed, the same counts as these; their counts are
replaced by these.

Increment 11 adds to the corpus the `MASS::rlm()` fit of the class block in
the other forms of data (`class_forms()`: without `data`, shuffled under its
own row names, as a shuffled tibble and as its model frame), each of its five
cells carrying the defect R6, through every recipe, route and runner. These
calls are judged by a rule of their own, before any other
(`identity_increment11_verdict()`): a call may stay `identical()` to `main`, or
change, from a value or from another refusal, to the refusal of the context
only, its message and its class `heteroTests_model_context_error`. And, as
`main` holds increments 1 to 10, every other call must be `identical()` to
`main`, with `main`'s warnings, which the earlier runs counted and found equal,
and the state of the generator it left: a change an earlier rule would allow
is unexpected there.

Increment 11, its first part, against `main` at the merge of increment 10
(565e6ae), each side installed privately and its workers loading it through
`R_LIBS`: 43,630 calls on the 410 cells of the matrix, the 5 cells of
Harrison-McCabe's rank, the 8 cells of the follow-up of N2, the 4 cells of the
coding of a factor, the 8 grouped cells, the 7 cells of the block `item3/` and
the 105 cells of the tidy block, and the examples of forty-one help pages, of
which 43,144 identical, 486 allowed changes and 0 unexpected, every one of
them a call on the five cells of the `rlm` fit and the refusal of the context
with its class: 285 calls that returned a value (261 of the recipes and
routes, 24 of the runners, through 81 functions and routes), and 201 that
refused the fit with another message (99 the guard on the shuffled tibble,
which the context now refuses first; 28 the weights of the variance form
test; 27 "`data` does not reproduce the fit" of `bootstrapTestStatistic()`
and the robust bootstraps; 28 a variable the model frame or `data` lacks, M1
and the message of `data`; 11 the weighted fit of the quantile regression
test; 8 the checks of `data` of the routes and the panel tests). Of the 506
calls on those cells, 20 are `identical()` to `main`: the checks of their
arguments that come before the context (`data` that is not a data frame, a
missing `order_by` or `listw`), and `augment()` and
`recommendRemediationStrategies()` without `data`, which do not build the
context. The 43,124 other calls, every call of the earlier increments among
them, are `identical()` to `main`, with its warnings, and leave the generator
as it left it; so do 441 of the 486 changes, the other 45 being resampling
calls that drew on `main` and are refused before they draw. The 642 `poly()`
calls that `main` computes on the stored design are `identical()` to it.
`main` took 625 s and the branch 581 s, each with the other processes of the
machine.

Its second part, with the review of the first, adds to the corpus the `gam`
with a fixed smooth, `gam(y ~ s(x1, fx = TRUE) + x2)`, in every form of data
(`class/gam_fixed` and `class_forms()`), and the `rlm` fit to the grouped
block, and records each `gam` cell with the kind of its fit
(`identity_gam_kind()`, decided in the harness without the package: the
least-squares refit of its stored design, with its weights and offset, gives
its residuals to `1e-8` of the spread of the response, or not). On a
penalised `gam` (decision 37) a call may change only to the refusal of the
context, its class and its words, or, for the streaming tests and the runner
that streams them, to the value of the exact test of the same call on the
same cell, to `1e-8` on every number. On a `gam` that is the least-squares fit
of its design (decision 38) a call may change only to what the same call
gives on the `lm()` of its basis, to `1e-8` on every number and with the same
draws of the generator: the harness runs every recipe and runner on that `lm`
as reference cells `lmbasis/` (`context_basis_lm()`), on the data frame of the
fit and on its data shuffled, the `lm` fitted in the order of the
observations of the `gam` there, so that a bootstrap draws the same rows of
both (from the smoke run, where the five bootstraps of each of the four
shuffled cells had no reference); or to the value of the cell it reproduces,
itself verified; or to what its twin gives, on a cell of `na.exclude`; or to
the refusal of the guard, on a cell that expects it. Every other call must be
`identical()` to `main`, with its warnings and the state of the generator.

Increment 11, both parts, against `main` at the merge of increment 10
(565e6ae), each side installed privately and its workers loading it through
`R_LIBS`: 44,140 calls on the 415 cells of the matrix, the 5 cells of
Harrison-McCabe's rank, the 8 cells of the follow-up of N2, the 4 cells of
the coding of a factor, the 10 grouped cells, the 7 cells of the block
`item3/` and the 105 cells of the tidy block, and the examples of forty-one
help pages, of which 43,075 identical, 1,065 allowed changes and 0
unexpected. The 41,099 calls outside the cells of increment 11 are all
`identical()` to `main`, with its warnings and the state of the generator it
left. On those cells:

| Class | Cells | Calls | `identical()` | New refusals | Other refusals now the refusal of the context | Values that change | Refusals that become values |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| R6, `MASS::rlm()` | 7 | 510 | 20 | 287 | 203 | 0 | 0 |
| decision 37, a penalised `gam` | 7 | 708 | 391 | 78 | 173 | 66, each the exact test | 0 |
| decision 38, a least-squares `gam` | 18 | 1,823 | 1,565 | 0 | 10, the guard on a shuffled tibble | 48, each the `lm()` of the basis | 200, each the `lm()` of the basis |

The 78 new refusals of decision 37 are those of `fitWLS()` (22),
`performVarianceFormTest()` (20) and its registered name (6),
`performHarrisonMcCabeTest()` (6), `performSzroeterTest()` (6) and its
registered name (6), and the runner (12); its 173 changes of message are those
of the tests that refused a `gam` with smooth terms already (RESET,
Goldfeld-Quandt, the quantile and studentized tests, the White bootstrap),
and of `bootstrapTestStatistic()`, the robust bootstraps and the wild
bootstrap, which refused or stopped on it with messages of their own; its 66
changes of value are those of the three streaming tests (18 each) and of the
runner that streams them (12). The 200 refusals that became values of
decision 38 are those of `bootstrapTestStatistic()` (70), the robust (52) and
wild (32) bootstraps, and the tests that refit the design, their registered
names, routes and runners; its 48 changes of value are those of the
streaming tests on a `gam` with a fixed smooth (12 each) and of the runner
(12). 861 of the 1,065 changes leave the generator as `main` left it; the
others are resampling calls that drew on `main` and are refused before they
draw, or bootstraps that draw where `main` refused. The 642 `poly()` calls
that `main` computes on the stored design are `identical()` to it. `main`
took 647 s and the branch 1,368 s, with other R processes running on the
machine during the second.

## Test matrix and oracles

`tests/testthat/test-model-context.R`, with the cells in
`tests/testthat/helper-model-context.R`. A vector `context_migrated` lists the
functions each increment has moved; the cells are run for those only, so no
expectation of a function that is not migrated is weakened. The data are
`contract_data()` with weights `w = 1 / x1^2`, an offset `o = 0.5 z` outside
the span of the regressors, and a positive response `ypos`.

- **Axes.** The fit: `ols`, `wls` (weights from a column), `wvec` (weights from
  the workspace), a weighted Gaussian `glm`, `fitWLS()`. The offset: none,
  `offset = o`, `+ offset(o)`. The terms: none, `log()`, `poly(x1, 2)`,
  `ns(x1, 3)`, `I(x1^2)`, `I(x1 > median(x1))`, `cut(x1, quantile(x1))`, a
  factor, the factor with `log()` terms or an `offset()` term (increment 4).
  The data: `NULL`, the data frame of the fit, shuffled with its own
  row names, a tibble, a shuffled tibble, a shuffled data.table,
  `dplyr::arrange()`, the model frame. The rows the fit dropped: none,
  `na.omit` with the full data, shuffled or not, the complete rows named and
  renumbered, the tibble, the tibble the fit was made on; `na.exclude`. A
  group missing at observations of the fit (increment 4). The
  identifiers: rows in front, rows at the end, a fit to repeated rows with its
  tibble, forced repeated row names, character row names with a tibble, a
  repeated `(id, time)`. The classes: a multivariate `lm`, a rank-deficient
  fit, `MASS::rlm()` (in every form of data since increment 11, refused),
  `mgcv::gam()` with and without smooth terms, a
  `model = FALSE` fit whose data are gone, and a fit edited after the fit
  (increment 4). The panels (increment 5): a fit that drops every row of a
  unit, by an `lm`, a weighted `lm`, a weighted Gaussian `glm` and a `gam`, in
  fit order, shuffled and as a tibble, against the fit to the complete rows,
  an unbalanced panel fitted to its rows and dropped from the full data (N12),
  and a repeated `(id, time)`, shuffled. Increment 6: a `model = FALSE` fit
  whose data changed after the fit given its data, one whose data are kept,
  with and without `data`, and a variable of a second formula, `z`, missing at
  an observation of the fit and at a row the fit dropped. Increment 9: a fit
  with `na.exclude` (the block `exclude/` and `missing/na_exclude`), each
  against its twin, the same fit with `na.omit` given the same data: a
  regressor missing, given the full data, shuffled, the complete rows named
  and renumbered, a tibble, a shuffled tibble, no `data` and the model frame;
  the response missing; `x1` missing, the variable and the ordering of the
  recipes; data with row names of their own, in fit order and shuffled;
  weights and an offset from the data, with and without `data`; a weighted
  Gaussian `glm`; a `fitWLS()` fit; an `mgcv::gam()` fit, without and with a
  smooth term (from the review of increment 9); a `log()` model, with and
  without `data`;
  a subset; a group missing at an observation of the fit and at a row it
  dropped, given the data, shuffled and renumbered, and a model of the group;
  `id` and `time` missing at rows the fit dropped only; `z` missing at a row
  the fit dropped; and a balanced panel, a fit that drops every row of a unit,
  in fit order and shuffled, and an unbalanced one. Increment 10, in matrices
  of their own (`helper-model-context.R`): the grouped runs of B11, an `lm`, a
  weighted `lm`, the `fitWLS()` fit of the `lm` and the model as a formula on
  a tibble grouped by one column and by two (`context_grouped_fits`,
  `context_groupings`); and the runners of N5 on workflows with a formula,
  with transformed predictors and a factor, with a recipe that transforms a
  predictor, with a transformed outcome (a recipe and a formula preprocessor,
  decision 35), fitted to data with missing values, with variables, with case
  weights, and with recipes that drop rows (`step_naomit()`, `step_filter()`,
  skipped on new data or not), and, from the second review of increment 10,
  with recipes that sort the rows (`step_arrange()` of `x1`, and of `x2` in
  descending order), that skip on new data a step that transformed the
  outcome (`step_log(ypos, skip = TRUE)`) and that drop rows of new data with
  case weights, and, from its third review, with a recipe written in a
  function whose step reads a variable of its frame, and on parsnip fits of
  `fit()` and of
  `fit_xy()`, with the outcome as a data frame and as a vector, and with case
  weights (from the third review), each given no
  data, its data, a tibble, its rows shuffled and its data without the
  outcome, through the runners and `augment()` of their results
  (`context_tidy_models()`, `context_tidy_forms`, `context_tidy_expect()`,
  `context_tidy_augment_expect()`), each value of `augment()` checked row by
  row against the prediction of the engine on that row alone
  (`context_tidy_own()`).
- **Cells.** The fit by offset block without terms, the term block on `ols`
  and `wls`, each crossed with the eight forms of data; the blocks of missing
  rows, of identifiers, of `subset =` and of model classes on `ols`, among them
  an `lm` and a Gaussian `glm` with a zero weight and a `model = FALSE` fit
  whose data changed after the fit (increment 2), and the classes whose answer
  differs between the tests, also without `data` and shuffled, and (increment 3
  and its review) the `gam` without smooth terms with its rows shuffled, a
  `gam` with an offset as an argument and as an `offset()` term, and the
  `model = FALSE` fit whose data are gone given a shuffled tibble; and
  (increment 4) the block of missing groups, the model of the group with
  `log()` and `offset()` terms in every form of data, and the classes in every
  form of data, the data frame, `NULL`, shuffled, a shuffled tibble and the
  model frame (`class_forms()`), an edited fit among them; and (increment 5)
  the block of panels, and the fits to the complete rows of a fit that dropped
  rows with an indicator computed beforehand and of a model of the group; and
  (increment 6) the cells of the stale and kept `model = FALSE` frames and of
  a missing `z`; and (increment 9) the 30 cells of a fit with `na.exclude`
  and the 16 twins they do not share with the other blocks; and (increment
  11) the `MASS::rlm()` fit in the other forms of data (`class_forms()`):
  410 cells, run through each of the forty functions, the
  three streaming tests of increment 7 among them, in one chunk of 10,000
  observations, and the White and wild bootstraps and the robust White and
  Breusch-Pagan tests of increment 8, with `B = 19` for the resampling tests
  (`context_resampling`; a `gam` with smooth terms refused by the White
  bootstrap and the robust Breusch-Pagan test), through the five calls
  of a second formula (`context_second_formula`: Cook-Weisberg with
  `var_formula = ~ z`, the variance form test with `var_formula = ~ z` and
  `against = ~ z`, `fitWLS()` with and without `var_formula = ~ z`), and, in a
  test of their own, through the streaming tests in chunks of 7
  (`context_streaming_recipes()`), where White's streaming test refused the
  collinear squares of a dummy, of `poly()` and of `x1` beside `I(x1^2)`
  until the pull request of N3, and gives on those cells, since, the value of
  the reference of the cell, as on the others (`context_cell_for_streaming()`).
  Where a test departs from a cell by design, its own expectation applies
  (`context_cell_for()`): the quantile regression test refuses a weighted fit;
  a test that reads a variable the model frame does not hold (`context_reads`:
  the `x1` of Goldfeld-Quandt, Glejser, Park, Szroeter and Harrison-McCabe,
  the `group` of Hartley and of the five group tests) gives M1 without
  `data`, and the message of `data` given the model frame, also on a fit with
  a zero weight, and refuses the tibble of a fit to repeated rows, whose rows
  tie; Hartley's test refuses a missing group, which the group tests leave
  out; RESET reads nothing from `data` (decision 24), and on a class of model
  given data the guard refuses gives what it gives on the data frame of the
  fit; the tests that need least squares (`context_refuse_penalised`, and
  the variance form test and `fitWLS()` among the calls of a second formula)
  refuse a penalised `gam` (`context_penalised()`) once `data` is matched,
  and every test takes the `gam` with a fixed smooth of the cell
  `class/gam_fixed` (decisions 37 and 38; until increment 11 the tests of
  increment 2 refused every `gam` with smooth terms, decision 23); and the
  functions of increment 5
  (`context_cell_for_panel()`) read `id` and `time`, which no model of the
  matrix uses, or the `x1` and `x2` of the scatter diagnostic: without `data`
  the panel tests give M1 and the scatter diagnostic and `runPanelTests()` the
  message on `data`, the model frame lacks `id` and `time`, Pesaran's test
  refuses a repeated pair (R2), and since the pull request of N12 tests an
  unbalanced panel, as that of a fit that dropped some rows of some units,
  against the same reference as the random-effects test, where increment 5
  refused it (N12), the scatter diagnostic refuses a zero
  weight before the guard, and the
  fits that dropped rows are compared with their fit to the complete rows;
  the tests computed from the model alone and RESET
  (`context_cell_for_model_alone()`) read nothing from `data`, which they
  match as every test does (decision 31), and keep their own refusals: RESET
  takes a zero weight, and the variance form test refuses weights that did not
  come from `fitWLS()`; and the calls of a second formula
  (`context_cell_for_formula()`) read `z` at the confirmed rows, so that the
  guard refuses what it refuses, `z` is refused without `data` (M1) and with
  the model frame as `data`, after a zero weight (Cook-Weisberg) or weights
  (the variance form test), and `fitWLS()`, which is not a test, takes a zero
  weight and an edited fit.
  Fligner-Killeen's test ranks the absolute
  deviations from the group medians, and the two residuals in the middle of a
  group of even size tie at the median exactly on one fit of a model and not,
  by rounding, on another (the `lm`, the `gam`, the Gaussian `glm`; N15): its
  statistic moves by up to 2.6e-6 and its p-value by up to 1.1e-5 (measured on Windows; between a Gaussian `glm` with an offset and the `glm` of `y - o`, equal on Windows to `1e-10`, 1.5e-7 of the statistic on Linux and 1.9e-6 on macOS, in the CI run before the tolerance applied there), so against
  another fit of the same model it is compared to `1e-4`
  (`context_fligner_tolerance`). The full
  matrix runs under `skip_on_cran()`, a 15-cell subset on CRAN.
  Tibbles and data.tables come from `dplyr` and `data.table` under
  `skip_if_not_installed()`.
- **Oracles** (relations between calls, not stored values):

| Rule | Expectation |
| --- | --- |
| invariance over the accepted forms of data | `identical()` to the data frame of the fit |
| `NULL` and the model frame | `identical()` to the call with data |
| refused order | the guard's error, class `heteroTests_data_rows_error` |
| offset equivalence | equal to the fit of `y - o` without an offset, to `1e-10` (Fligner-Killeen to `1e-4`, N15); RESET: the nested F test of the model with its offset, from `anova()` of the two `lm()` fits with the offset, to `1e-10` (decision 19) |
| Goldfeld-Quandt | against `lmtest::gqtest()`, which fits the segments to the rows of the design, on `ns()`, `bs()`, `I(x > median(x))` and `poly()` terms, and on the fit of `y - o` for an offset, to `1e-10` |
| Harrison-McCabe's rank | against the test written out from the fit's own QR decomposition, with its rank, on a Gaussian `glm`, weighted or not, and `lm(tol = 1e-11)` with a nearly collinear column, to `1e-6` (they agree to 1e-8, the conditioning of a column at 1e-9), and on well-conditioned fits to `1e-12` |
| the registered Szroeter test | its fitted values `identical()` to `predict(type = "response")` on every class, and equal to `fitted()` to `1e-12`; on a `gam` with an `offset` argument, which `predict.gam()` leaves out, `identical()` to `fitted()` and to the same `gam` with an `offset()` term; the value of `performSzroeterTest()` ordered by them |
| ties | for each test of increments 3 and 4 that reads a column beyond the model frame, the reviewer's case of increment 1 (numbered data in fit order, and the same data exchanged within ties, as data frames and tibbles): the guard's refusal; a column of the frame and the spatial test: the value of the named data |
| the group tests | against the test computed without the package, on the residuals of the fit (`weighted.residuals()` for a weighted fit) and the groups of the data it was fitted to, in its order: `car::leveneTest()` centred at the means (Levene) and at the medians (Brown-Forsythe), `stats::fligner.test()`, `stats::bartlett.test()` and O'Brien's transformation written out with `anova()` of `lm()` (`context_group_oracle()`), to `1e-10`, on a `gam` with and without smooth terms or an `offset` argument given its data shuffled, the character-named fit given its tibble, the complete rows renumbered, a model of the group without `data` and with its model frame, a weighted fit, and missing groups, left out |
| missing groups | the five group tests on a group missing at observations of the fit: the warning "Removed N observations due to missing values in group" and the value of the data frame of the fit on every accepted form of data; a group missing at a row the fit dropped is not counted |
| the panel tests and the scatter diagnostic | against the statistic written out without the package on the residuals of the fit and the rows of `data` that carry their names (`context_panel_oracle()`), to `1e-10`: Pesaran's CD from the correlations of each pair of units over the periods, the LM statistic of Breusch and Pagan (1980), equation 5, with the mean number of periods, and Spearman's correlation of the absolute residuals; on data shuffled with their row names and on a fit that drops every row of a unit; the 0.15.0 example in fit order and shuffled, and with rows after those of the fit, `identical()`; a repeated pair of `id` and `time` refused by Pesaran's test and not by the random-effects test; an unbalanced panel, where Pesaran's test recycled the residuals of a unit or stopped, increment 5 refused it, and since the pull request of N12 it gives, directly and through `runPanelTests()`, the CD written out from the periods each pair of units shares, and `plm::pcdtest()`'s value, -0.373, to `1e-10`; ties in numbered data in fit order refused for `id` and `time` and for a column of the scatter diagnostic beyond the model frame |
| the tests computed from the model alone (increment 6) | numbered data in another order, rows in front of those of the fit and repeated row names refused by the guard, named data in any order and the complete rows of a fit that dropped rows, numbered again, `identical()` to the data frame of the fit (decision 31); a stale `model = FALSE` frame refused by the nine, RESET, `fitWLS()`, the registered names and `runTimeSeriesTests()`, with and without `data`, and the values of before once the frame is restored; a fit whose data are gone tested on its data, in fit order and shuffled under their row names, `identical()` to the fit with its frame |
| a second formula (increment 6) | `var_formula = ~ z` and `against = ~ z` on named data shuffled `identical()` to the data frame; numbered data in another order, rows in front and the tibble of a fit to repeated rows refused by the guard; the complete rows numbered again `identical()` to the fit to the complete rows; a variable of the workspace refused (M1, the message of `data`), and given as a column of `data` the value it gave from the workspace; a variable of the model read from the model frame and the functions of the formula from its environment |
| term equivalence | `log()`, `I()`, `ns()`, `cut()` against the same columns computed beforehand, `1e-10`; `poly()`, `1e-8`; Fligner-Killeen, `1e-4` (N15) |
| the streaming tests | in chunks of 7, 40 and 10,000, a term computed from its rows (`I(x1 > median(x1))`, `cut(x1, quantile(x1))`, and the median of a fit that dropped rows) against the same column computed beforehand, to `1e-10`, and against the exact test, to `1e-8`; in chunks of 7, 25, 119, 120 and 10,000, the exact test on unweighted, weighted, Gaussian `glm`, offset, `log()` and rank-deficient fits, to `1e-8` (`test-streaming.R`); on a `gam` with smooth terms, the Breusch-Pagan and Koenker statistics written out on the variables of its formula, to `1e-8`; the refusals of the context final in the runner that streams them; since the pull request of N3, White's streaming test on every cell of collinear terms (a factor, `poly()`, `I(x1^2)`, the median, `cut()`) against `performWhiteTest()`, to `1e-8`, with its degrees of freedom, and the three on regressors 100 to 10,000 standard deviations from zero and on squared residuals of any scale against the exact tests |
| weights equivalence | the Gaussian `glm` against the weighted `lm`, `1e-8` (Fligner-Killeen, `1e-4`, N15); workspace weights against a column, and `fitWLS()` against `lm(weights = weights(fit))`, `identical()`, but for the White and wild bootstraps, which estimate the weights of a `fitWLS()` fit again in each replicate |
| the resampling tests (increment 8) | each replicate of the White bootstrap the formula refitted to the fitted values plus the residuals drawn, and White's test of that refit, `identical()`, with the generator left as the draws leave it; without `data` and with the model frame, on weights, an offset argument, an `offset()` term, `I(x1 > median(x1))` and `ns()`, the four resampling routes and `bootstrapTestStatistic()` `identical()` to the call with `data` (B12); on `cut(x1, quantile(x1))` and on the median of a fit that dropped rows, the White bootstrap, `bootstrapTestStatistic()` with null and pairs resampling and the robust White bootstrap against the fit with the column computed beforehand, to `1e-10`; `bootstrapTestStatistic()` of Koenker's, White's and the Breusch-Pagan statistics and the robust White bootstrap on a Gaussian `glm` against the weighted `lm`, to `1e-8`, with the same p-value (N8); the White bootstrap on a `gam` given its rows shuffled `identical()` to the rows in order (N17); an edited fit and a frame that cannot be rebuilt refused |
| missing equivalence | `na.omit` with any accepted form against the fit to the complete rows, `identical()`; `I(x1 > median(x1))` against the term computed beforehand |
| `na.exclude` (increment 9) | every function of the matrix and every call of a second formula on a fit with `na.exclude` against the same call on its twin, the same fit with `na.omit` given the same data (`context_same_as_twin()`): the same refusal, class and message, or the same value, `identical()`, the weights `fitWLS()` returns padded as `naresid()` pads them (`context_twin_value()`); and, in tests of their own, the same warnings and draws of the generator, for the resampling tests and the runners too, Pesaran's CD that of `plm::pcdtest()` on the rows of the fit, and the estimated variances of `fitWLS()` padded |
| identifiers | rows at the end, the tibble of a fit to repeated rows, character row names: `identical()` to their reference; rows in front and repeated row names: the guard's error |
| grouped runs (increment 10, B11) | a `fitWLS()` fit refused with the class of the context, on one and two grouping columns; an `lm`, a weighted `lm` and a formula: each group the test of `lm()` fitted to the rows of the group, with its weights, to `1e-10`; the `fitWLS()` fit given the data ungrouped and through `runDiagnostics()`, which ungroups them, `identical()` to the fit tested as a whole |
| workflows and parsnip fits (increment 10, N5) | given the data the fit was made from, as a data frame or a tibble, `identical()` to the call without `data`, through `runHeteroTests()` and `runDiagnostics()`, a transformed outcome among them (decision 35), and `augment()` of the result binding each row the value of its observation, checked row by row against the prediction of the engine on that row alone, mapped by the preprocessing (`context_tidy_own()`, from the second review of increment 10); a workflow given its rows shuffled, a subset of them, or the data of a step that dropped rows at the fit only that cannot be found again, refused with the class of the guard; a recipe that sorts the rows (`step_arrange()`, both directions) testing the model frame, its data refused by `augment()`; a step skipped on new data that transformed the outcome named in the refusal; other case weights refused, on the rows a recipe that drops rows of new data keeps too; the results keeping what `augment()` needs of the workflow, its blueprint as the workflow keeps it and no fit (decision 36), a few KB for a recipe written at the top level, two calls `identical()`; the variables a step reads of the function it was written in read as the step reads them (the cases of the fourth review); a step that makes a row from other rows (`cumsum()`, `row_number()`) testing the model frame, its data refused by `augment()`; the data of a `fit_xy()` fit with case weights refused; only the skipped steps that matter named; data that cannot be mapped refused with a message that says why |
| the rows of `data` outside the tests (increment 10) | `recommendRemediationStrategies()` on the full data of a fit that dropped rows, shuffled, renumbered and as a tibble, with `na.omit` and with `na.exclude`, `identical()` to the call on the rows of the fit in its order, and `generateHeteroRecommendations()` its tests and advice so, its profile that of `data` as given; `augment()` binding the rows of `data` in their order, each with the fitted value and residual of its observation (`fitted()`, `residuals()`, `predict()` row by row), the rows the fit used on a fit with `na.omit`, every row with `NA` at the rows it dropped on a fit with `na.exclude`, as `broom::augment()` binds them, to `1e-12` (decision 34); numbered data in another order refused by the guard; data that lack the variables of the model and a model the context refuses by kind refused, naming the function; grouped data matched with their groups, and the advice on missing data of a profile with more than 5% of values missing |
| classes | a multivariate `lm`: M7; a binomial `glm` and a `survey::svyglm()` fit: today's messages; a `MASS::rlm()` fit (increment 11): the refusal of R6, class `heteroTests_model_context_error`, by every function of the matrix and every call of a second formula on every form of data, and, in a test of its own, by every test, its robust, resampling and streaming forms, `bootstrapTestStatistic()`, `fitWLS()`, the registered names, `test_factory`, `cachedTest()`, every runner, a grouped run, `autoCompareRemediations()`, the recommendations and `augment()` given `data`, the runners keeping it final, the suggested `lm()` call checked with weights, a subset, `model = FALSE`, a tibble and the matrix interface; a penalised `mgcv::gam()` fit (decision 37): the refusal of the tests that need least squares, and the values of the exact tests from the streaming tests and the runner with and without chunking; a `gam` with a fixed smooth and one without smooth terms (decision 38): every test, the bootstraps included, against the same call on the `lm()` of its basis (`context_basis_lm()`), to `1e-8`, and the check of `rgam_least_squares()` against the smoothing parameter |

The unit tests of the context check its fields and their lengths on every
specification, that `rcontext_pearson()` is the unpadded
`rpearson_residuals()`, the alignment of each form of data, the variables read
at the confirmed rows and M1 without `data`, the tolerance of the rank (`1e-7`
for an `lm`, `1e-11` for a `glm`), and that the validator refuses a fit whose
coefficients, residuals or weights were changed and accepts an ill-conditioned
fit and responses scaled by `1e-200` to `1e200`.

## The defects

Measured on `main` at the start of the work (after #136), with the release
that fixes each.

| # | Defect | Status |
| --- | --- | --- |
| B1, B2 | Numbered data in another order, or with rows in front | Refused since 0.15.0; stays refused (decision 3) |
| B3, B4 | The panel tests and the scatter diagnostic read `data` by position | Refused since 0.15.0; fixed in increment 5: they match the rows by name, by number or in fit order |
| B5 | A term computed from all the rows, re-evaluated on the rows of the fit | Fixed in increment 1 for the five, in increment 2 for its four, in increment 7 for the streaming tests, in increment 8 for the replicates of the resampling tests and of `bootstrapTestStatistic()`, pairs resampling included, which computed the terms again on the rows it drew; the others follow their increments |
| B6 | Streaming evaluates terms per chunk | Fixed in increment 7: the chunks are rows of the stored design |
| B7 | Offsets left out of a refit | Refused by #134; fixed in increment 2 |
| B8, B10, B17 | Stale `fitWLS()` weights; a zero prior weight of a `glm`; White streaming on `aov` | Fixed in 0.15.0 and 0.16.0 |
| B9 | A second formula matched by position | Fixed in increment 6: `var_formula` and `against` are evaluated on the rows the alignment confirmed (`rcontext_formula_rows()`); `rvariance_frame()` is gone |
| B11 | A grouped run of a `fitWLS()` fit refits without weights | Fixed in increment 10 (decision 10): refused, with the class of the context, before any group is refitted (`rrefuse_grouped_fitwls()`); a grouped run of a weighted `lm`, whose weights the refit of each group reads from its rows, is unchanged |
| B12 | Weights or an offset from a column, `data` omitted | Fixed in increment 1 for the five and the runner, in increment 2 for its four, in increment 8 for the replicates and the White bootstrap; the group tests of increment 4 and the streaming tests of increment 7 gave values already |
| B13 | `na.exclude` | Fixed in increment 9: the checks of the model and every test read the residuals, fitted values, weights and leverages of the fit without the padding, and a fit with `na.exclude` gives what the same fit with `na.omit` gives, an output with one value per row padded as `naresid()` pads it (`fitWLS()`'s `"variance_model"`); the panel tests and the scatter diagnostic no longer read it by position |
| B14 | Numbered data holding the rows the fit dropped, or only those it kept | Fixed in increment 1 for the five, in increment 2 for its four, in increment 3 for its six, in increment 4 for the five group tests (the complete rows renumbered, the tibble of the character-named fit), in increment 5 for the panel tests and the scatter diagnostic, in increment 7 for the streaming tests, in increment 8 for the resampling tests and the robust tests |
| B15 | `subset =` by index | Fixed in increment 1 for the five, in increment 2 for its four; the six of increment 3, the five of increment 4 and the streaming tests of increment 7 gave values already; in increment 8 for the White bootstrap, which rebuilt a design of the rows of `data` from the call of the subset fit |
| B16 | The panel tests on a fit that dropped rows | Fixed in increment 5 for the random-effects test and the scatter diagnostic; Pesaran's test reaches the panel of the fit, which is unbalanced unless every row of a unit was dropped, and refused it (N12, decision 28) until the pull request of N12, since which it gives the value of the fit to the complete rows |
| R1 | Data whose values are not the fit's | Refused since 0.15.0 |
| R5 | The quantile test with an offset | Refused by #134; fixed in increment 2 |
| R2 | Repeated `(id, time)` | Refused by Pesaran's test since increment 5 (decision 13); the random-effects test reads no period |
| R3 | `var_formula` variables from the workspace | Refused since increment 6 (decision 9), with or without `data`, and the variables of `against` too |
| R4 | A multivariate `lm` | M7 in increment 1 for the five, in increment 2 for its four, in increment 3 for its six, in increment 4 for the group tests, in increment 5 for the panel tests and the scatter diagnostic, in increment 7 for the streaming tests, in increment 8 for the resampling and robust tests |
| R6 | `MASS::rlm()` | Fixed in increment 11 (decision 4): refused by the context, with its class, on every route and form of `data` (`rrefuse_rlm()`), where 37 tests tested its M-estimation residuals as least-squares ones; the runners keep the refusal final, and a grouped run refuses it before any group is refitted, where with MASS attached it refitted each group as an `rlm` (from the review of increment 11) |
| gam | A penalised `mgcv::gam()` fit (decision 23): tested on its basis by some tests and on the variables of its formula by the streaming tests; refitted without its penalty by the variance form test and `fitWLS()`; its residuals compared with the null distribution of least-squares ones by Harrison-McCabe and Szroeter; refused in three ways, the context's, `bootstrapTestStatistic()`'s and the `hatvalues()` error of the wild bootstrap, under whose name the runner put White's test; and a fixed smooth refused as a penalised one (the survey of increment 11) | Fixed in increment 11 (decisions 37 and 38): tested on its stored design by the tests that do not need least squares, the streaming tests included; refused with the class of the context by those that do; a `gam` that is the least-squares fit of its design taken by every test |
| R7 | Messages that name variables a test does not need, or rows `data` holds | Fixed in increment 1 for the five, in increment 4 for the group tests |
| L1, L2 | Transformations and `offset()` terms without `data`; the model frame of a weighted or transformed fit as `data` | Fixed in increment 1 for the five, in increment 2 for its four, in increment 3 for its six, in increment 4 for the group tests (a variable the model frame does not hold gives M1), in increment 7 for the streaming tests, in increment 8 for the resampling and robust tests and the replicates of `bootstrapTestStatistic()` |
| N1 | Goldfeld-Quandt refits the segments from the formula (6-10% off with `ns()`); the quantile test refits its quantile regressions from it | Fixed in increment 2 (decision 20) |
| N2 | White's degrees of freedom when the auxiliary columns span the constant | Fixed in 0.16.0 by a separate pull request (decision 22): the rank of the auxiliary design with its intercept, minus one, as `lmtest::bptest()` with the same terms; the harness allows the changes on the cells where main's count was wrong (the `poly()` fits, and the `gam` with a smooth term, whose auxiliary regression lost a column to its intercept and whose statistic changes by 0.28%) and verifies each against White's test written out (`identity_white_by_hand()`). Its follow-up (decisions 25 and 26) decides the rank on the auxiliary design of the centred regressors, refuses an auxiliary regression that fits the squared residuals exactly, saturated or not, and fits it with `lm.fit()`; the harness runs cells of its own for them, checks every number of a changed result, and requires the refusal on the exact fits |
| N3 | White streaming on collinear terms | Fixed in 0.17.0 by a separate pull request (decision 22): the three streaming tests reduce each chunk of the auxiliary design of the centred regressors by a QR decomposition stacked on the factor of the chunks before it, and decide its rank as `performWhiteTest()` does, by `qr()` of that factor at its tolerance, in the order of the columns; White's streaming test drops the collinear terms and gives the degrees of freedom and the value of `performWhiteTest()` (17.83 on 8 on `lm(y ~ x1 + group)`, where it stopped), and the cross-products of a regressor located far from zero no longer look collinear. With it, the threshold on squared residuals that do not vary is relative to their scale, and the columns of the chunks are taken in their order, not matched by name. The harness verifies each change against the exact test of the call (`identity_n3_verdict()`) |
| N4 | Harrison-McCabe's rank is not the fit's | Fixed in increment 3 (decision 21) |
| N5 | `runHeteroTests(<workflow>, data)` | Fixed in increment 10: the data a workflow was fitted to are mapped to the variables of its engine by its preprocessing (`hardhat::forge()`), the outcome of a parsnip fit of `fit_xy()` to `..y`, their rows confirmed by the alignment, and the model frame of the engine's fit tested, the value without `data`, a transformed outcome mapped as the preprocessing makes it (decision 35); data in another order and data that cannot be mapped are refused; with it, the recommendations and `generics::augment()` match the rows of `data` (`rcontext_data_rows()`), `augment()` binding as broom does (decision 34) |
| N6 | RESET reads a stale `model = FALSE` frame | Fixed in increment 2 for RESET, in increment 6 for the other tests computed from the model alone and `fitWLS()` |
| N7 | `cut(x1, quantile(x1))` stops with "new levels" | Fixed in increment 1 for the five, in increment 2 for its four, in increment 7 for the streaming tests, in chunks of any size (B6), in increment 8 for the White bootstrap and for the replicates of `bootstrapTestStatistic()`, which refused the fit ("`data` does not reproduce the fit") |
| N8 | `bootstrapTestStatistic()` refuses every `glm` | Fixed in increment 8: a Gaussian `glm` with the identity link is bootstrapped as the `lm` it is, with its prior weights; another `glm`, a `survey::svyglm()` fit and an `mgcv::gam()` fit are refused, as before |
| N9 | A multivariate `lm`: coercion errors | M7 in increment 1 for the five |
| N10 | Studentized Breusch-Pagan on `rlm` tests the least-squares refit | Fixed in increment 11 (decision 4): the fit is refused, where the statistic gave 9.04, the value of the `lm()` of the same formula, and Koenker's test 8.74 on the residuals of the M-estimate |
| N11 to N15 | Influence diagnostics count aliased coefficients; Pesaran on an unbalanced panel; Szroeter through `test_factory`; Szroeter on a weighted fit; Fligner-Killeen ties | Separate pull requests; N13 unchanged by increment 3; N15 unchanged by increment 4, whose matrix compares Fligner-Killeen's test on another fit of the same model to `1e-4`. N12 refused by increment 5 (decision 28): Pesaran's test filled a matrix of periods by units, and stopped on a unit with fewer periods than the panel, or, when its number of periods divided that of the panel, recycled its residuals and returned a wrong value (0.234 where `plm::pcdtest()` gives -0.373, on `contract_data()` without its first five rows); it refused an unbalanced panel. Fixed by the pull request of N12, which removes the refusal: the statistic of an unbalanced panel is computed from the periods each pair of units shares, as `plm::pcdtest(test = "cd")` computes it (Pesaran 2004, Section 9, equation 31), `CD = sqrt(1 / P) sum_{i<j} sqrt(T_ij) rho_ij`, `rho_ij` the correlation of the residuals of units `i` and `j` over the `T_ij` periods both are observed at, each centred at its mean over them, over the `P` pairs with `T_ij >= 2` (`N (N - 1) / 2` when every pair has); a pair with `T_ij < 2` is left out with a warning, as plm leaves it out, and a panel without such a pair is refused; a correlation that is not defined, a unit constant over the periods of a pair, adds nothing and the pair stays in `P`, with a warning, as on a balanced panel, where plm returns `NA`; -0.373 on that panel, and plm's value to `1e-10` on 252 panels of 5 to 30 units and 4 to 12 periods, 228 of them unbalanced (late entry, early exit, gaps, 1 to 20 rows missing at random, a unit observed once), by an `lm`, a weighted `lm` and a Gaussian `glm`, weighted or not, directly, on the full data of a fit with `na.omit` and through `runPanelTests()`: 5,040 comparisons, the largest difference 3.6e-15 on the statistic, and on the cells of the matrix (below). A balanced panel keeps the matrix of periods by units, every value `identical()`. The refusal stayed on the positional reading of a fit with `na.exclude` until increment 9 (decision 29), which removes that reading and the refusal with it |
| N16 | Data that reproduce the model frame but carry another extra column | A limit no alignment can lift |
| N17 | A `gam`'s unnamed residuals matched to `data` by position: named data in another order than the fit gave a wrong value, without an error, in the tests that read a column of `data` (found in increment 3) | Fixed in increment 3 for Glejser, Park, Szroeter, Harrison-McCabe and Hartley, in increment 4 for the five group tests, in increment 5 for the panel tests and the scatter diagnostic, in increment 7 for the streaming tests, in increment 8 for the White bootstrap, whose replicates added each draw to the fitted value of the row of `data` at the same position |

## Decisions

| # | Decision | Date |
| --- | --- | --- |
| 1 | Last-bit differences of `poly()` terms that come from using the stored design are accepted: the value equals, with `identical()`, what main computes on the fit's stored design (the model refitted with `x = TRUE`); differences against main's rebuilt design are in the last bits (measured up to 1.3e-15 relative on the statistic). | 2026-10-16; wording 2026-10-18 |
| 2 | The response alone is not enough to confirm rows: superseded by the 0.15.0 guard, which compares every column of the model frame. | 2026-10-16 |
| 3 | Numbered data in another order than the fit are refused, not matched by position. | 2026-10-16 |
| 4 | `MASS::rlm()` is refused, in increment 11. Done in its first part: the context refuses it (`rrefuse_rlm()`), with the class of its refusals, before `data` is matched, on every route, a grouped run and `autoCompareRemediations()` among them (from its review). | 2026-10-18 |
| 5 | The quantile regression test refuses an offset until it accounts for it; increment 2 is the fix (done). | 2026-10-17 |
| 6 | A refusal of the context inside `runHeteroTests()` is final: no fallback test runs under its name. | 2026-10-16 |
| 7 | The "Removed N observations" warning is kept, and nothing is dropped. | 2026-10-16 |
| 8 | An extra variable that is `NA` for a fitted row: each test keeps its behaviour, with a message that names the variable and the rows. | 2026-10-18 |
| 9 | A variable of `var_formula` found only in the workspace, with `data = NULL`, is refused. Increment 6 applies it to every second formula (`var_formula`, `against`) and with `data` too: a variable that neither `data`, when it is given, nor, without `data`, the model frame holds is refused (with the message of `data`, or M1), never read from the workspace. A constant, a value of length one such as `k` in `~ I(hp^k)` or `pi`, is not a variable and is evaluated as R evaluates it (from the review of increment 6). | 2026-10-16 |
| 10 | A grouped run of a `fitWLS()` fit is refused, with a final error class. Done in increment 10: the class of the context, `heteroTests_model_context_error`, on every route that reaches a grouped run, before any group is refitted. | 2026-10-18 |
| 11 | The estimated weights of `fitWLS()` are not written into `(weights)` of its model frame, since the guard would then refuse every `fitWLS()` fit of a weighted model given `data`. | 2026-10-18 |
| 12 | The tests build tibbles with `dplyr::as_tibble()` under `skip_if_not_installed("dplyr")`; dplyr and data.table stay in Suggests. | 2026-10-18 |
| 13 | Repeated `(id, time)` pairs are refused, in increment 5. | 2026-10-18 |
| 14 | Refusals of calls that return a value are allowed in 0.16.0, announced in bold. | 2026-10-18 |
| 16 | `na.exclude` goes in 0.16.0, as the last increment (9). Moved to 0.17.0 with increments 8 to 11 (2026-10-26), where it is the second; done in increment 9. | 2026-10-18 |
| 17 | The wording of the release notes and the roadmap: named data in any order, numbered data in fit order. | 2026-10-18 |
| 18 | Numbered data with exactly one row per observation, whose numbers are not the fit's, are taken in fit order once the guard's comparison passes. | 2026-10-18 |
| 19 | RESET with an offset is the nested F test of the offset model; one evidence cell (5000 replications) is added to `offset-refits-size.R` before release. | 2026-10-18 |
| 20 | Goldfeld-Quandt segments come from the stored design. | 2026-10-18 |
| 21 | Harrison-McCabe takes its rank at the fit's tolerance, in increment 3. | 2026-10-18 |
| 22 | N2 and N3 are separate pull requests, outside the context: N2 in 0.16.0, N3 in 0.17.0 (the milestones were split on 2026-10-26). | 2026-10-18 |
| 23 | The tests of increment 2 refuse an `mgcv::gam()` fit with smooth terms, usually penalised, fixed ones included, with a final class, until increment 11 settles which tests take it; the five of increment 1 test it as before. Confirmed by the author on 2026-10-19. Settled by decisions 37 and 38 in increment 11. | 2026-10-18 |
| 24 | RESET builds its context without `data` and keeps `rcheck_data_argument()` until increment 6, so that its values on every form of `data` are those of 0.15.0. Confirmed by the author on 2026-10-19. | 2026-10-18 |
| 25 | After the review of N2, White's test decides its rank on the auxiliary design of the centred regressors, so that it does not depend on where they are located, fitting the columns as they are whenever that fit keeps them all; and it refuses an auxiliary regression with as many independent terms as observations, the runner falling back on White's test without cross products. Decided by the author. | 2026-10-21 |
| 26 | After the review of the follow-up of N2, White's test also refuses an auxiliary regression that fits the squared residuals exactly with fewer terms than observations, as on a paired design: its residual sum of squares at most `sqrt(.Machine$double.eps)` of the total. The refusal advises what the call allows, and the runner takes no fallback when the regressors alone fit the squared residuals exactly, since the Breusch-Pagan and Koenker tests would fit them exactly too. The same refusal in those two tests is left to a pull request of its own. | 2026-10-22 |
| 27 | The streaming tests run only when asked for, outside the context: `runHeteroTests()` and `runDiagnostics()` default to `chunk_threshold_mb = Inf`, a finite value asks for them, and `performWhiteTest()`, `performBPTest()`, `performKoenkerTest()` and the White bootstrap no longer switch to them above 100 MB. Streaming saves memory mainly in White's test, and the White streaming test stops on any model with a factor. The three streaming tests stay exported, not deprecated, and increment 7 still builds them on the stored design. Decided by the author. | 2026-10-22 |
| 28 | Pesaran's test refuses an unbalanced panel, whose units are not all observed at the same periods, in increment 5, with the class of the context and on every route, so that it never returns the value of recycled residuals; a fit that drops every row of a unit keeps a balanced panel. The refusal comes off in the pull request of N12, which computes the statistic on the periods each pair of units shares, as `plm::pcdtest()` does. Decided by the author: refuse now, fix later. Done in the pull request of N12 (2026-10-26): the refusal is off on every route but the positional reading of a fit with `na.exclude`, left to increment 9 (decision 29), which removes it; a pair of units that shares fewer than two periods is left out of the statistic with a warning, as `plm::pcdtest()` leaves it out, rather than refused, and a panel in which no pair shares two periods is refused. | 2026-10-24 |
| 29 | The panel tests and the scatter diagnostic read a fit with `na.exclude` as before, its padded residuals against the rows of `data` by position, until increment 9. Decided by the author. Done in increment 9: they read it as the same fit with `na.omit`. | 2026-10-24 |
| 30 | A repeated pair of `id` and `time` is refused by Pesaran's test only, and so by `runPanelTests()` when it runs that test; the random-effects test reads no period. Decided by the author. | 2026-10-24 |
| 31 | The tests that read nothing from `data`, the nine tests computed from the model alone, RESET and `fitWLS()` without `var_formula`, match `data` through the guard, as every test built on the context does: one rule for every test. Data the guard cannot match are refused, where they returned the value of the fit, and numbered data in the order of the fit that the check of 0.15.0 refused are taken; that check goes, but for the type of `data`. It ends decision 24. Decided by the author, on the open question of the first version of increment 6. | 2026-10-26 |
| 32 | A fit with `na.action = na.exclude` gives what its twin gives, the same fit with `na.omit` on the same data: the same values and refusals, the same warnings, the twin's "Removed N observations" among them (decision 7), and the same draws of the generator; an output with one value per row is the twin's padded as `naresid()` pads it. A group, `id` or `time` that is `NA`, or a pair of `id` and `time` that repeats, only at a row the fit dropped is not read, as for the twin, so that Pesaran's test gives a value there. Decided by the author on the review of increment 9. | 2026-10-28 |
| 33 | The record of the variance function that `fitWLS()` attaches to its fit (`"variance_function"`: `regressors`, `index`, `initial_weights`) stays unpadded on a fit with `na.exclude`, one row per fitted observation, as the bootstraps read it; only the estimated variances, `"variance_model"`, are padded. Decided by the author on the review of increment 9. | 2026-10-28 |
| 34 | `generics::augment()` of the results of the runner binds as broom does. The rows of `data` are matched to the fit by name or number through the alignment, as the tests match them, and bound in the order of `data`: a fit with `na.omit` given its full data binds only the rows it used (117 of 120); a fit with `na.exclude` given its full data binds every row (120), with `NA` at the rows it dropped, so that it is its twin with `na.omit` padded (decision 32); shuffled full data, named, get each row its own fitted value and residual, for both, which ends the binding by position of a fit with `na.exclude`. Decided by the author on the review of increment 10. | 2026-10-28 |
| 35 | A transformed outcome is mapped too. When a workflow's preprocessing transforms the outcome (`step_log(y)` in a recipe, or a formula preprocessor `log(y) ~ x + z`), `..y` is mapped to the outcome `hardhat::forge()` produces, as for the predictors; the rows are still confirmed against every column of the engine's model frame, and the value is `identical()` to that with `data = NULL`. Decided by the author on the review of increment 10. | 2026-10-28 |
| 36 | The `"tidymodels"` attribute of the results of the runners keeps the mold's blueprint exactly as the workflow keeps it, with no rewriting of the user's expressions and no change to their environments; it stays the small record (`kind`, `blueprint`, `fitted_rows`, `case_weights`), not the fitted workflow. A recipe step keeps the environment it was written in, so that a saved result carries whatever that environment holds, as saving the workflow does: 1,523 KB against 1,522 KB on 5,000 rows, and 51,245 KB against 1,894 KB for a recipe written in a function whose frame held a 50 MB data frame, where `saveRDS()` of the workflow takes 50,272 KB; NEWS says so in bold. The pruning of the third review, which rewrote the expressions, goes. Decided by the author on the fourth review of increment 10. | 2026-10-29 |
| 37 | A penalised `mgcv::gam()` fit is tested on its stored design (the basis of its smooths, `model.matrix()`) by every test that reads only its residuals, its fitted values, columns of `data`, groups, panel identifiers, the order of time, or the columns of that design as auxiliary regressors. Every test that refits it by least squares (RESET, Goldfeld-Quandt, the quantile regression test, the studentized Breusch-Pagan statistic of `performStudentizedBPTest()`, of `performBPTestRobust()` by default, of the registered `"student_bp"` and of `rvalidate_against_reference("studentized_bp")`, the White and wild bootstraps, `bootstrapTestStatistic()` and the robust bootstraps, the variance form test and `fitWLS()`), or whose null distribution is derived for least-squares residuals of its design (Harrison-McCabe and Szroeter), refuses it with `heteroTests_model_context_error`, final in the runners. The streaming tests read the basis chunk by chunk and give the values of the exact tests (Breusch-Pagan 23.995 on 10, Koenker 18.313 on 10, White 46.05 on 60), and `runHeteroTests()` gives the same with and without chunking. The wild bootstrap and `bootstrapTestStatistic()` refuse with the class of the context, which ends the fallback of the runner that returned White's test under the name `wild_bootstrap`. `runDiagnostics()` keeps refusing such a fit as a whole, through RESET. Decided by the author on the survey of the first part of increment 11. | 2026-10-29 |
| 38 | A `gam` whose residuals are those of the least-squares fit of its stored design is treated as least squares by every test, the refitting ones included, on its basis: one with fixed smooths (`fx = TRUE`) or none. It is decided by the check of `rleast_squares_reproduces()`, residuals to `1e-8` of the spread of the response (`rgam_least_squares()`), not by the arguments of its smooths; a penalised fit with a smoothing parameter near zero passes (`sp = 1e-10` on the fit of the survey, not `1e-8`). Its refusals become values: the tests of increment 2 and the White bootstrap on a fixed smooth, the bootstraps on every such `gam`. The wild bootstrap reads its leverages from the stored design (`rmodel_leverage()`), not from `hatvalues()`. Decided by the author with decision 37. | 2026-10-29 |

Decision 15 of the first design, B8, was fixed in 0.15.0 by #129 on
2026-10-16, without the context.

## Cost

At 100,000 observations (`y ~ x1 + x2 + g`), building the context took 13 to
30 ms, the guard alone at most 7 ms, and Koenker's test 0.12 to 0.18 s through
the context against 0.20 to 0.31 s before, on a shared machine: the context
removes the rebuilt design and the preparation of the data, and keeps the
comparisons of the guard.

In increment 1 the rows of the five functions in the full sweep, at 400
replications, took 117, 60, 129 and 99 s against 112, 58, 127 and 92 s
before, in four paired runs on a shared machine: 2 to 7% more, against a
spread between runs, from 58 to 129 s, much larger than the difference. With
both versions installed, repeated calls at 150 observations took 1.6 to 2.0 ms
for Koenker's test and 1.8 to 2.1 ms for White's, against 1.8 to 2.4 and 1.8
to 2.7 ms before, and Koenker's test at 100,000 observations without `data`
0.16 to 0.22 s against 0.20 to 0.22 s.
