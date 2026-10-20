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
Increments 4 to 11 are planned; see [Migration plan](#migration-plan).

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
  `variable` of Glejser's and Park's tests or the `group` of Hartley's, such
  data are refused if two rows of the fit tie so (a finding of the review of
  increment 1).
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
  (`rrefuse_non_gaussian_glm()`), a multivariate `lm`, a model frame that
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
- **An `mgcv::gam()` fit with smooth terms**, usually penalised, is refused by the tests of
  increment 2 (`rrefuse_smooth_gam()`, class
  `heteroTests_model_context_error`): it is not the least-squares fit of its
  design, which they fit again. The five of increment 1 and the six of
  increment 3 test it as before 0.16.0, and increment 11 settles it for every
  test (decision 23). The residuals of a `gam` carry no names; its
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
  incomplete rows is kept, and nothing is dropped (decision 7).
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
| `kind` | `"lm"`, `"glm_gaussian"`, `"gam"`, `"fitwls"`, `"rlm"` | `rcontext_kind()` |
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
| `rcontext_kind(model, fn)` | `rrefuse_non_gaussian_glm()`, then refuses a multivariate `lm` (M7); returns the kind. |
| `rcontext_align(ctx, data, fn, test_label, beyond)` | The alignment step below; `beyond` says whether the test reads columns of `data` beyond the model frame. |
| `rcontext_variables(ctx, data, need)` | Reads the variables a test names: from the rows the alignment confirmed, or without `data` from the model frame, when it holds them under their own names (M1 otherwise). |
| `rvalidate_model_context(ctx)` | One value per observation everywhere; distinct rows; and, but for `MASS::rlm()`, that the response is the design times the coefficients plus the offset plus the residuals, and, but for a penalised `mgcv::gam()` fit, that the residuals are orthogonal to the design in the metric of the weights, each relative to the size of its terms (M8). The orthogonality allows $10^{-6} \lVert X_j \rVert_W \lVert e \rVert_W + \epsilon\, n\, \lVert X_j \rVert_W \lVert y \rVert_W$: a Gaussian `glm` computes its residuals as $y - \hat\mu$, which shifts them by about $\epsilon \lvert \bar y \rvert$, and the first term alone refused valid fits of a response near `1e11`. In a sweep of `lm` and Gaussian `glm` fits with 120 to 10,000 observations and responses up to `1e14`, the largest score of a valid fit was 0.04 of the second term. An edited coefficient leaves the residuals unchanged, so only the first check sees it, at `1e-7` of the largest term: an edit of 0.5 goes undetected once the response is above about `1e7`. M8 is a guard against accidental edits, not a proof that a fit is unedited. |
| `rcontext_frame_from_data(model, data)` | The model frame of a `model = FALSE` fit whose data are gone, from `data`, verified as `rcheck_model_frame()` verifies a rebuilt frame. |
| `rstop_model_context(message)` | The refusals of the context, with class `heteroTests_model_context_error`. |
| `rcontext_pearson(ctx)` | The Pearson residuals the tests read (`rpearson_residuals()`), unpadded; refuses a zero weight as before. |
| `rcontext_design(ctx)` | The design without its aliased columns. |
| `rcontext_rows(ctx, data)` | The confirmed rows of `data`, in fit order and under the names of the fit, or the model frame without `data`: what a test hands to its checks of the data and to a test it calls. |
| `rcontext_unpad(ctx, x)` | Removes the padding of `na.exclude`. |
| `rwarn_incomplete_rows(data, model, also)` | The "Removed N observations" warning, dropping nothing; `also` names the other variables the test reads from `data`, such as the `order_by` of Goldfeld-Quandt. |
| `rrefuse_smooth_gam(ctx, fn)` | Refuses an `mgcv::gam()` fit with smooth terms, fixed ones included, in the tests of increment 2 (decision 23). |
| `rcontext_mean(ctx)` | The fitted values, the linear predictor with the total offset, computed from the coefficients as `predict.lm()` computes it, in fit order: what `fitted()` holds (an `lm` up to rounding, as `y - e`), and so what `predict(type = "response")` returns, but on a `gam` with an `offset` argument, which `predict.gam()` leaves out. The registered `"szroeter"` orders by it (increment 3). |
| `rstop_missing_variable(ctx, values, variable, argument, test_label)` | Refuses an extra variable that is `NA` at an observation of the fit, with the "Missing rows" message of before, naming the variable (decision 8). |

The guard of 0.15.0 is split in two, so that the alignment can run its
comparison on rows found another way: `rdata_fit_rows()` picks the rows by
name or by number, and `rcheck_rows_at()` compares them with the model frame.
`rcheck_data_rows()` keeps its signature and runs the two for the tests that
are not migrated yet; its behaviour is unchanged.

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
| M1 | an extra variable the model frame does not hold, without `data` | "The following variables are missing from the model frame of `model`: x1. It holds the terms of the model (log(ypos), log(x1), x2), not the variables they are computed from; pass the data the model was fitted to as `data`." |
| M2, M4 | rows of the fit absent; another number of rows | `rstop_data_rows()`, today's text ("requires `data` to contain the rows used to fit the model. Missing rows: ...") |
| M7 | a multivariate `lm` | "performKoenkerTest() needs a model with one response; `model` has 2." |
| M8 | the fit does not reproduce itself | "The fitted model does not reproduce its residuals from its design and coefficients: it was changed after the fit. Refit the model." / "The weights of the model are not those its coefficients were fitted with: ..." A guard against accidental edits, not a proof that a fit is unedited: an edited coefficient, which leaves the residuals unchanged, is seen only at `1e-7` of the largest term (an edit of 0.5 goes undetected once the response is above about `1e7`). |
| frame | a model frame that cannot be rebuilt, and `data` absent or not giving the response and the fitted values of the fit | "... cannot use the model frame of `model`: it cannot be rebuilt (...), and `data` does not give the response and the fitted values of the fit. ..." |

M1, M7, M8 and the frame message have class `heteroTests_model_context_error`.

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
  the fit"), as it was refused before with "factor has new levels". Above the
  100 MB switch to the streaming tests, the five still rebuild the design chunk
  by chunk until increment 7.
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
  `na.action = na.exclude` is still refused, as before, until increment 9. A
  Gaussian `mgcv::gam()` fit, a `glm` by class, is tested as before; without
  smooth terms it gives the value of the `lm`. Whether a penalised `gam` with
  smooth terms is an input the tests justify is a question of the contract,
  left to increment 11 with `MASS::rlm()`.

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
  on every form of `data` is that of 0.15.0.
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
  orders by the fitted values with it, 0.800386 (p 0.2117) becoming 0.962844
  (p 0.1678) on the fit of `mtcars` in NEWS, the value of the same `gam` with
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
  wrong one for the five that read a column: Szroeter -0.537 for 3.185 in
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
- **Still matched by position** (N17): the group tests (increment 4), Pesaran
  and the random-effects test (5), the three streaming tests (7) and the
  replicates of the White bootstrap (8) on a `gam` given data in another
  order; each is closed by its increment.

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
| 4 | The group tests (Levene, Brown-Forsythe, Fligner-Killeen, Bartlett, O'Brien); their copies of the alignment go | the character-named fit with its tibble; R7 | Planned |
| 5 | `performPesaranTest()`, `performBPRandomEffectsTest()`, `performScatterDiagnostic()`, `runPanelTests()`: `id`, `time` and `variable` read at the confirmed rows | B3, B4, B16, R2 (decision 13) | Planned |
| 6 | The tests computed from the model alone (the context applies `rcheck_model_frame()` to them), and the check of `data` RESET keeps until then; Cook-Weisberg with `var_formula`; `performVarianceFormTest()`; `fitWLS(data, var_formula)` | B9, R3, N6 | Planned |
| 7 | The three streaming tests, chunk by chunk from the stored design | B6; B12, B14, L1 for these | Planned |
| 8 | The resampling tests and `bootstrapTestStatistic()`, replicates from the stored design: `performWhiteTestBootstrap()`, whose observed statistic and replicates move together, and `rwhite_test_rebuilt_design()`, which keeps them on the rebuilt design until then, goes; `performWildBootstrapTest()`; `bootstrapTestStatistic()` and `performWhiteTestRobust(bootstrap = TRUE)`, which refuse a fit whose frame cannot be rebuilt from `data` until then | B12 for replicates, N8 | Planned |
| 9 | `na.exclude`: unpadded residuals in the checks, padded outputs per observation | B13 | Planned |
| 10 | The runners and the clean-up: grouped `fitWLS()` refused (decision 10), workflows with `data` (N5); `prepare_model_data_for_test()`, `rdata_or_model_frame()` and the copies of the old preparation go | B11, N5 | Planned |
| 11 | `MASS::rlm()` refused in `rcontext_kind()` (decision 4); and the same question for a penalised `mgcv::gam()` with smooth terms, settled for every test together: accepted as before by the five of increment 1 until then, refused by the four of increment 2 (decision 23) | R6, N10 | Planned |

Outside the context, as separate pull requests: White's degrees of freedom
when the auxiliary columns span the constant (N2) and White streaming on
collinear auxiliary terms (N3) (decision 22), and N11 to N15.

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
  factor. The data: `NULL`, the data frame of the fit, shuffled with its own
  row names, a tibble, a shuffled tibble, a shuffled data.table,
  `dplyr::arrange()`, the model frame. The rows the fit dropped: none,
  `na.omit` with the full data, shuffled or not, the complete rows named and
  renumbered, the tibble, the tibble the fit was made on; `na.exclude`. The
  identifiers: rows in front, rows at the end, a fit to repeated rows with its
  tibble, forced repeated row names, character row names with a tibble, a
  repeated `(id, time)`. The classes: a multivariate `lm`, a rank-deficient
  fit, `MASS::rlm()`, `mgcv::gam()` with and without smooth terms, and a
  `model = FALSE` fit whose data are gone.
- **Cells.** The fit by offset block without terms, the term block on `ols`
  and `wls`, each crossed with the eight forms of data; the blocks of missing
  rows, of identifiers, of `subset =` and of model classes on `ols`, among them
  an `lm` and a Gaussian `glm` with a zero weight and a `model = FALSE` fit
  whose data changed after the fit (increment 2), and the classes whose answer
  differs between the tests, also without `data` and shuffled, and (increment 3
  and its review) the `gam` without smooth terms with its rows shuffled, a
  `gam` with an offset as an argument and as an `offset()` term, and the
  `model = FALSE` fit whose data are gone given a shuffled tibble: 279 cells, run through
  each of the fifteen functions, with `B = 19` for the resampling test. Where a
  test departs from a cell by design, its own expectation applies
  (`context_cell_for()`): the quantile regression test refuses a weighted fit;
  a test that reads a variable the model frame does not hold (`context_reads`:
  the `x1` of Goldfeld-Quandt, Glejser, Park, Szroeter and Harrison-McCabe,
  the `group` of Hartley) gives M1 without `data`, and the message of `data`
  given the model frame, and refuses the tibble of a fit to repeated rows,
  whose rows tie; RESET reads nothing from `data` (decision 24); and the tests
  of increment 2 refuse a `gam` with smooth terms (decision 23). The full matrix runs under `skip_on_cran()`, a 12-cell subset on CRAN.
  Tibbles and data.tables come from `dplyr` and `data.table` under
  `skip_if_not_installed()`.
- **Oracles** (relations between calls, not stored values):

| Rule | Expectation |
| --- | --- |
| invariance over the accepted forms of data | `identical()` to the data frame of the fit |
| `NULL` and the model frame | `identical()` to the call with data |
| refused order | the guard's error, class `heteroTests_data_rows_error` |
| offset equivalence | equal to the fit of `y - o` without an offset, to `1e-10`; RESET: the nested F test of the model with its offset, from `anova()` of the two `lm()` fits with the offset, to `1e-10` (decision 19) |
| Goldfeld-Quandt | against `lmtest::gqtest()`, which fits the segments to the rows of the design, on `ns()`, `bs()`, `I(x > median(x))` and `poly()` terms, and on the fit of `y - o` for an offset, to `1e-10` |
| Harrison-McCabe's rank | against the test written out from the fit's own QR decomposition, with its rank, on a Gaussian `glm`, weighted or not, and `lm(tol = 1e-11)` with a nearly collinear column, to `1e-6` (they agree to 1e-8, the conditioning of a column at 1e-9), and on well-conditioned fits to `1e-12` |
| the registered Szroeter test | its fitted values `identical()` to `predict(type = "response")` on every class, and equal to `fitted()` to `1e-12`; on a `gam` with an `offset` argument, which `predict.gam()` leaves out, `identical()` to `fitted()` and to the same `gam` with an `offset()` term; the value of `performSzroeterTest()` ordered by them |
| ties | for each test of increment 3 that reads a column beyond the model frame, the reviewer's case of increment 1 (numbered data in fit order, and the same data exchanged within ties, as data frames and tibbles): the guard's refusal; a column of the frame and the spatial test: the value of the named data |
| term equivalence | `log()`, `I()`, `ns()`, `cut()` against the same columns computed beforehand, `1e-10`; `poly()`, `1e-8` |
| weights equivalence | the Gaussian `glm` against the weighted `lm`, `1e-8`; workspace weights against a column, and `fitWLS()` against `lm(weights = weights(fit))`, `identical()` |
| missing equivalence | `na.omit` with any accepted form against the fit to the complete rows, `identical()`; `I(x1 > median(x1))` against the term computed beforehand |
| identifiers | rows at the end, the tibble of a fit to repeated rows, character row names: `identical()` to their reference; rows in front and repeated row names: the guard's error |
| classes | a multivariate `lm`: M7; a binomial `glm` and a `survey::svyglm()` fit: today's messages |

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
| B3, B4 | The panel tests and the scatter diagnostic read `data` by position | Refused since 0.15.0; values in increment 5 |
| B5 | A term computed from all the rows, re-evaluated on the rows of the fit | Fixed in increment 1 for the five, in increment 2 for its four; the others follow their increments |
| B6 | Streaming evaluates terms per chunk | Increment 7 |
| B7 | Offsets left out of a refit | Refused by #134; fixed in increment 2 |
| B8, B10, B17 | Stale `fitWLS()` weights; a zero prior weight of a `glm`; White streaming on `aov` | Fixed in 0.15.0 and 0.16.0 |
| B9 | A second formula matched by position | Increment 6 |
| B11 | A grouped run of a `fitWLS()` fit refits without weights | Increment 10 (decision 10) |
| B12 | Weights or an offset from a column, `data` omitted | Fixed in increment 1 for the five and the runner, in increment 2 for its four; replicates in increment 8 |
| B13 | `na.exclude` | Increment 9 |
| B14 | Numbered data holding the rows the fit dropped, or only those it kept | Fixed in increment 1 for the five, in increment 2 for its four, in increment 3 for its six |
| B15 | `subset =` by index | Fixed in increment 1 for the five, in increment 2 for its four; the six of increment 3 gave values already |
| B16 | The panel tests on a fit that dropped rows | Increment 5 |
| R1 | Data whose values are not the fit's | Refused since 0.15.0 |
| R5 | The quantile test with an offset | Refused by #134; fixed in increment 2 |
| R2 | Repeated `(id, time)` | Increment 5 (decision 13) |
| R3 | `var_formula` variables from the workspace | Increment 6 |
| R4 | A multivariate `lm` | M7 in increment 1 for the five, in increment 2 for its four |
| R6 | `MASS::rlm()` | Increment 11 (decision 4) |
| R7 | Messages that name variables a test does not need, or rows `data` holds | Fixed in increment 1 for the five |
| L1, L2 | Transformations and `offset()` terms without `data`; the model frame of a weighted or transformed fit as `data` | Fixed in increment 1 for the five, in increment 2 for its four, in increment 3 for its six (a variable the model frame does not hold gives M1) |
| N1 | Goldfeld-Quandt refits the segments from the formula (6-10% off with `ns()`); the quantile test refits its quantile regressions from it | Fixed in increment 2 (decision 20) |
| N2, N3 | White's degrees of freedom when the auxiliary columns span the constant; White streaming on collinear terms | Separate pull requests (decision 22) |
| N4 | Harrison-McCabe's rank is not the fit's | Fixed in increment 3 (decision 21) |
| N5 | `runHeteroTests(<workflow>, data)` | Increment 10 |
| N6 | RESET reads a stale `model = FALSE` frame | Fixed in increment 2 for RESET; increment 6 for the other tests computed from the model alone |
| N7 | `cut(x1, quantile(x1))` stops with "new levels" | Fixed in increment 1 for the five, in increment 2 for its four |
| N8 | `bootstrapTestStatistic()` refuses every `glm` | Increment 8 |
| N9 | A multivariate `lm`: coercion errors | M7 in increment 1 for the five |
| N10 | Studentized Breusch-Pagan on `rlm` tests the least-squares refit | Decision 4, increment 11 |
| N11 to N15 | Influence diagnostics count aliased coefficients; Pesaran on an unbalanced panel; Szroeter through `test_factory`; Szroeter on a weighted fit; Fligner-Killeen ties | Separate pull requests; N13 unchanged by increment 3 |
| N16 | Data that reproduce the model frame but carry another extra column | A limit no alignment can lift |
| N17 | A `gam`'s unnamed residuals matched to `data` by position: named data in another order than the fit gave a wrong value, without an error, in the tests that read a column of `data` (found in increment 3) | Fixed in increment 3 for Glejser, Park, Szroeter, Harrison-McCabe and Hartley; the group tests in increment 4, the panel tests in 5, the streaming tests in 7, the White bootstrap's replicates in 8 |

## Decisions

| # | Decision | Date |
| --- | --- | --- |
| 1 | Last-bit differences of `poly()` terms that come from using the stored design are accepted: the value equals, with `identical()`, what main computes on the fit's stored design (the model refitted with `x = TRUE`); differences against main's rebuilt design are in the last bits (measured up to 1.3e-15 relative on the statistic). | 2026-10-16; wording 2026-10-18 |
| 2 | The response alone is not enough to confirm rows: superseded by the 0.15.0 guard, which compares every column of the model frame. | 2026-10-16 |
| 3 | Numbered data in another order than the fit are refused, not matched by position. | 2026-10-16 |
| 4 | `MASS::rlm()` is refused, in increment 11. | 2026-10-18 |
| 5 | The quantile regression test refuses an offset until it accounts for it; increment 2 is the fix (done). | 2026-10-17 |
| 6 | A refusal of the context inside `runHeteroTests()` is final: no fallback test runs under its name. | 2026-10-16 |
| 7 | The "Removed N observations" warning is kept, and nothing is dropped. | 2026-10-16 |
| 8 | An extra variable that is `NA` for a fitted row: each test keeps its behaviour, with a message that names the variable and the rows. | 2026-10-18 |
| 9 | A variable of `var_formula` found only in the workspace, with `data = NULL`, is refused. | 2026-10-16 |
| 10 | A grouped run of a `fitWLS()` fit is refused, with a final error class. | 2026-10-18 |
| 11 | The estimated weights of `fitWLS()` are not written into `(weights)` of its model frame, since the guard would then refuse every `fitWLS()` fit of a weighted model given `data`. | 2026-10-18 |
| 12 | The tests build tibbles with `dplyr::as_tibble()` under `skip_if_not_installed("dplyr")`; dplyr and data.table stay in Suggests. | 2026-10-18 |
| 13 | Repeated `(id, time)` pairs are refused, in increment 5. | 2026-10-18 |
| 14 | Refusals of calls that return a value are allowed in 0.16.0, announced in bold. | 2026-10-18 |
| 16 | `na.exclude` goes in 0.16.0, as the last increment (9). | 2026-10-18 |
| 17 | The wording of the release notes and the roadmap: named data in any order, numbered data in fit order. | 2026-10-18 |
| 18 | Numbered data with exactly one row per observation, whose numbers are not the fit's, are taken in fit order once the guard's comparison passes. | 2026-10-18 |
| 19 | RESET with an offset is the nested F test of the offset model; one evidence cell (5000 replications) is added to `offset-refits-size.R` before release. | 2026-10-18 |
| 20 | Goldfeld-Quandt segments come from the stored design. | 2026-10-18 |
| 21 | Harrison-McCabe takes its rank at the fit's tolerance, in increment 3. | 2026-10-18 |
| 22 | N2 and N3 are separate pull requests in 0.16.0, outside the context. | 2026-10-18 |
| 23 | The tests of increment 2 refuse an `mgcv::gam()` fit with smooth terms, usually penalised, fixed ones included, with a final class, until increment 11 settles which tests take it; the five of increment 1 test it as before. Confirmed by the author on 2026-10-19. | 2026-10-18 |
| 24 | RESET builds its context without `data` and keeps `rcheck_data_argument()` until increment 6, so that its values on every form of `data` are those of 0.15.0. Confirmed by the author on 2026-10-19. | 2026-10-18 |

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
