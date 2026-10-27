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
`R/performance.R`). Increments 8 to 11 are planned; see
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
  design, which they fit again. The five of increment 1, the six of
  increment 3, the five of increment 4, the three of increment 5 and the nine
  of increment 6 test it as before 0.16.0, and so do the three streaming tests
  of increment 7, on the design of the variables of its formula, which its
  model frame holds, not on its stored design, the basis of its smooths
  (below), and
  increment 11 settles it for every test (decision 23). The residuals of a
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
  its behaviour). The panel tests and the scatter diagnostic, which have no
  check of the residuals that refuses the padding of `na.exclude`, read such a
  fit as before, its padded residuals against the rows of `data` by position,
  until increment 9 (`rcontext_columns_at_fit()`).
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
| `rcontext_group_rows(ctx, data, group)` | The five group tests (increment 4): the group read as `rcontext_variables()` reads it (M1 without `data`), the Pearson residuals (`rcontext_pearson()`, which refuses a zero weight), and the confirmed rows of `data` in fit order, or the model frame, without the observations whose group or variable of the model is missing, left out by `rhandleMissingValues()` with its warning, as before (decision 8). |
| `rcontext_columns_at_fit(ctx, data, columns, fn, test_label)` | The panel tests and the scatter diagnostic (increment 5): the columns they read, `id`, `time` or `vars`, as `rcontext_variables()` reads them (M1 without `data`), and the Pearson residuals (`rcontext_pearson()`); for a fit with `na.exclude`, its padded residuals against the rows of `data` by position, checked by `rcheck_data_in_fit_order()`, as before, until increment 9. |
| `rstop_repeated_panel_cells(id_values, time_values, id, time, fn)` | Pesaran's test (increment 5): refuses a pair of `id` and `time` that holds two observations of the fit, naming up to three pairs, with the class of the context (decision 13). |
| `rstop_unbalanced_panel(id_values, time_values, id, fn)` | Pesaran's test (increment 5): refuses a panel whose units are not all observed at the same periods, naming a unit and its count of periods against the panel's, with the class of the context (N12, decision 28). Since the pull request of N12, on the positional reading of a fit with `na.exclude` only, until increment 9; on every other route the statistic of an unbalanced panel is computed (`rpesaran_cd_unbalanced()`, `R/performPesaranTest.R`). |
| `rcontext_second_formula(model, data, formulas, fn, test_label, check_type)` | The functions that may evaluate a second formula (increment 6): the type of `data` checked first (`rcheck_data_argument()`, whose message the test suite pins; not by `fitWLS()`), then the context built with `data`, the variables of the formulas passed as `reads` (`.` reads every column). The tests computed from the model alone and RESET call `rcheck_data_argument()` and `rmodel_context(model, data, ...)` themselves (decision 31). |
| `rcontext_formula_rows(ctx, data, formula, arg)` | The rows a second formula is evaluated on (increment 6): the confirmed rows of `data` in fit order (`rcontext_rows()`), or the model frame without its terms; a variable of the formula that `data`, or without `data` the model frame, does not hold is refused, with the message of `data` or M1 (R3, decision 9). |
| `rformula_constants(formula, names)` | The names of a second formula that are constants (from the review of increment 6): not columns, and of a value, found from the environment of the formula as `eval()` finds it, that is an atomic vector of length one; `rcontext_formula_rows()` lets them through, and refuses every other name it cannot find in `data` or the model frame. |
| `rstop_missing_from_frame(ctx, absent, also)` | M1, for `rcontext_variables()` and `rcontext_formula_rows()`, which adds that the variables of the formula are not read from the workspace. |

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
| M1 | an extra variable the model frame does not hold, without `data`; a variable of a second formula (increment 6, R3) | "The following variables are missing from the model frame of `model`: x1. It holds the terms of the model (log(ypos), log(x1), x2), not the variables they are computed from; pass the data the model was fitted to as `data`.", and for a second formula "Variables that are not part of the model must be supplied through `data`: those of `var_formula` are not read from the workspace.", whose first words are the message of 0.15.0 the test suite pins |
| M2, M4 | rows of the fit absent; another number of rows | `rstop_data_rows()`, today's text ("requires `data` to contain the rows used to fit the model. Missing rows: ...") |
| M7 | a multivariate `lm` | "performKoenkerTest() needs a model with one response; `model` has 2." |
| M8 | the fit does not reproduce itself | "The fitted model does not reproduce its residuals from its design and coefficients: it was changed after the fit. Refit the model." / "The weights of the model are not those its coefficients were fitted with: ..." A guard against accidental edits, not a proof that a fit is unedited: an edited coefficient, which leaves the residuals unchanged, is seen only at `1e-7` of the largest term (an edit of 0.5 goes undetected once the response is above about `1e7`). |
| frame | a model frame that cannot be rebuilt, and `data` absent or not giving the response and the fitted values of the fit | "... cannot use the model frame of `model`: it cannot be rebuilt (...), and `data` does not give the response and the fitted values of the fit. ..."; since increment 7, for `data` that lack rows named as the observations of the fit, as the tibble of a fit to named data does, "..., and the rows of `data` cannot be matched to the observations of the fit: `data` has no rows named Mazda RX4, Mazda RX4 Wag, Datsun 710. ..." |
| R2 | in Pesaran's test, a pair of `id` and `time` that holds more than one observation (decision 13), on every route, the positional reading of a fit with `na.exclude` included | "performPesaranTest() needs one observation of the fit for each pair of `id` and `time`, and 1 pair holds more than one: id = 1 and time = 5 (2 observations). The residuals are arranged in a matrix of periods by units, with one cell for each pair. Pass a panel with one row per unit and period.", naming up to three pairs, "and N more". On the positional reading of a fit with `na.exclude` the checks run on the rows of `data`, so a refusal can name a row the fit dropped: with `d$time[2] <- 1L` and `d$x2[2] <- NA` this message counts row 2 among the "observations of the fit", and with `d$time[3] <- NA` and `d$x2[3] <- NA` the refusal of a missing period says "Missing rows: 3", a row the fit did not use. Both refusals are right: the matrix would hold those rows, with `NA` residuals |
| N12 | in Pesaran's test, an unbalanced panel, whose units are not all observed at the same periods (decision 28): on every route in increment 5; since the pull request of N12, which computes the statistic of an unbalanced panel, on the positional reading of a fit with `na.exclude` only, until increment 9 | Increment 5: "performPesaranTest() needs a balanced panel, every unit observed at the same periods, and the panel of the fit is unbalanced: 1 of its 12 units is observed at fewer of its 10 periods, id = 1 at 5 of them. The statistic is computed here on a matrix of periods by units, which an unbalanced panel does not fill; plm::pcdtest() computes it for an unbalanced panel from the periods each pair of units shares." Since the pull request of N12: "performPesaranTest() reads a fit with na.action = na.exclude by position, on a matrix of periods by units, and needs a balanced panel, every unit observed at the same periods; the panel of the rows of `data` is unbalanced: 1 of its 12 units is observed at fewer of its 10 periods, id = 1 at 5 of them. Refit the model with na.action = na.omit: the statistic of an unbalanced panel is then computed from the periods each pair of units shares, as plm::pcdtest() computes it." And, on any route, a panel in which no pair of units shares two periods: "performPesaranTest() needs a pair of units observed together at two periods or more, and no pair of the 6 units of the fit shares more than one period. ..." |

M1, M7, M8, the frame message, R2 and N12 have class
`heteroTests_model_context_error`. Both panel refusals find the repeated pairs
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
  (below).
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
  only, decision 28). `runPanelTests()`
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
  increment 9;
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
  increment 9. A second formula, the `var_formula` of Cook-Weisberg's test (and
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
- **Unchanged**: every other value on valid calls; the zero-weight refusals
  and the refusals of the guard; a fit with `na.action = na.exclude`, refused
  as before ("Model residuals must be finite"), until increment 9; and the
  refusal of a regression of squared residuals that do not vary, "requires
  variation in squared residuals", decided against the absolute threshold
  `.Machine$double.eps` on their total sum of squares, which refuses residuals
  of about `1e-5` that the exact tests take (outside the context, an open
  item of ROADMAP.md).

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
| 5 | `performPesaranTest()`, `performBPRandomEffectsTest()`, `performScatterDiagnostic()`, `runPanelTests()`: `id`, `time` and `vars` read at the confirmed rows (`rcontext_columns_at_fit()`); repeated pairs of `id` and `time` refused | B3, B4, B16, R2 (decision 13); N17 for these. Pesaran's test refuses an unbalanced panel, as that of a fit that dropped some rows of some units, until the pull request of N12 (decision 28); `na.exclude` is read as before until 9 (decision 29) | **Done, 0.16.0** |
| 6 | The tests computed from the model alone (the context applies `rcheck_model_frame()` to them), and the check of `data` RESET keeps until then; Cook-Weisberg with `var_formula`; `performVarianceFormTest()`; `fitWLS(data, var_formula)`: `rcontext_second_formula()`, `rcontext_formula_rows()`; `rvariance_frame()` goes. The tests that read nothing from `data`, RESET included, match it through the guard, as every test does (decision 31): the check of 0.15.0 goes, but for its check of the type of `data` | B9, R3, N6; M7, M8 and "frame" for these; decision 31 | **Done, 0.16.0** |
| 7 | The three streaming tests, chunk by chunk from the stored design (`rstreaming_design()`) | B6; B12, B14, L1 for these, with B5, L2, N7, N17 and R4 (B12 and B15 gave values already); a smooth `gam` kept on the design of its variables, as before (decision 23) | **Done, 0.16.0** |
| 8 | The resampling tests and `bootstrapTestStatistic()`, replicates from the stored design: `performWhiteTestBootstrap()`, whose observed statistic and replicates move together, and `rwhite_test_rebuilt_design()`, which keeps them on the rebuilt design until then, goes; `performWildBootstrapTest()`; `bootstrapTestStatistic()` and `performWhiteTestRobust(bootstrap = TRUE)`, which refuse a fit whose frame cannot be rebuilt from `data` until then | B12 for replicates, N8 | Planned, 0.17.0 |
| 9 | `na.exclude`: unpadded residuals in the checks, padded outputs per observation | B13 | Planned, 0.17.0 |
| 10 | The runners and the clean-up: grouped `fitWLS()` refused (decision 10), workflows with `data` (N5); `prepare_model_data_for_test()`, `rdata_or_model_frame()` and the copies of the old preparation go | B11, N5 | Planned, 0.17.0 |
| 11 | `MASS::rlm()` refused in `rcontext_kind()` (decision 4); and the same question for a penalised `mgcv::gam()` with smooth terms, settled for every test together: accepted as before by the five of increment 1 until then, refused by the four of increment 2 (decision 23) | R6, N10 | Planned, 0.17.0 |

Outside the context, as separate pull requests: White's degrees of freedom
when the auxiliary columns span the constant (N2, done) and White streaming on
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
  fit, `MASS::rlm()`, `mgcv::gam()` with and without smooth terms, a
  `model = FALSE` fit whose data are gone, and a fit edited after the fit
  (increment 4). The panels (increment 5): a fit that drops every row of a
  unit, by an `lm`, a weighted `lm`, a weighted Gaussian `glm` and a `gam`, in
  fit order, shuffled and as a tibble, against the fit to the complete rows,
  an unbalanced panel fitted to its rows and dropped from the full data (N12),
  and a repeated `(id, time)`, shuffled. Increment 6: a `model = FALSE` fit
  whose data changed after the fit given its data, one whose data are kept,
  with and without `data`, and a variable of a second formula, `z`, missing at
  an observation of the fit and at a row the fit dropped.
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
  a missing `z`: 361 cells, run through each of the thirty-six functions, the
  three streaming tests of increment 7 among them, in one chunk of 10,000
  observations, with `B = 19` for the resampling test, through the five calls
  of a second formula (`context_second_formula`: Cook-Weisberg with
  `var_formula = ~ z`, the variance form test with `var_formula = ~ z` and
  `against = ~ z`, `fitWLS()` with and without `var_formula = ~ z`), and, in a
  test of their own, through the streaming tests in chunks of 7
  (`context_streaming_recipes()`), where White's streaming test refuses the
  collinear squares of a dummy, of `poly()` and of `x1` beside `I(x1^2)`
  (`context_cell_for_streaming()`).
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
  fit; the tests of increment 2 refuse a `gam` with smooth terms (decision
  23) once `data` is matched; and the functions of increment 5
  (`context_cell_for_panel()`) read `id` and `time`, which no model of the
  matrix uses, or the `x1` and `x2` of the scatter diagnostic: without `data`
  the panel tests give M1 and the scatter diagnostic and `runPanelTests()` the
  message on `data`, the model frame lacks `id` and `time`, Pesaran's test
  refuses a repeated pair (R2), and since the pull request of N12 tests an
  unbalanced panel, as that of a fit that dropped some rows of some units,
  against the same reference as the random-effects test, where increment 5
  refused it (N12), the scatter diagnostic refuses a zero
  weight before the guard, a fit with `na.exclude` is read as before, and the
  fits that dropped rows are compared with their fit to the complete rows;
  the tests computed from the model alone and RESET
  (`context_cell_for_model_alone()`) read nothing from `data`, which they
  match as every test does (decision 31), and keep their own refusals: the
  spread-level and Davidian-Carroll tests take a fit with `na.exclude`, RESET
  a zero weight, and the variance form test refuses weights that did not
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
| the streaming tests | in chunks of 7, 40 and 10,000, a term computed from its rows (`I(x1 > median(x1))`, `cut(x1, quantile(x1))`, and the median of a fit that dropped rows) against the same column computed beforehand, to `1e-10`, and against the exact test, to `1e-8`; in chunks of 7, 25, 119, 120 and 10,000, the exact test on unweighted, weighted, Gaussian `glm`, offset, `log()` and rank-deficient fits, to `1e-8` (`test-streaming.R`); on a `gam` with smooth terms, the Breusch-Pagan and Koenker statistics written out on the variables of its formula, to `1e-8`; the refusals of the context final in the runner that streams them |
| weights equivalence | the Gaussian `glm` against the weighted `lm`, `1e-8` (Fligner-Killeen, `1e-4`, N15); workspace weights against a column, and `fitWLS()` against `lm(weights = weights(fit))`, `identical()` |
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
| B3, B4 | The panel tests and the scatter diagnostic read `data` by position | Refused since 0.15.0; fixed in increment 5: they match the rows by name, by number or in fit order |
| B5 | A term computed from all the rows, re-evaluated on the rows of the fit | Fixed in increment 1 for the five, in increment 2 for its four, in increment 7 for the streaming tests; the others follow their increments |
| B6 | Streaming evaluates terms per chunk | Fixed in increment 7: the chunks are rows of the stored design |
| B7 | Offsets left out of a refit | Refused by #134; fixed in increment 2 |
| B8, B10, B17 | Stale `fitWLS()` weights; a zero prior weight of a `glm`; White streaming on `aov` | Fixed in 0.15.0 and 0.16.0 |
| B9 | A second formula matched by position | Fixed in increment 6: `var_formula` and `against` are evaluated on the rows the alignment confirmed (`rcontext_formula_rows()`); `rvariance_frame()` is gone |
| B11 | A grouped run of a `fitWLS()` fit refits without weights | Increment 10 (decision 10) |
| B12 | Weights or an offset from a column, `data` omitted | Fixed in increment 1 for the five and the runner, in increment 2 for its four; the group tests of increment 4 and the streaming tests of increment 7 gave values already; replicates in increment 8 |
| B13 | `na.exclude` | Increment 9 |
| B14 | Numbered data holding the rows the fit dropped, or only those it kept | Fixed in increment 1 for the five, in increment 2 for its four, in increment 3 for its six, in increment 4 for the five group tests (the complete rows renumbered, the tibble of the character-named fit), in increment 5 for the panel tests and the scatter diagnostic, in increment 7 for the streaming tests |
| B15 | `subset =` by index | Fixed in increment 1 for the five, in increment 2 for its four; the six of increment 3, the five of increment 4 and the streaming tests of increment 7 gave values already |
| B16 | The panel tests on a fit that dropped rows | Fixed in increment 5 for the random-effects test and the scatter diagnostic; Pesaran's test reaches the panel of the fit, which is unbalanced unless every row of a unit was dropped, and refused it (N12, decision 28) until the pull request of N12, since which it gives the value of the fit to the complete rows |
| R1 | Data whose values are not the fit's | Refused since 0.15.0 |
| R5 | The quantile test with an offset | Refused by #134; fixed in increment 2 |
| R2 | Repeated `(id, time)` | Refused by Pesaran's test since increment 5 (decision 13); the random-effects test reads no period |
| R3 | `var_formula` variables from the workspace | Refused since increment 6 (decision 9), with or without `data`, and the variables of `against` too |
| R4 | A multivariate `lm` | M7 in increment 1 for the five, in increment 2 for its four, in increment 3 for its six, in increment 4 for the group tests, in increment 5 for the panel tests and the scatter diagnostic, in increment 7 for the streaming tests |
| R6 | `MASS::rlm()` | Increment 11 (decision 4) |
| R7 | Messages that name variables a test does not need, or rows `data` holds | Fixed in increment 1 for the five, in increment 4 for the group tests |
| L1, L2 | Transformations and `offset()` terms without `data`; the model frame of a weighted or transformed fit as `data` | Fixed in increment 1 for the five, in increment 2 for its four, in increment 3 for its six, in increment 4 for the group tests (a variable the model frame does not hold gives M1), in increment 7 for the streaming tests |
| N1 | Goldfeld-Quandt refits the segments from the formula (6-10% off with `ns()`); the quantile test refits its quantile regressions from it | Fixed in increment 2 (decision 20) |
| N2 | White's degrees of freedom when the auxiliary columns span the constant | Fixed in 0.16.0 by a separate pull request (decision 22): the rank of the auxiliary design with its intercept, minus one, as `lmtest::bptest()` with the same terms; the harness allows the changes on the cells where main's count was wrong (the `poly()` fits, and the `gam` with a smooth term, whose auxiliary regression lost a column to its intercept and whose statistic changes by 0.28%) and verifies each against White's test written out (`identity_white_by_hand()`). Its follow-up (decisions 25 and 26) decides the rank on the auxiliary design of the centred regressors, refuses an auxiliary regression that fits the squared residuals exactly, saturated or not, and fits it with `lm.fit()`; the harness runs cells of its own for them, checks every number of a changed result, and requires the refusal on the exact fits |
| N3 | White streaming on collinear terms | A separate pull request (decision 22) |
| N4 | Harrison-McCabe's rank is not the fit's | Fixed in increment 3 (decision 21) |
| N5 | `runHeteroTests(<workflow>, data)` | Increment 10 |
| N6 | RESET reads a stale `model = FALSE` frame | Fixed in increment 2 for RESET, in increment 6 for the other tests computed from the model alone and `fitWLS()` |
| N7 | `cut(x1, quantile(x1))` stops with "new levels" | Fixed in increment 1 for the five, in increment 2 for its four, in increment 7 for the streaming tests, in chunks of any size (B6) |
| N8 | `bootstrapTestStatistic()` refuses every `glm` | Increment 8 |
| N9 | A multivariate `lm`: coercion errors | M7 in increment 1 for the five |
| N10 | Studentized Breusch-Pagan on `rlm` tests the least-squares refit | Decision 4, increment 11 |
| N11 to N15 | Influence diagnostics count aliased coefficients; Pesaran on an unbalanced panel; Szroeter through `test_factory`; Szroeter on a weighted fit; Fligner-Killeen ties | Separate pull requests; N13 unchanged by increment 3; N15 unchanged by increment 4, whose matrix compares Fligner-Killeen's test on another fit of the same model to `1e-4`. N12 refused by increment 5 (decision 28): Pesaran's test filled a matrix of periods by units, and stopped on a unit with fewer periods than the panel, or, when its number of periods divided that of the panel, recycled its residuals and returned a wrong value (0.234 where `plm::pcdtest()` gives -0.373, on `contract_data()` without its first five rows); it refused an unbalanced panel. Fixed by the pull request of N12, which removes the refusal: the statistic of an unbalanced panel is computed from the periods each pair of units shares, as `plm::pcdtest(test = "cd")` computes it (Pesaran 2004, Section 9, equation 31), `CD = sqrt(1 / P) sum_{i<j} sqrt(T_ij) rho_ij`, `rho_ij` the correlation of the residuals of units `i` and `j` over the `T_ij` periods both are observed at, each centred at its mean over them, over the `P` pairs with `T_ij >= 2` (`N (N - 1) / 2` when every pair has); a pair with `T_ij < 2` is left out with a warning, as plm leaves it out, and a panel without such a pair is refused; a correlation that is not defined, a unit constant over the periods of a pair, adds nothing and the pair stays in `P`, with a warning, as on a balanced panel, where plm returns `NA`; -0.373 on that panel, and plm's value to `1e-10` on 252 panels of 5 to 30 units and 4 to 12 periods, 228 of them unbalanced (late entry, early exit, gaps, 1 to 20 rows missing at random, a unit observed once), by an `lm`, a weighted `lm` and a Gaussian `glm`, weighted or not, directly, on the full data of a fit with `na.omit` and through `runPanelTests()`: 5,040 comparisons, the largest difference 3.6e-15 on the statistic, and on the cells of the matrix (below). A balanced panel keeps the matrix of periods by units, every value `identical()`. The refusal stays on the positional reading of a fit with `na.exclude` until increment 9 (decision 29) |
| N16 | Data that reproduce the model frame but carry another extra column | A limit no alignment can lift |
| N17 | A `gam`'s unnamed residuals matched to `data` by position: named data in another order than the fit gave a wrong value, without an error, in the tests that read a column of `data` (found in increment 3) | Fixed in increment 3 for Glejser, Park, Szroeter, Harrison-McCabe and Hartley, in increment 4 for the five group tests, in increment 5 for the panel tests and the scatter diagnostic, in increment 7 for the streaming tests; open for the White bootstrap's replicates until 8 |

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
| 9 | A variable of `var_formula` found only in the workspace, with `data = NULL`, is refused. Increment 6 applies it to every second formula (`var_formula`, `against`) and with `data` too: a variable that neither `data`, when it is given, nor, without `data`, the model frame holds is refused (with the message of `data`, or M1), never read from the workspace. A constant, a value of length one such as `k` in `~ I(hp^k)` or `pi`, is not a variable and is evaluated as R evaluates it (from the review of increment 6). | 2026-10-16 |
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
| 25 | After the review of N2, White's test decides its rank on the auxiliary design of the centred regressors, so that it does not depend on where they are located, fitting the columns as they are whenever that fit keeps them all; and it refuses an auxiliary regression with as many independent terms as observations, the runner falling back on White's test without cross products. Decided by the author. | 2026-10-21 |
| 26 | After the review of the follow-up of N2, White's test also refuses an auxiliary regression that fits the squared residuals exactly with fewer terms than observations, as on a paired design: its residual sum of squares at most `sqrt(.Machine$double.eps)` of the total. The refusal advises what the call allows, and the runner takes no fallback when the regressors alone fit the squared residuals exactly, since the Breusch-Pagan and Koenker tests would fit them exactly too. The same refusal in those two tests is left to a pull request of its own. | 2026-10-22 |
| 27 | The streaming tests run only when asked for, outside the context: `runHeteroTests()` and `runDiagnostics()` default to `chunk_threshold_mb = Inf`, a finite value asks for them, and `performWhiteTest()`, `performBPTest()`, `performKoenkerTest()` and the White bootstrap no longer switch to them above 100 MB. Streaming saves memory mainly in White's test, and the White streaming test stops on any model with a factor. The three streaming tests stay exported, not deprecated, and increment 7 still builds them on the stored design. Decided by the author. | 2026-10-22 |
| 28 | Pesaran's test refuses an unbalanced panel, whose units are not all observed at the same periods, in increment 5, with the class of the context and on every route, so that it never returns the value of recycled residuals; a fit that drops every row of a unit keeps a balanced panel. The refusal comes off in the pull request of N12, which computes the statistic on the periods each pair of units shares, as `plm::pcdtest()` does. Decided by the author: refuse now, fix later. Done in the pull request of N12 (2026-10-26): the refusal is off on every route but the positional reading of a fit with `na.exclude`, left to increment 9 (decision 29); a pair of units that shares fewer than two periods is left out of the statistic with a warning, as `plm::pcdtest()` leaves it out, rather than refused, and a panel in which no pair shares two periods is refused. | 2026-10-24 |
| 29 | The panel tests and the scatter diagnostic read a fit with `na.exclude` as before, its padded residuals against the rows of `data` by position, until increment 9. Decided by the author. | 2026-10-24 |
| 30 | A repeated pair of `id` and `time` is refused by Pesaran's test only, and so by `runPanelTests()` when it runs that test; the random-effects test reads no period. Decided by the author. | 2026-10-24 |
| 31 | The tests that read nothing from `data`, the nine tests computed from the model alone, RESET and `fitWLS()` without `var_formula`, match `data` through the guard, as every test built on the context does: one rule for every test. Data the guard cannot match are refused, where they returned the value of the fit, and numbered data in the order of the fit that the check of 0.15.0 refused are taken; that check goes, but for the type of `data`. It ends decision 24. Decided by the author, on the open question of the first version of increment 6. | 2026-10-26 |

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
