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
it. Increments 2 to 11 are planned; see [Migration plan](#migration-plan).

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
  permutation test, such data are refused if two rows of the fit tie so (a
  finding of the review of increment 1).
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
  any `offset()` terms. A refit through the context carries it.
- **Rank.** The rank of the fit, its aliased coefficients and the tolerance
  its rank was decided with (`1e-7` for an `lm`, `1e-11` for a `glm`); a
  rank-deficient fit is tested on the estimable part of its design, as since
  0.16.0.
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
| `rwarn_incomplete_rows(data, model)` | The "Removed N observations" warning, dropping nothing. |

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

## Migration plan

Each increment is one pull request from `main`, in this order: the
foundation first, then the corrections with the strongest pins, then the
migrations that keep every value, then the riskier resampling and
`na.exclude` changes, then the clean-up.

| # | Functions | Closes | Status |
| --- | --- | --- | --- |
| 1 | `R/model_context.R`; the guard split in two; `performWhiteTest()`, `performBPTest()` and `performBreuschPaganTest()`, `performKoenkerTest()`, `performRankPermutationTest()`, `performHighDimensionalTest()`, and through them the base statistic of `performWhiteTestRobust()`, `performBPTestRobust(studentize = FALSE)`, the registry names and the default battery | B5, B12, B14, B15, N7, R4/N9, R7 for these; L1, L2 | **Done, 0.16.0** |
| 2 | `.studentized_bp_test()` (`performStudentizedBPTest()`, `performBPTestRobust()`, `student_bp`), `performRESETTest()`, `performGQTest()` on the stored design, `performQuantileRegressionTest()` on `y - offset`; `rrefuse_offset()` goes | B7, N1, N6 (RESET), R5 | Planned |
| 3 | `performGlejserTest()`, `performParkTest()`, `performSzroeterTest()`, `performHarrisonMcCabeTest()` (rank at the fit's tolerance), `performHartleyFmaxTest()`, `performSpatialHeteroTest()` | B14, B15, L1, M1 for `variable`, `order_by`, `group`; N4 | Planned |
| 4 | The group tests (Levene, Brown-Forsythe, Fligner-Killeen, Bartlett, O'Brien); their copies of the alignment go | the character-named fit with its tibble; R7 | Planned |
| 5 | `performPesaranTest()`, `performBPRandomEffectsTest()`, `performScatterDiagnostic()`, `runPanelTests()`: `id`, `time` and `variable` read at the confirmed rows | B3, B4, B16, R2 (decision 13) | Planned |
| 6 | The tests computed from the model alone (the context applies `rcheck_model_frame()` to them); Cook-Weisberg with `var_formula`; `performVarianceFormTest()`; `fitWLS(data, var_formula)` | B9, R3, N6 | Planned |
| 7 | The three streaming tests, chunk by chunk from the stored design | B6; B12, B14, L1 for these | Planned |
| 8 | The resampling tests and `bootstrapTestStatistic()`, replicates from the stored design: `performWhiteTestBootstrap()`, whose observed statistic and replicates move together, and `rwhite_test_rebuilt_design()`, which keeps them on the rebuilt design until then, goes; `performWildBootstrapTest()`; `bootstrapTestStatistic()` and `performWhiteTestRobust(bootstrap = TRUE)`, which refuse a fit whose frame cannot be rebuilt from `data` until then | B12 for replicates, N8 | Planned |
| 9 | `na.exclude`: unpadded residuals in the checks, padded outputs per observation | B13 | Planned |
| 10 | The runners and the clean-up: grouped `fitWLS()` refused (decision 10), workflows with `data` (N5); `prepare_model_data_for_test()`, `rdata_or_model_frame()` and the copies of the old preparation go | B11, N5 | Planned |
| 11 | `MASS::rlm()` refused in `rcontext_kind()` (decision 4); and the same question for a penalised `mgcv::gam()` with smooth terms, accepted as before until then | R6, N10 | Planned |

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
  rows, of identifiers, of `subset =` and of model classes on `ols`: 262 cells,
  run through each of the five functions, with `B = 19` for the resampling
  test. The full matrix runs under `skip_on_cran()`, a 12-cell subset on CRAN.
  Tibbles and data.tables come from `dplyr` and `data.table` under
  `skip_if_not_installed()`.
- **Oracles** (relations between calls, not stored values):

| Rule | Expectation |
| --- | --- |
| invariance over the accepted forms of data | `identical()` to the data frame of the fit |
| `NULL` and the model frame | `identical()` to the call with data |
| refused order | the guard's error, class `heteroTests_data_rows_error` |
| offset equivalence | equal to the fit of `y - o` without an offset, to `1e-10` |
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
| B5 | A term computed from all the rows, re-evaluated on the rows of the fit | Fixed in increment 1 for the five; the others follow their increments |
| B6 | Streaming evaluates terms per chunk | Increment 7 |
| B7 | Offsets left out of a refit | Refused since #134; values in increment 2 |
| B8, B10, B17 | Stale `fitWLS()` weights; a zero prior weight of a `glm`; White streaming on `aov` | Fixed in 0.15.0 and 0.16.0 |
| B9 | A second formula matched by position | Increment 6 |
| B11 | A grouped run of a `fitWLS()` fit refits without weights | Increment 10 (decision 10) |
| B12 | Weights or an offset from a column, `data` omitted | Fixed in increment 1 for the five and the runner; replicates in increment 8 |
| B13 | `na.exclude` | Increment 9 |
| B14 | Numbered data holding the rows the fit dropped, or only those it kept | Fixed in increment 1 for the five |
| B15 | `subset =` by index | Fixed in increment 1 for the five |
| B16 | The panel tests on a fit that dropped rows | Increment 5 |
| R1, R5 | Data whose values are not the fit's; the quantile test with an offset | Refused since 0.15.0 and 0.16.0 |
| R2 | Repeated `(id, time)` | Increment 5 (decision 13) |
| R3 | `var_formula` variables from the workspace | Increment 6 |
| R4 | A multivariate `lm` | M7 in increment 1 for the five |
| R6 | `MASS::rlm()` | Increment 11 (decision 4) |
| R7 | Messages that name variables a test does not need, or rows `data` holds | Fixed in increment 1 for the five |
| L1, L2 | Transformations and `offset()` terms without `data`; the model frame of a weighted or transformed fit as `data` | Fixed in increment 1 for the five |
| N1 | Goldfeld-Quandt refits the segments from the formula (6-10% off with `ns()`) | Increment 2 (decision 20) |
| N2, N3 | White's degrees of freedom when the auxiliary columns span the constant; White streaming on collinear terms | Separate pull requests (decision 22) |
| N4 | Harrison-McCabe's rank is not the fit's | Increment 3 (decision 21) |
| N5 | `runHeteroTests(<workflow>, data)` | Increment 10 |
| N6 | RESET reads a stale `model = FALSE` frame | Increments 2 and 6 |
| N7 | `cut(x1, quantile(x1))` stops with "new levels" | Fixed in increment 1 for the five |
| N8 | `bootstrapTestStatistic()` refuses every `glm` | Increment 8 |
| N9 | A multivariate `lm`: coercion errors | M7 in increment 1 for the five |
| N10 | Studentized Breusch-Pagan on `rlm` tests the least-squares refit | Decision 4, increment 11 |
| N11 to N15 | Influence diagnostics count aliased coefficients; Pesaran on an unbalanced panel; Szroeter through `test_factory`; Szroeter on a weighted fit; Fligner-Killeen ties | Separate pull requests |
| N16 | Data that reproduce the model frame but carry another extra column | A limit no alignment can lift |

## Decisions

| # | Decision | Date |
| --- | --- | --- |
| 1 | Last-bit differences of `poly()` terms that come from using the stored design are accepted: the value equals, with `identical()`, what main computes on the fit's stored design (the model refitted with `x = TRUE`); differences against main's rebuilt design are in the last bits (measured up to 1.3e-15 relative on the statistic). | 2026-10-16; wording 2026-10-18 |
| 2 | The response alone is not enough to confirm rows: superseded by the 0.15.0 guard, which compares every column of the model frame. | 2026-10-16 |
| 3 | Numbered data in another order than the fit are refused, not matched by position. | 2026-10-16 |
| 4 | `MASS::rlm()` is refused, in increment 11. | 2026-10-18 |
| 5 | The quantile regression test refuses an offset until it accounts for it; increment 2 is the fix. | 2026-10-17 |
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
