# heteroTests Roadmap

This document tracks where the package is going. It is updated alongside
meaningful code changes. Milestones are ordered, not dated. The history of what
changed and why lives in `NEWS.md`; the statistical evidence lives in
`inst/validation/`.

## Where things stand (0.14.0)

- **On CRAN.** 0.11.2 was accepted in September 2026, after one review round on
  0.11.1. 0.12.0, 0.13.0 and 0.14.0 are GitHub releases: the next submission
  is 1.0.0 (decided 2026-10-11).
- **The validation matrix is complete** for every exported `perform*Test()`.
  The sweep found six broken procedures, all corrected or withdrawn, and
  twenty-five of the twenty-six heteroscedasticity tests hold their nominal
  level (`inst/validation/README.md`). The tests added since have studies of
  their own: `inst/validation/harrison-mccabe-size-power.R` and
  `inst/validation/glejser-skewed-errors.R`.
- **Weighted fits were outside that matrix, and wrong.** Every design in it
  fitted an unweighted model. On a weighted fit the tests read the raw
  residuals, whose variance differs across observations by assumption, and
  rejected a correctly weighted model every time. 0.12.0 corrects that and
  adds weighted designs to the matrix (`inst/validation/weighted-fits-size.R`).
  In 0.13.0 the bootstrap tests accept a weighted fit too, with designs of
  their own in the same study.
- **Reference equivalence** is asserted to `1e-8` against `lmtest`, `car` and
  `plm` wherever a reference implementation exists. `lmtest::bptest()` is not
  a reference for weighted fits: it tests the raw residuals.
- **The public surface has been reviewed.** 0.14.0 gave each of the 129
  exports of 0.13.0 a verdict (the export inventory under 0.14.0): 104 are core
  API, 22 are deprecated and warn, and 3 move to a companion package that has
  no name yet. With `bootstrapTestStatistic()`, `simulate_size()` and
  `simulate_power()` the package exports 132 objects, and `test-public-api.R`
  asserts the 107 planned for 1.0.0. What stands between 0.14.0 and 1.0.0 is
  the interface itself, whose changes break calls (0.15.0), and the evidence
  and internals of 0.16.0.

## What 1.0.0 means

1.0.0 is a promise rather than a feature count. From 1.0.0:

- Exported names, argument names and order, and defaults change only in a major
  release, and only after a minor release in which the old form still works and
  warns.
- Every exported test returns an `htest` object with a documented, uniform set
  of fields.
- A correction to a statistic, its degrees of freedom or its p-value is a bug
  fix. It is never deferred for the sake of compatibility. It bumps the minor
  version and gets its own `NEWS.md` section, as 0.11.0 did.

Version numbers follow from that: a patch release changes no reported value; a
minor release adds functionality or corrects a statistic; a major release
breaks the API.

Everything below exists to make that promise safe to give. The sequencing rule
this file has always followed still holds: statistical correctness first, then
the API, and never both in one release. 0.12.0 reopened correctness for
weighted fits and closed it again, and 0.13.0 corrected what was found after
it, so the API is next.

## Guiding principles

- **Statistical correctness first.** Every test must reproduce an established
  reference (or a defensible derivation) and be covered by a test that checks
  *behaviour* (size and power), not just object structure.
- **A reference comparison is not a substitute for a size check.** Four of the
  six faults the sweep found were in procedures with no reference to compare
  against, and the two that had one agreed with it.
- **Guards are simulated, not stored.** A stored-value regression test would
  have frozen each of those faults rather than caught it.
- **One consistent interface.** All diagnostics return base-R `htest` objects
  and follow the `perform*Test(model, data, ...)` convention. (Nine exported
  tests do not yet; see 0.15.0.)
- **Scale and robustness as first-class concerns**: streaming implementations
  for large data, resampling and robust variants for small or non-normal
  samples.

## Milestones

### 0.11.x: CRAN maintenance (now)

- [ ] Watch the CRAN check results across all flavours and fix anything flagged
  within the deadline CRAN sets.
- [x] README: install from CRAN first (`install.packages("heteroTests")`), with
  GitHub as the development route; add a CRAN badge and use the canonical URL
  `https://CRAN.R-project.org/package=heteroTests`.
- [x] The README feature list is out of date. It still advertises an HC0–HC4
  covariance test and Cameron–Trivedi, both removed in 0.8.0. Bring it in line
  with the exports, and extend `test-documentation-consistency.R` so a README
  or vignette that names a removed function fails. The guard went into
  `test-public-api.R`, beside the list of removed exports, and also matches
  the names prose gives the removed tests.
- [x] Documentation drift: the `performStudentizedBPTest()` help page describes
  `performKoenkerTest()` as "the absolute-residual variant", which predates the
  Breusch–Pagan/Koenker correction. Koenker is the studentized `n R^2` form.
- [x] Check that the Zenodo record picked up 0.11.2, and cite the CRAN release
  in `inst/CITATION` and `CITATION.cff`. Zenodo archives 0.11.2 and 0.12.0;
  only the 0.12.0 record links to CRAN.
- [x] Close tracking issue #38 as On CRAN. It was closed on 2026-09-27.
- [x] Reinstate a release gate. `R CMD check --as-cran` can stay weekly rather
  than per merge, but it must pass in full on the tagged commit (vignettes and
  `--run-donttest` included) before any submission, alongside win-builder
  (R-devel). `release.yml` now runs `--as-cran --run-donttest` on the tarball
  it is about to tag and creates no tag if the check warns or errors;
  win-builder stays a manual step (`cran-comments.md`).
- [x] The gate failed. The weekly `--as-cran` job stopped with an error
  on 2026-09-28 and on 2026-10-05: the `\donttest{}` example of
  `performWhiteTestBootstrap(parallel = TRUE)` starts `detectCores() - 1`
  processes, three on the runner, and `--as-cran` allows two. The example is
  skipped on Windows, so a local check there does not see it. Fixed in 0.13.0
  by changing the example, which now runs the call only where it starts at
  most two processes; the "CRAN submission checks" job passed on the branch on
  2026-10-11 and on `main` on 2026-10-12. The function still uses
  `detectCores() - 1` workers; capping them changes how the draws are split,
  and belongs with reproducibility under `parallel = TRUE` in 0.15.0.

### 0.12.0: weighted fits and the shape of the variance (done)

This release was not on the roadmap. It took the 0.12.0 number because a
corrected statistic bumps the minor version, and the milestones below moved up
by one.

- [x] **Weighted fits.** On an `lm` fitted with weights, every residual-based
  test is computed from the Pearson residuals `sqrt(w) * e`. Goldfeld–Quandt
  and RESET refit with the weights. Unweighted fits return bit-identical
  values.
- [x] **`performVarianceFormTest()`**, a test whose null hypothesis is a
  variance function (exponential or power) rather than constant variance.
- [x] **`fitWLS()`** takes `var_formula` and `form`, and records the variance
  function it fitted.
- [x] **Evidence.** `inst/validation/weighted-fits-size.R` and
  `inst/validation/variance-form-size-power.R`, with simulated guards in
  `test-weighted-fits.R` and `test-variance-form.R`.

Found on the way and left for their own changes:

- [x] **Resampling on weighted fits.** `performWildBootstrapTest()`,
  `performWhiteTestBootstrap()`, `rbootstrap_test_statistic()` and
  `performQuantileRegressionTest()` refused a weighted fit, because they
  refitted without the weights. Resolved in 0.13.0 for the first three:
  they resample the Pearson residuals, return each draw to the scale of its
  observation and refit with the weights. That change leaves an unweighted
  fit as it was; the corrections 0.13.0 also makes to unweighted refits are
  listed under 0.13.0 below. In `inst/validation/weighted-fits-size.R`, at 2000
  replications with `B = 499`, they reject a correctly weighted fit 0.045 to
  0.054 of the time in two designs. `performQuantileRegressionTest()` still
  refuses one: on the weighted scale its slopes differ across quantiles under
  the null hypothesis, and a version for weighted fits needs its own
  construction. A
  lead: the quantile regressions of `sqrt(w) y` on `sqrt(w) X` with a column
  of ones added have equal slopes under the null hypothesis, which is the
  location-shift form the test assumes. It needs a derivation of the
  reference distribution and a size study before it can ship.
- [x] **`runSurveyHeteroTests()` did not use the survey weights.** It called
  `stats::lm(formula, data = data, weights = weights)`, where `weights` was a
  local variable. `lm()` looks the name up in `data` and then in the
  environment of the formula, found `stats::weights`, failed, and the
  `tryCatch()` refitted without weights. Resolved in 0.13.0 by making the
  function say what it does, because using the weights would have been worse:
  sampling weights are not inverse error variances, and a test on a fit that
  uses them rejected a homoscedastic model 97% to 100% of the time when the
  weights varied with the regressor. `runHeteroTests(formula, design)` did
  exactly that through a `svyglm()` fit, and now takes the unweighted route
  too. Evidence: `inst/validation/survey-designs-size.R`. A design-based test
  is under *Candidate tests*.
- [x] **`.ht_fit_from_formula()` had the same scoping fault** and always failed
  with `invalid type (closure) for variable '(weights)'`, so the per-group
  refits built from an `lm`, `glm` or parsnip fit could not run. Fixed in
  0.13.0: each group is refitted by evaluating the model's own call on it,
  which gives it its own rows of the weights and keeps the family of a `glm`.
- [x] **`tidy()` returned one row per degree of freedom.** A result with two
  parameters, such as the F test of `performVarianceFormTest()` or the wild
  bootstrap with its `df` and `B`, was tidied into two rows, and the grouped
  and suite methods stacked them. Fixed in 0.13.0: one row per test, with
  the second parameter in a column of its own, `parameter2`.
- [x] **`autoplot()` of a test suite did not show significant results.** The
  bars were p-values on a linear axis from 0 to 1, so a test with p = 0.002
  had no visible bar, and the chart was empty exactly when the tests rejected.
  The highlighting of p < 0.05 never applied either: the condition was written
  `isTRUE(df$p.value < 0.05)`, which is `FALSE` for a suite of more than one
  test. The grouped method had the same condition and added a facet for the
  `suggestions` column. Fixed in 0.13.0: the bars are `-log10(p)` on an
  axis labelled with p-values, with a line at `alpha` and the p-value printed
  beside each bar.
- [ ] **An additive variance function**, `sigma^2 = a + z'b`, was implemented
  for `fitWLS()` and the form test and withdrawn before release. Under its own
  null, at 5000 replications, the test rejected 6.1% of the time at the 5%
  level with 150 and with 400 observations, outside the release gate, and the
  fitted variances were not all positive in 17% of samples at 50 observations
  and 6% at 150. It needs an estimator that keeps the fitted variances
  positive before it can come back.
- [ ] **Tests of constant variance on a `fitWLS()` fit** take the estimated
  weights as known. With the variance function right and Gaussian errors,
  Koenker rejects 8% of the time at the 5% level with 150 observations and 11%
  with 600, White 6% and 8%, and Harvey never. That is documented, and
  `performVarianceFormTest()` is the calibrated alternative, but a correction
  for the estimation step would let `compareModelDiagnostics()` report
  calibrated p-values for a weighted remedy.

  The bootstrap tests hold the weights fixed and share the error: the wild
  bootstrap rejects 0.086 with 150 observations and 0.105 with 600, where it
  rejects 0.054 and 0.048 with the true weights in the same design. A
  bootstrap that runs `fitWLS()` again on every replicate is such a
  correction. In the same designs it rejects 0.042 to 0.060 with 150
  observations for the wild bootstrap, the White bootstrap and
  `rbootstrap_test_statistic()` with Koenker's statistic, and 0.051 with 600
  for the wild bootstrap (`inst/validation/weighted-fits-size.R`). Proposal,
  not implemented: an argument to the three functions that refits the
  variance function `fitWLS()` recorded on the model, with the same
  `var_formula` and `form`, in each replicate. It applies only to a fit that
  carries that record; weights the user supplied are known and stay fixed. See
  *Decisions needed*.

### 0.13.0: what was found after 0.12.0 (done)

This release was not on the roadmap either. It took the 0.13.0 number because
it corrects statistics and adds functionality, and the milestones below moved
up by one again.

- [x] **Values that changed.** The bootstrap tests keep an offset given as an
  argument when they refit an unweighted model, `rbootstrap_test_statistic()`
  matches the rows of the data to the fit by name and leaves out rows the fit
  dropped, and `performWhiteTestBootstrap()` regenerates a transformed
  response, where its p-value was 1. `fitWLS()` keeps the offset of the model
  it refits, and `performVarianceFormTest()` changes with it on a model with an
  offset. The Szroeter test that `runHeteroTests()` runs by name orders the
  observations by the fitted values instead of by the second column of the
  data. `runHeteroTests()` and `runDiagnostics()` no longer test a survey
  design through a fit weighted by its sampling weights. `tidy()` returns one
  row per test.
- [x] **What was found on the way to 0.12.0**: the resampling tests accept a
  weighted fit, and the survey designs, the grouped refits and `autoplot()`
  are fixed (above).
- [x] **Two tests from the candidates**: `performHarrisonMcCabeTest()` and
  `performGlejserTest(robust = TRUE)`. Two more were measured and not
  adopted, the modified Wald test and stage 1 of the Dette, Neumeyer and Van
  Keilegom test (*Candidate tests*).
- [x] **`listDiagnostics()`**, and the release gate in `release.yml` (0.11.x,
  above).

### 0.14.0: review the public surface, deprecate and remove nothing (done)

Goal: every one of the 129 exports gets a verdict, which is one of **core API**,
**renamed**, **internal** or **moved out**. Nothing is removed in this release.
Everything that will go warns.

The author accepted the four proposals for this release on 2026-10-14: the
dashboard and the benchmark suite move to a companion package and the
recommendation engine stays, with its return format frozen;
`performCookWeisbergTest()` stays, with an optional `var_formula`, and
`performNCVTest()` is deprecated; `test()` is folded into
`summary.HeteroDiagnostic()`; and `simulate_*()` and `sigma_*()` stay as a
documented snake_case family while the one-offs are renamed. The companion
package has no name yet, so the dashboard and benchmark functions are not
deprecated until it has one.

- [x] **Duplicate exports.** Each duplicate below is deprecated in favour of
  the name it duplicates. `performStudentizedBPTest()` agrees with
  `performKoenkerTest()` to `1e-8` except on a `fitWLS()` fit, where it reads
  the stale weights listed under 0.16.0; it keeps its implementation, so no
  value changes.
  - `performBreuschPaganTest()` is `performBPTest()`, assigned at
    `R/performBPTest.R:232`.
  - `performStudentizedBPTest()` is a separate implementation that matches
    `performKoenkerTest()` in every column of the full sweep. Confirm the
    statistics agree to `1e-8`, then keep one.
  - `performCookWeisbergTest()` delegates to `performNCVTest()` with the fitted
    values as the variance regressor. `performCookWeisbergTest()` becomes the
    one entry point for the Cook–Weisberg score test, with `var_formula` as an
    optional argument, and `performNCVTest()` is deprecated (decided above).
  - `*Enhanced` pairs: `checkModel()`/`checkModelEnhanced()`,
    `plotResidualsFitted()`/`plotResidualsFittedEnhanced()` and
    `plotDiagnosticSuite()`/`plotDiagnosticSuiteEnhanced()`. Merge each
    improvement into the base name.
- [x] **Internal machinery that is exported:** `std_error()`, `std_warning()`,
  `validateTestInputs()`, `checkData()`, `TestFactory`/`test_factory()`, the
  cache functions (`cachedTest()`, `clearTestCache()`, `clearAnalysisCache()`)
  and the `ht_*` logging functions. Unexport them unless a user-facing reason
  exists; for logging, keep at most `ht_set_log_level()`. Deprecated in
  0.14.0 and unexported in 1.0.0, except the cache functions, which stay for
  now (decided), and `ht_set_log_level()`.
- [ ] **The five `r`-prefixed exports** (`rbootstrap_test_statistic()`,
  `rcalculateEffectSize()`, `restimate_test_power()`,
  `rrunAdvancedDiagnostics()` and `rvalidate_against_reference()`) follow no
  naming scheme in the package. Rename each to the house convention or
  unexport it. Four are done in 0.14.0: `rbootstrap_test_statistic()` is
  renamed `bootstrapTestStatistic()` and the other three are deprecated as
  internal helpers. `rrunAdvancedDiagnostics()` stays core API (decided), and
  is renamed `runAdvancedDiagnostics()` in 0.15.0.
- [x] **`test()` is a generic with a very common name.** Attached after
  `devtools`, it masks `devtools::test()`. Fold it into
  `summary.HeteroDiagnostic()` (decided above). Deprecated in 0.14.0, naming
  `runDiagnostics(x$model, x$data, tests)`; `summary.HeteroDiagnostic()`
  calls `runDiagnostics()` and returns what it returned.
- [x] **Naming convention.** camelCase dominates (`performXTest()`, `fitWLS()`,
  `plotX()`). The simulation family (`simulate_hetero()`, `sigma_*()`) is
  declared a deliberate snake_case family and documented as one (decided
  above). The one-offs (`run_benchmark_suite()`, `generate_benchmark_report()`,
  `simulate_type_I_errors()`, `simulate_power_analysis()`) are renamed. The
  last two are `simulate_size()` and `simulate_power()` in 0.14.0; the first
  two move out with the benchmark suite instead.
- [ ] **Scope.** The dashboard (`R/dashboard.R`, 698 lines) and the benchmark
  suite (`R/benchmarking_system.R`, 798 lines) move to a companion package
  (decided above), once it has a name. Between them they account for most of
  the optional `Suggests` (`shiny`, `DT`, `plotly`, `htmlwidgets`, `bench`).
  Open: the package has no name, so they are not deprecated in 0.14.0 (see
  *Decisions needed*).
- [x] **Related model checks** (`performRESETTest()`, `performVIFDiagnostic()`,
  `performInfluenceDiagnostics()`, `performScatterDiagnostic()`) are not
  heteroscedasticity tests. Keep them, grouped in the reference index as model
  checks, so they are not mistaken for tests of the variance.
- [x] **Mechanism.** Use base `.Deprecated()`, so no new dependency is needed.
  Each call warns and names its replacement. `NEWS.md` carries an old → new
  mapping table.
- [x] **Exit criterion.** An export inventory with a verdict for every one of
  the 129 exports, and `test-public-api.R` extended to assert the planned 1.0
  export list, so that an accidental new export fails CI.

#### Export inventory

Every export in `NAMESPACE` on 2026-10-14, with a verdict. Of the 129, 104 are
core API, 10 are renamed, 12 become internal and 3 move out. The S3 methods
registered in `NAMESPACE` are not exports and are not listed;
`S3method(test, HeteroDiagnostic)` goes with `test()`. The verdicts apply the
decisions of 2026-10-14 as written, and the author's decisions on this
inventory, taken the same day, are marked "decided".

For each renamed or internal export, the last column lists every caller found
by a search of `R/` (code, not comments or the definition), `vignettes/` and
`inst/validation/`, adds `README.md` where it names the export, and counts the
lines in `tests/` that call it. The notebooks in `inst/tutorials/` call none of
these exports. The article was not searched: it moves to the 1.0 API after
1.0.0. An export that becomes internal keeps working for its callers in `R/`,
and for the tests, which run inside the namespace; a renamed export keeps
working under its old name and warns. What has to change in each pull request
are the callers outside `R/`, and the package's own calls of a name that will
warn.

| Export | Area | Verdict | Evidence that the change is safe |
| --- | --- | --- | --- |
| `performWhiteTest()` | test | core API |  |
| `performWhiteTestStreaming()` | test | core API |  |
| `performWhiteTestRobust()` | test | core API |  |
| `performWhiteTestBootstrap()` | test | core API |  |
| `performBPTest()` | test | core API |  |
| `performBPTestStreaming()` | test | core API |  |
| `performBPTestRobust()` | test | core API |  |
| `performBreuschPaganTest()` | test | renamed: `performBPTest()`, the same object | R/: none (an alias, assigned in `performBPTest.R`). Vignettes: `using_heteroTests.Rmd:128`. inst/validation/: `full-sweep-size-power.R:169`; 2 lines of `README.md`. Tests: 3 lines. |
| `performKoenkerTest()` | test | core API |  |
| `performKoenkerTestStreaming()` | test | core API |  |
| `performStudentizedBPTest()` | test | renamed: `performKoenkerTest()`, once item 2 shows they agree to `1e-8` (provisional, decided) | R/: registry name `"student_bp"` (`diagnosticsRegistry.R:70`, `test_factory.R:105`); `performBPTestRobust(studentized = TRUE)` (`robust_implementations.R:249`); `rvalidate_against_reference()` (`:455`). Vignettes: none. inst/validation/: `full-sweep-size-power.R:171`, `weighted-fits-size.R:138`; 4 lines of `README.md`. Tests: 8 lines. |
| `performGQTest()` | test | core API |  |
| `performParkTest()` | test | core API |  |
| `performSzroeterTest()` | test | core API |  |
| `performHarrisonMcCabeTest()` | test | core API |  |
| `performWildBootstrapTest()` | test | core API |  |
| `performQuantileRegressionTest()` | test | core API |  |
| `performRankPermutationTest()` | test | core API |  |
| `performHighDimensionalTest()` | test | core API |  |
| `performSpatialHeteroTest()` | test | core API |  |
| `performSpearmanTest()` | test | core API |  |
| `performLeveneTest()` | test | core API |  |
| `performGlejserTest()` | test | core API |  |
| `performBrownForsytheTest()` | test | core API |  |
| `performFlignerKilleenTest()` | test | core API |  |
| `performBartlettTest()` | test | core API |  |
| `performHarveyTest()` | test | core API |  |
| `performArchLMTest()` | test | core API |  |
| `performMcLeodLiTest()` | test | core API |  |
| `performHartleyFmaxTest()` | test | core API |  |
| `performOBrienTest()` | test | core API |  |
| `performCookWeisbergTest()` | test | core API |  |
| `performNCVTest()` | test | renamed: `performCookWeisbergTest()`, which gains an optional `var_formula` (decided) | R/: registry name `"ncv"` (`diagnosticsRegistry.R:67`); `performCookWeisbergTest()` delegates to it (`performCookWeisbergTest.R:69`); the benchmark suite (`benchmarking_system.R:368`). Vignettes: none. inst/validation/: `full-sweep-size-power.R:165`, `pass-a-size-power.R:87`, `weighted-fits-size.R:133,196`; 6 lines of `README.md`. Also `README.md:34`. Tests: 20 lines. |
| `performSpreadLevelTest()` | test | core API |  |
| `performPesaranTest()` | test | core API |  |
| `performBPRandomEffectsTest()` | test | core API |  |
| `performDavidianCarrollTest()` | test | core API |  |
| `performVarianceFormTest()` | test | core API |  |
| `performBoxMTest()` | test | core API |  |
| `rbootstrap_test_statistic()` | test | renamed: `bootstrapTestStatistic()` (decided) | R/: `performWhiteTestRobust()` and `performBPTestRobust()` (`robust_implementations.R:135,270`). Vignettes: none. inst/validation/: `offset-refits-size.R`, `weighted-fits-size.R`, labels in `make-table.R`; 22 lines of `README.md`. Also the *Decisions needed* row on `fitWLS()` fits. Tests: 60 lines. |
| `performScatterDiagnostic()` | model check | core API |  |
| `performVIFDiagnostic()` | model check | core API |  |
| `performRESETTest()` | model check | core API |  |
| `performInfluenceDiagnostics()` | model check | core API |  |
| `checkModel()` | model check | core API |  |
| `checkModelEnhanced()` | model check | renamed: merged into `checkModel()` | R/: `performWhiteTestStreaming()` (`performance.R:267`). Vignettes: none. inst/validation/: none. Tests: 8 lines. |
| `runHeteroTests()` | runner | core API |  |
| `runHeteroTestsParallel()` | runner | core API (decided) |  |
| `runDiagnostics()` | runner | core API |  |
| `runDiagnosticPlots()` | runner | core API |  |
| `registerDiagnostic()` | runner | core API |  |
| `listDiagnostics()` | runner | core API |  |
| `registerPlot()` | runner | core API |  |
| `runMultivariateTests()` | runner | core API |  |
| `runTimeSeriesTests()` | runner | core API |  |
| `runPanelTests()` | runner | core API |  |
| `runSurveyHeteroTests()` | runner | core API |  |
| `compareModelDiagnostics()` | runner | core API |  |
| `compareTestResults()` | runner | core API (decided) |  |
| `analyzeMLResiduals()` | runner | core API |  |
| `HeteroDiagnostic()` | runner | core API |  |
| `test()` | runner | renamed: its warning names `runDiagnostics(x$model, x$data, tests)`; `summary()` is unchanged (decided) | R/: `summary.HeteroDiagnostic()` calls it (`HeteroDiagnostic.R:59`). Vignettes: `real_world_case_studies.Rmd:110`, `tutorial.Rmd:32`, `using_heteroTests.Rmd:100,105`. inst/validation/: none. Also `README.md:143`. Tests: 19 lines. `summary.HeteroDiagnostic()` will call `runDiagnostics()` directly. |
| `fitWLS()` | remediation | core API |  |
| `fitRobust()` | remediation | core API |  |
| `autoTransform()` | remediation | core API |  |
| `suggestRemediation()` | remediation | core API |  |
| `autoCompareRemediations()` | remediation | core API |  |
| `analyseDatasetCharacteristics()` | recommendation | core API |  |
| `suggestDiagnosticsForProfile()` | recommendation | core API |  |
| `interpretHeteroTestResults()` | recommendation | core API |  |
| `recommendRemediationStrategies()` | recommendation | core API |  |
| `warnDiagnosticAssumptions()` | recommendation | core API |  |
| `buildDiagnosticDecisionTree()` | recommendation | core API |  |
| `composeDiagnosticNarrative()` | recommendation | core API |  |
| `generateHeteroRecommendations()` | recommendation | core API |  |
| `simulate_hetero()` | simulation | core API |  |
| `simulate_arch1()` | simulation | core API |  |
| `sigma_linear()` | simulation | core API |  |
| `sigma_exponential()` | simulation | core API |  |
| `sigma_group()` | simulation | core API |  |
| `sigma_piecewise()` | simulation | core API |  |
| `sigma_poly()` | simulation | core API |  |
| `sigma_sin()` | simulation | core API |  |
| `sigma_multiplicative()` | simulation | core API |  |
| `sigma_spatial()` | simulation | core API |  |
| `sigma_logistic()` | simulation | core API |  |
| `sigma_inverse()` | simulation | core API |  |
| `sigma_power()` | simulation | core API |  |
| `sigma_step()` | simulation | core API |  |
| `sigma_u_shape()` | simulation | core API |  |
| `sigma_exp_decay()` | simulation | core API |  |
| `sigma_gaussian_peak()` | simulation | core API |  |
| `sigma_piecewise_linear()` | simulation | core API |  |
| `simulate_type_I_errors()` | simulation | renamed: `simulate_size()` (decided) | R/: none. Vignettes: none. inst/validation/: none. Tests: 3 lines. |
| `simulate_power_analysis()` | simulation | renamed: `simulate_power()` (decided) | R/: none. Vignettes: named in the prose of `troubleshooting.Rmd:44`. inst/validation/: none. Tests: 3 lines. |
| `plotResidualsFitted()` | plotting | core API |  |
| `plotSpreadLevel()` | plotting | core API |  |
| `plotResidualDensity()` | plotting | core API |  |
| `plotResidualQQ()` | plotting | core API |  |
| `plotBubbleVariance()` | plotting | core API |  |
| `plotDiagnosticSuite()` | plotting | core API |  |
| `plotBeforeAfter()` | plotting | core API |  |
| `plotResidualsFittedEnhanced()` | plotting | renamed: merged into `plotResidualsFitted()` | R/: `plotDiagnosticSuiteEnhanced()` (`plotDiagnostics.R:279`). Vignettes: none. inst/validation/: none. Tests: 5 lines. |
| `plotDiagnosticSuiteEnhanced()` | plotting | renamed: merged into `plotDiagnosticSuite()` | R/: `generateDiagnosticReport()` (`reporting.R:53`); the dashboard (`dashboard.R:536`). Vignettes: none. inst/validation/: none. Tests: 4 lines. |
| `theme_hetero()` | plotting | core API |  |
| `scale_colour_hetero_diagnostic()` | plotting | core API |  |
| `scale_fill_hetero_diagnostic()` | plotting | core API |  |
| `generateDiagnosticReport()` | reporting | core API |  |
| `downloadTeachingData()` | reporting | core API |  |
| `ht_set_log_level()` | infrastructure | core API |  |
| `std_error()` | infrastructure | internal | R/: 92 lines in 24 files, all internal callers, which keep working. Vignettes: none. inst/validation/: none. Tests: 9 lines. |
| `std_warning()` | infrastructure | internal | R/: `performance.R:181`, `performWhiteTest.R:232`, `validation.R:675`. Vignettes: none. inst/validation/: none. Tests: none. |
| `validateTestInputs()` | infrastructure | internal | R/: `generateHeteroRecommendations()` (`intelligent_recommendations.R:686`), the test factory (`test_factory.R:80`). Vignettes: none. inst/validation/: none. Tests: 21 lines. |
| `checkData()` | infrastructure | internal | R/: 16 lines in 7 files (`advancedDiagnostics.R`, `advancedRemediation.R`, `fitWLS.R`, `HeteroDiagnostic.R`, `intelligent_recommendations.R`, `reporting.R`, `runPanelTests.R`). Vignettes: none. inst/validation/: none. Tests: 9 lines. |
| `TestFactory` | infrastructure | internal | R/: only `test_factory.R:98`, which builds `test_factory`. Vignettes: none. inst/validation/: none. Tests: 2 lines. |
| `test_factory` | infrastructure | internal | R/: through its alias `.test_factory`, in `.run_registered_diagnostic()` (`performance.R:610-612`) and the dashboard (`dashboard.R:53`). Vignettes: none. inst/validation/: none. Tests: 10 lines. |
| `cachedTest()` | infrastructure | core API for now (decided); revisited at 1.0.0, once 0.16.0 bounds the caches |  |
| `clearTestCache()` | infrastructure | core API for now (decided); revisited at 1.0.0, once 0.16.0 bounds the caches |  |
| `clearAnalysisCache()` | infrastructure | core API for now (decided); revisited at 1.0.0, once 0.16.0 bounds the caches |  |
| `ht_enable_log_capture()` | infrastructure | internal | R/: none. Vignettes: none. inst/validation/: none. Tests: none. |
| `ht_log_history()` | infrastructure | internal | R/: none. Vignettes: none. inst/validation/: none. Tests: none. |
| `ht_clear_log_history()` | infrastructure | internal | R/: none. Vignettes: none. inst/validation/: none. Tests: none. |
| `rcalculateEffectSize()` | infrastructure | internal | R/: `performWhiteTestRobust()`, `performBPTestRobust()` (`robust_implementations.R:158,292`), `restimate_test_power()` (`bootstrap_methods.R:599`). Vignettes: none. inst/validation/: none. Tests: 4 lines. |
| `restimate_test_power()` | infrastructure | internal | R/: `performWhiteTestRobust()`, `performBPTestRobust()` (`robust_implementations.R:159,293`). Vignettes: none. inst/validation/: none. Tests: 3 lines. |
| `rrunAdvancedDiagnostics()` | runner | core API (decided); its name is still open, see the item on the `r`-prefixed exports |  |
| `rvalidate_against_reference()` | infrastructure | internal | R/: none. Vignettes: none. inst/validation/: none. Tests: 2 lines. The reference comparisons live in `tests/testthat/`. |
| `launchDiagnosticDashboard()` | dashboard and benchmarks | moved out (companion package not named yet; not deprecated in 0.14.0) | R/: none outside `dashboard.R`. Vignettes: none. inst/validation/: none. |
| `run_benchmark_suite()` | dashboard and benchmarks | moved out (companion package not named yet; not deprecated in 0.14.0) | R/: none outside `benchmarking_system.R`. Vignettes: none. inst/validation/: none. |
| `generate_benchmark_report()` | dashboard and benchmarks | moved out (companion package not named yet; not deprecated in 0.14.0) | R/: none outside `benchmarking_system.R`. Vignettes: none. inst/validation/: none. |

Notes on the verdicts:

- **`performStudentizedBPTest()` is provisional.** Item 2 first asserts that it
  agrees with `performKoenkerTest()` to `1e-8` over the designs of the full
  sweep, weighted, rank-deficient and `fitWLS()` fits included. Disagreement
  anywhere is a finding, not a rename, and is reported before anything else.
- **`test()`** warns and names `runDiagnostics(x$model, x$data, tests)`, which
  returns what `test()` returns. `summary.HeteroDiagnostic()` keeps its return
  value, so no value changes in 0.14.0.
- **Registry names.** `"student_bp"` and `"ncv"` stay registered and keep
  calling what they call now, so their output does not change. Each warns once
  per call of `runHeteroTests()` or `runDiagnostics()`, not once per group or
  bootstrap replicate, and goes in 1.0.0 with the function behind it.
- **The cache functions** (`cachedTest()`, `clearTestCache()`,
  `clearAnalysisCache()`) stay exported and are not deprecated in 0.14.0. They
  are revisited at 1.0.0, once 0.16.0 bounds the caches.
- **`rrunAdvancedDiagnostics()`, `runHeteroTestsParallel()` and
  `compareTestResults()`** stay core API. The name of
  `rrunAdvancedDiagnostics()` is still open under the item on the `r`-prefixed
  exports.
- **The simulation renames.** `simulate_size()` and `simulate_power()` join the
  documented snake_case family, since both simulate under the `sigma_*()`
  functions.
- **`bootstrapTestStatistic()`** takes a test function as its first argument,
  so it is not a `perform*Test(model, data, ...)`; the new name says what it
  does in the house camelCase.
- **The dashboard and the benchmark suite** are not deprecated in 0.14.0,
  because the companion package has no name yet. They call
  `plotDiagnosticSuiteEnhanced()`, the test factory and `performNCVTest()`,
  which change before the code moves.
- `analyzeMLResiduals()` and `analyseDatasetCharacteristics()` spell the verb
  two ways. Both are core API; the roadmap names no change for them.

### 0.15.0: make the interface match its description

These changes break existing calls. They have to happen before 1.0.0, not
after it.

- [x] **The `(model, data, ...)` convention.** Nine exported tests did not
  follow it:
  - no `data` argument: `performArchLMTest()`, `performMcLeodLiTest()`,
    `performCookWeisbergTest()`, `performNCVTest()`, `performSpearmanTest()`,
    `performSpreadLevelTest()`, `performDavidianCarrollTest()` and
    `performHarveyTest()`;
  - no `model` argument: `performBoxMTest(data, group)`.

  Decided on 2026-10-15 as proposed: the eight take `data = NULL` in second
  position, recovered from the model frame when omitted, and
  `performBoxMTest()` is the documented exception, since it tests the data
  rather than residuals. A call that passes the old second argument by
  position (`performArchLMTest(m, 3)`, `performCookWeisbergTest(m, ~x)`,
  `performHarveyTest(m, "fitted")`) is recognised by its type, works in
  0.15.0 and warns with the new call; `NEWS.md` has the table. No value
  changes: every old and new form returns what the old form returned in
  0.14.0, checked with `identical()` against 0.14.0.
- [x] **Argument names.** Harvey used `studentize` and `performBPTestRobust()`
  `studentized`. Standardised on `studentize`, which matches
  `lmtest::bptest()`; `studentized` is accepted with a deprecation warning in
  0.15.0, and the `robust_details$studentized` field of the result keeps its
  name. No value changes.
- [x] **The name of `rrunAdvancedDiagnostics()`.** Renamed
  `runAdvancedDiagnostics()` (decided 2026-10-15), the house convention for
  a runner. The old name warns with `.Deprecated()` in 0.15.x and goes in
  1.0.0 with the names deprecated in 0.14.0. No value changes.
- [ ] **Resampling defaults.** `B` is 499 for the wild bootstrap, 999 for rank
  permutation, and 1000 for the White bootstrap and
  `bootstrapTestStatistic()`. Under the `(1 + #) / (B + 1)` convention, a
  test at level `alpha` is exact only when `alpha * (B + 1)` is an integer, and
  1000 fails that at 5%. Standardise on 999, or on 499 where cost matters, and
  never use 1000.
- [ ] **Wild bootstrap multiplier** (Rademacher or Mammen). Settle it by a size
  study at small `n` with skewed errors, and record the result in
  `inst/validation/`.
- [ ] **Reproducibility under `parallel = TRUE`.** A search of `R/` finds no
  `clusterSetRNGStream()` and no L'Ecuyer set-up on the `mclapply()` and
  `parLapply()` paths, so `set.seed()` does not make parallel resampling
  results reproducible. Fix that, and add a test that compares two seeded
  parallel runs.
- [ ] **The default battery.** `runHeteroTests()`, `test()`/`summary()` on
  `HeteroDiagnostic`, the advanced diagnostics and the dashboard all default to
  `c("white", "breusch_pagan")`, and `"breusch_pagan"` is the classical
  statistic. The package's own sweep shows it rejecting 28.7% of the time under
  a `t5` null, against 4.8% for Koenker. Make the studentized form the default.
  The change alters default output, so it gets its own `NEWS.md` section.
- [x] **Return contract.** Document the fields every `perform*Test()` returns,
  both the `htest` core and the package's additions, on one help page. Check
  every exported test against it in a single parametrised test. This also
  retires the hand-maintained printer and metadata drift listed under the old
  technical debt. Done: `?test_results` and `test-return-contract.R`. Five
  tests gained the name `df` of their `parameter` and `performBoxMTest()` its
  `data.name`; no number changed.
- [ ] **Uniform input validation** across all exported tests. The README lists
  its absence as a known limitation.

### 0.16.0: statistics evidence, dependencies and internals (no API change)

- [ ] **Re-run the full sweep at `N_MC = 5000`,** so that the release gate in
  `inst/validation/README.md` (`[0.042, 0.058]`) applies to every exported
  test, not only Passes A and B. At 400 replications the Monte Carlo standard
  error is 1.1%, so the Gaussian sizes between 0.068 and 0.072 (Cook–Weisberg
  and NCV, Spearman, Levene) cannot be told apart from noise. Pass A, at 5000
  replications, put Cook–Weisberg at 0.044.
- [ ] **`performBoxMTest()`** is conservative, with size 0.012. It uses the
  chi-square approximation. Compare it against Box's F approximation, then
  either switch or document the conservatism on the help page.
- [ ] **`performSzroeterTest()` on a weighted fit.** In
  `inst/validation/harrison-mccabe-size-power.R`, on a correctly weighted fit
  with Gaussian errors, it rejects 0.067 of the time at 40 observations,
  outside the release gate, and 0.056 and 0.055 at 100 and 400; without
  weights it is inside the gate at all three sizes. Its null mean and
  variance, `(n + 1) / 2` and `(n^2 - 1) / (6 n)`, do not depend on the
  design, and in that weighted design the start of the ordering has the larger
  leverages. Either take its null distribution from the weighted design, as
  `performHarrisonMcCabeTest()` does, or document the restriction on the help
  page.
- [ ] **Defects found during the 0.12.0 and 0.13.0 work**, left for this
  release so that 0.14.0 changes no value. Each fix that changes a value is
  announced in bold in `NEWS.md`.
  - `fitWLS()` keeps the original weights in the model frame of its result, so
    `performStudentizedBPTest()` on a `fitWLS()` fit reads stale weights: 14.81,
    against 1.73 for the equivalent `lm(weights = estimated)` fit. Suggested
    fix: `mf[["(weights)"]] <- w` in `rweighted_refit()`. Changes a value.
  - `performWildBootstrapTest()` and `performWhiteTestBootstrap()` accept a
    non-Gaussian `glm` and return a meaningless result. They should refuse it,
    as `bootstrapTestStatistic()` does. Such a call fails instead.
  - The single tests and `performWhiteTestBootstrap()` match a reordered tibble
    or `data.table` to the fit by position, through
    `prepare_model_data_for_test()`. Changes a value for such data.
  - The caches of `runHeteroTests(use_cache = TRUE)` (about 33 KB per new
    model) and `cachedTest()` (about 25 KB per call) grow without bound, as the
    validation cache did before #97. No value changes.
  - A multivariate `lm`, `cbind(y, y2) ~ ...`, fails with a coercion error
    instead of a clear refusal. No value changes.
  - The unexported `performWhiteTestEnhanced(bootstrap = TRUE)`, through
    `bootstrap_white_test()`, takes the log of a log response twice, drops an
    offset given as an argument and takes rows by position. Remove it or fix
    it. No exported value changes.
  - About 0.8 KB per test call on a new model still persists after the
    validation cache is cleared, as seen in long simulations. No value changes.
  - On a rank-deficient fit (`z2 = 2 * z`, the design in
    `tests/testthat/test-koenker-studentized.R`) both `performKoenkerTest()`
    and `performStudentizedBPTest()` refuse with "Model coefficients must be
    finite…", where `lmtest::bptest()` returns a statistic (20.87, df 2).
    Decide whether to compute on the estimable subspace or keep the refusal
    with a clearer message.
- [ ] **Dependencies.**
  - Move `curl` to `Suggests`: only `downloadTeachingData()` uses it.
  - `R6` goes if `TestFactory` leaves the API, since it is the only user.
  - `Suggests` shrinks with the companion split.
  - Settle whether `digest` belongs in `Imports` or `Suggests`, depending on
    whether caching stays exported.
- [ ] **Pass `R CMD check` with `_R_CHECK_DEPENDS_ONLY_=true`.** With 39
  packages in `Suggests`, every test, example and vignette that uses one must
  degrade gracefully. Audit the `skip_if_not_installed()` coverage; 20 of the
  69 test files currently use it.
- [ ] **The R floor.** `DESCRIPTION` claims R >= 4.1, but CI tests only devel,
  release and oldrel-1. Either add R 4.1 to the matrix or raise the floor to
  what is tested.
- [ ] Replace non-ASCII characters in `R/` with escapes (89 lines across 27
  files).
- [ ] Split `R/validation.R` (1,441 lines) into cache, result-type,
  assumption-checking and requirement-dispatch units, and factor out the
  repeated boilerplate: scalar validators, model and data preparation, and
  intercept stripping.
- [ ] **Retire `TODO.md`, `SUGGESTIONS.md` and `cran.md`.** The open items in
  the first two all concern `setup.sh` and bootstrapping `renv`, and `cran.md`
  is a generic guide. With CRAN now the install route, decide whether
  `setup.sh`, `renv.lock` and the `Dockerfile` remain supported. If they do,
  complete `renv.lock` against `Imports`/`Suggests` so the Docker build uses it
  instead of `install_local()`. If they do not, remove them.
- [ ] Keep the CRAN test run short. Long simulations run under `skip_on_cran()`,
  and the full versions run on CI.

### 1.0.0: freeze

- [ ] Remove everything deprecated in 0.14.0 and 0.15.0, or turn it into
  `.Defunct()` stubs that name the replacement.
- [ ] `test-public-api.R` asserts the frozen export list exactly.
- [ ] pkgdown reference index grouped by the frozen API: tests by family
  (auxiliary regression, group-wise, rank-based, time series, panel and
  spatial, resampling), remediation, simulation, plotting and reporting.
- [ ] **Vignettes.** There are eight, and `tutorial`, `using_heteroTests` and
  `comprehensive_guide` overlap. Consolidate them into a short set that does
  not:
  - getting started;
  - choosing a test, turning the `t5` and power tables into advice;
  - remediation;
  - theory and references.
- [ ] Publish the stability policy from [What 1.0.0 means](#what-100-means) in
  the README and on the pkgdown site.
- [ ] `NEWS.md` for 1.0.0 with the complete old → new migration table.
- [ ] Full `--as-cran` on every CI flavour, plus win-builder, then submit.
- [ ] Update the R Journal manuscript, kept in its own repository, to the 1.0
  API and submit it.
  Submitting on the frozen API avoids publishing function names that are about
  to be deprecated.

## After 1.0.0

- First-class panel and spatial heteroscedasticity workflows.
- Helpers that bridge detection to conditional-variance modelling (ARCH/GARCH)
  for the time-series path.
- An experiment/report artefact (model-card style) summarising a diagnostic run.
- Streaming for more tests where it is meaningful, under a single
  `chunk_threshold_mb` policy.
- Stronger feasible-WLS and auto-transform helpers: iterated feasible GLS, an
  estimator of the additive variance function that keeps its fitted variances
  positive, and a Box–Cox search.
- A form of `performVarianceFormTest()` that does not need the standardized
  errors to have a constant fourth moment, following the robust statistic of
  Wooldridge (1991).
- A single parametrised accuracy-validation test, with reference coverage
  broadened to `skedastic` and `sandwich`.
- The companion package for the dashboard and the benchmark suite (decided on
  2026-10-14; not yet named).

## Candidate tests

None of these is scheduled. Each would be a new export, so each has to clear
the release gate in `inst/validation/README.md` before it ships: agreement to
`1e-8` with an established implementation or with a reconstruction of the
primary reference, simulated size inside the gate, and power against the
alternative it is built for.

The list is arranged by where the test can be found today, because that
decides both what there is to validate against and what the package would add.
Availability was checked on 2026-10-09 by searching CRAN, the R documentation
sites and the web for each test by name. "No R implementation found" reports
that search. It is not proof that none exists.

### No R implementation found

A first implementation in R. Where another system has one, it is the
reference; where none does, the guard is a reconstruction of the primary
reference and simulated size.

- **Modified Wald test for groupwise heteroscedasticity** in a fixed-effects
  panel (Greene 2000; Baum 2001). The panel tests cover an individual effect
  and cross-sectional dependence, not unequal variances across units.
  Reference implementation: Stata's `xttest3`. The Python package `panelbox`
  (1.0.2) has a function of that name that computes a different statistic, a
  likelihood-ratio form. A second search on 2026-10-10 found no R
  implementation either. **Measured for 0.13.0 and not adopted: it does not
  hold its level.** In the designs of
  `inst/validation/modified-wald-size-power.R`, at 5000 replications, the test
  as `xttest3` computes it rejects a true null hypothesis at the 5% level this
  often:

  | Errors | Units | T = 5 | T = 10 | T = 30 | T = 100 |
  | --- | ---: | ---: | ---: | ---: | ---: |
  | Gaussian | 10 | 0.927 | 0.681 | 0.282 | 0.100 |
  | Gaussian | 30 | 1.000 | 0.966 | 0.584 | 0.183 |
  | Gaussian | 100 | 1.000 | 1.000 | 0.928 | 0.361 |
  | t5 | 10 | 0.966 | 0.876 | 0.621 | 0.367 |
  | t5 | 30 | 1.000 | 0.999 | 0.944 | 0.717 |
  | t5 | 100 | 1.000 | 1.000 | 1.000 | 0.982 |

  Two unbalanced panels are at 0.809 and 1.000 under Gaussian errors. Each
  unit's term is divided by a variance estimated from the same few residuals,
  and the excess grows with the number of units. In long panels the error
  reverses, because the statistic tends to a chi-squared with one degree of
  freedom fewer than its reference: with 2 units and 1000 periods it rejects
  0.014 of the time. Three of the study's 40 null cells fall inside the
  release gate, all at 300 or 1000 periods, where the two errors happen to
  cancel, and under `t5` errors it still rejects 0.129 of the time with 10
  units and 0.235 with 30 at 1000 periods. Before a test of this hypothesis
  can ship it needs a different variance estimate or a small-sample
  correction, which makes it a different test. Two leads, neither part of the
  study: Bartlett's test on the within residuals held its level under
  Gaussian errors in an exploratory run but, being a normal-theory test, not
  under `t5` errors; and Juhl and Sosa-Escudero (2014, *Journal of
  Econometrics* 178, 484-494) derive studentized tests for heteroscedasticity
  after fixed-effects estimation with the number of periods fixed, whose fit
  to this hypothesis has not been checked. Greene's page 598, which Baum
  follows, could not be obtained, so whether Greene's own form differs from
  Baum's was not checked.
- **A joint LM test of homoscedasticity in a one-way error-component model**
  (Baltagi, Bresson and Pirotte 2006), against heteroscedasticity in the
  individual effect and in the remainder error together.
- **Pagan–Hall (1983)**, the test of constant variance after
  instrumental-variables estimation, where Breusch–Pagan and White are valid
  only under further conditions. Reference implementation: Stata's
  `ivhettest`. The package accepts no instrumental-variables fit today, so
  this one starts with the input layer.
- **Kelejian–Robinson (1998)**, a joint test for spatial autocorrelation and
  heteroscedasticity. `sphet::kpjtest()` is a different test by Kelejian and
  Piras.
- **Newey–Powell (1987)**, a test built on asymmetric least squares:
  under constant variance the expectile regressions have equal slopes.
  `expectreg` fits expectiles and has no such test.
- **Tests in nonparametric regression**, which have power against any smooth
  departure from constant variance and need no variance regressors to be
  named: Eubank and Thomas (1993), Dette and Munk (1998), Zheng (2009), and
  Chown and Müller (2018). The cost is a bandwidth or a difference sequence
  to choose.
- **A consistent test of a variance function**: Dette, Neumeyer and Van
  Keilegom (2007), Wang and Zhou (2007), and Samarakoon and Song (2011).
  `performVarianceFormTest()` has power against the terms it is given; these
  have it against any departure, at the cost of smoothing. **Stage 1 of the
  first measured for 0.13.0, and not exported: as defined, it does not hold
  its level.** The 2007 article could not be obtained; a prototype in plain R
  follows the authors' submitted manuscript of 2005, whose Tables 4.1 and 4.5
  it reproduces in level and to within a few points in power. In the designs
  of `inst/validation/variance-form-size-power.R`, with the variance function
  in one variable and the mean a linear model, the Cramér-von Mises statistic
  is inside the release gate in 4 of its 12 Gaussian null cells at 5000
  replications, all four with constant variance at 150 or 400 observations;
  it rejects a true exponential or power variance function 0.019 to 0.036 of
  the time at 50 to 400 observations. The bootstrap draws its data from the
  least-squares fit of the variance function, on which the statistic's null
  distribution depends: drawn from the true shape, on the same samples, three
  of the four such cells come to 0.047 to 0.051. Against a departure the
  added terms of `performVarianceFormTest()` do not span it has the power
  that test lacks, 0.750 against 0.089 at 400 observations; against the other
  alternatives it has less. Design note, prototype and evidence:
  `inst/validation/variance-form-dnvk-design.md` and the
  `variance-form-dnvk-*` files beside it. What to do next is under *Decisions
  needed*. Wang and Zhou (2007) assume a parametric mean, which is this
  package's setting; Samarakoon and Song (2011) could not be obtained.
  **Stage 1 of Wang and Zhou (2007) is parked** on the branch
  `feat/wang-zhou-stage1`, to be resumed after 1.0.0: pass 1 of its size and
  power study is done, and the Gaussian null cells at 5000 replications that
  decide it have not been run. The branch's `wip/wang-zhou-stage1/README.md`
  says how to resume.
- **Bai, Pan and Yin (2018)**, a test of homoscedasticity when the number of
  regressors grows with the sample. Li–Yao, below, is a different test for the
  same setting.
- **A design-based test of constant variance for survey data.** The tests
  assume an independent sample with equal weights, and
  `runSurveyHeteroTests()` rejects a homoscedastic model 37% to 100% of the
  time when selection depends on the response or the sample is clustered. The
  natural candidates regress the squared residuals of a `survey::svyglm()` fit
  on the variance regressors using the design, as a Wald test
  (`survey::regTermTest()`) or as a Rao score test. Both were measured in
  `inst/validation/survey-designs-size.R` and neither clears the gate: 6% to
  25% under the null at 300 and 1000 observations when the weights vary with
  the regressor. The variance estimate needs a small-sample correction first.

### In another R package, not in this one

These add no test to R. They add it to the common interface, and each comes
with a reference implementation to agree with.

- **Sign and size bias tests** (Engle and Ng 1993): whether the squared
  residuals respond differently to negative and to positive lagged residuals.
  The time-series tests detect ARCH effects and say nothing about asymmetry.
  `rugarch::signbias()` computes the tests for a fitted GARCH model.
- **A break in the variance at an unknown date**: the CUSUM of squares test
  (Brown, Durbin and Evans 1975), the statistic of Inclán and Tiao (1994), or
  the kappa tests of Sansó, Aragó and Carrion (2004), which stay valid under
  heavy tails and conditional heteroscedasticity. Goldfeld–Quandt and Szroeter
  need the ordering and the split to be chosen in advance. Reference
  implementation for the last: `micss::kappa_test()`.
- **Simonoff–Tsai (1994) and Verbyla (1993).** Versions of the Cook–Weisberg
  score test built on modified profile likelihood and on residual maximum
  likelihood, for small samples and high-leverage designs. Reference
  implementations: `skedastic::simonoff_tsai()` and `skedastic::verbyla()`.
- **Bickel (1978) and Anscombe (1961).** Tests of the variance against the
  fitted values. Bickel's is the robust one. Reference implementations:
  `skedastic::bickel()` and `skedastic::anscombe()`.
- **Li–Yao (2019)** for regressions with many regressors.
  `performHighDimensionalTest()` regresses the squared residuals on principal
  components and cites no heteroscedasticity test as its source. Reference
  implementation: `skedastic::li_yao()`.
- **A spatial Breusch–Pagan test** (Anselin 1988) for fitted spatial models.
  `performSpatialHeteroTest()` applies Moran's I to the squared residuals,
  which detects variance that clusters in space and does not test it against
  regressors. Reference implementation: `spatialreg::bptest.Sarlm()`.
- **The rest of `skedastic`.** Of its 25 tests, the package has counterparts
  for eight. Besides those named above, `skedastic` provides `bamset()`,
  `carapeto_holt()`, `diblasi_bowman()`, `dufour_etal()`, `evans_king()`,
  `godfrey_orme()`, `honda()`, `horn()`, `rackauskas_zuokas()`,
  `wilcox_keselman()`, `yuce()` and `zhou_etal()`. None is a priority by
  itself; each is a candidate when a user asks for it.

### Order of work

The first group is where the package can contribute something R does not have,
which is also what a reader of the article asks of it. Within that group:

1. The modified Wald test. The panel functions exist, and it is the test
   Stata users look for under the name `xttest3`. Measured for 0.13.0 and
   not adopted, because it does not hold its level (above).
2. The Dette, Neumeyer and Van Keilegom test. It removes the main limitation
   of `performVarianceFormTest()`, and is the most work of the two. Stage 1
   done for 0.13.0: as the manuscript defines it, it does not hold its level
   when the variance is not constant (above).

### Done

- **A Glejser test that is valid under skewed errors** (Im 2000; Machado and
  Santos Silva 2000), in 0.13.0, as `performGlejserTest(robust = TRUE)`.
  In the designs of `inst/validation/glejser-skewed-errors.R` the uncorrected
  statistic rejects a true null hypothesis 9% to 18% of the time at the 5%
  level when the errors are skewed, at every sample size, and
  `skedastic::glejser()` uses the uncorrected absolute residuals as well. The
  default is still Glejser's statistic; whether it should be is under
  *Decisions needed*.
- **Harrison–McCabe (1979)**, in 0.13.0, as `performHarrisonMcCabeTest()`,
  beside Goldfeld–Quandt and Szroeter. The 1979 article was not read; the
  definition rests on Harrison (1981), by its first author, whose published
  exact critical values the function reproduces, and on `lmtest::hmctest()`,
  with which the statistic agrees to `1e-8`. The p-value is exact under
  normal errors given the regressors. Neither reference computes that p-value:
  `lmtest::hmctest()` simulates it from a regression on a constant alone, and
  `skedastic::harrison_mccabe()` 2.0.3 drops repeated eigenvalues and, with
  `deflator`, leaves the design in the order of the rows. In the designs of
  `inst/validation/harrison-mccabe-size-power.R` it holds its level under
  Gaussian errors, rejects 12% to 16% of the time under `t5` errors as the
  other two ordered tests do, and has less power than either.

## Decisions needed

| Question | Proposal | Needed by |
| --- | --- | --- |
| Default battery | White + Koenker | 0.15.0 |
| Default of `robust` in `performGlejserTest()` | `TRUE`. From 150 observations the corrected statistic holds its level under every error distribution measured, where the default is at 9% to 18% when the errors are skewed. At 50 observations it is at 5.7% to 6.6% under skewed errors and costs a point or two of power under Gaussian errors. It changes the values the function reports, so it belongs in a minor release that says so | 0.15.0 |
| Wild bootstrap multiplier and `B` | Decide by simulation; `B = 999` | 0.15.0 |
| Bootstrap tests on a `fitWLS()` fit | Add an argument to `performWildBootstrapTest()`, `performWhiteTestBootstrap()` and `bootstrapTestStatistic()` that runs `fitWLS()` again in each replicate, on by default when the model records a variance function. Holding the weights fixed, the wild bootstrap rejects 0.086 and 0.105 at 150 and 600 observations; estimating them again, 0.060 and 0.051; with the true weights, 0.054 and 0.048. Postponed on 2026-10-12: 0.13.0 accepts a weighted fit in these functions and holds the weights of a `fitWLS()` fit fixed in each replicate, so a default that estimates them again changes values 0.13.0 returned, and has to be announced in bold in a minor release. Moved from 0.14.0 on 2026-10-14: the new default changes a default and the values these functions return, which belongs with the other default changes of 0.15.0, not in 0.14.0, where no value may change | 0.15.0 |
| Ties in the ordering of `performSzroeterTest()` | Average the ranks within ties and adjust the null variance for them. Today ties keep the order of the rows the model was fitted to, which holds the level but makes the statistic depend on that order when `order_by` is discrete, and for the registered test on a model whose design rows repeat, as with factors, binary or other discrete regressors. It changes the values the function returns | 0.16.0 |
| Supported R floor | Test the floor in CI, or raise it to oldrel-1 | 0.16.0 |
| The name of the companion package for the dashboard and the benchmark suite | None recorded. Once it has one, `launchDiagnosticDashboard()`, `run_benchmark_suite()` and `generate_benchmark_report()` warn here for a minor release, naming it, and the code moves | 0.16.0 |
| A consistent test of the variance function after stage 1 | Do not ship the Dette, Neumeyer and Van Keilegom test as defined: its prototype rejects a true non-constant variance function 0.019 to 0.036 of the time. Choose between a new stage 1 for a modified procedure (a better estimate of the variance function for the bootstrap data, or a studentized statistic) and one for Wang and Zhou (2007), and compare the published article with the manuscript first. If a test ships, a function of its own rather than `performVarianceFormTest(against = "any")` | Before any stage 2 |
| `setup.sh` / `renv` / Docker | Keep Docker for reproducibility; drop `setup.sh` now CRAN is the install route | 0.16.0 |
