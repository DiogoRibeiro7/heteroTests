# heteroTests Roadmap

This document tracks where the package is going. It is updated alongside
meaningful code changes. Milestones are ordered, not dated. The history of what
changed and why lives in `NEWS.md`; the statistical evidence lives in
`inst/validation/`.

## Where things stand (0.12.0)

- **On CRAN.** 0.11.2 was accepted in September 2026, after one review round on
  0.11.1. 0.12.0 has not been submitted.
- **The validation matrix is complete** for every exported `perform*Test()`.
  The sweep found six broken procedures, all corrected or withdrawn, and
  twenty-five of the twenty-six heteroscedasticity tests hold their nominal
  level (`inst/validation/README.md`).
- **Weighted fits were outside that matrix, and wrong.** Every design in it
  fitted an unweighted model. On a weighted fit the tests read the raw
  residuals, whose variance differs across observations by assumption, and
  rejected a correctly weighted model every time. 0.12.0 corrects that and
  adds weighted designs to the matrix (`inst/validation/weighted-fits-size.R`).
- **Reference equivalence** is asserted to `1e-8` against `lmtest`, `car` and
  `plm` wherever a reference implementation exists. `lmtest::bptest()` is not
  a reference for weighted fits: it tests the raw residuals.
- **The public surface has not been reviewed as a whole.** The package exports
  127 objects. They accumulated feature by feature, and they include
  duplicates, internal helpers and several naming schemes. That surface, not
  the statistics, is what stands between 0.12.0 and 1.0.0.

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
weighted fits and closed it again, so the API is next.

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
  tests do not yet; see 0.14.0.)
- **Scale and robustness as first-class concerns**: streaming implementations
  for large data, resampling and robust variants for small or non-normal
  samples.

## Milestones

### 0.11.x: CRAN maintenance (now)

- [ ] Watch the CRAN check results across all flavours and fix anything flagged
  within the deadline CRAN sets.
- [ ] README: install from CRAN first (`install.packages("heteroTests")`), with
  GitHub as the development route; add a CRAN badge and use the canonical URL
  `https://CRAN.R-project.org/package=heteroTests`.
- [ ] The README feature list is out of date. It still advertises an HC0–HC4
  covariance test and Cameron–Trivedi, both removed in 0.8.0. Bring it in line
  with the exports, and extend `test-documentation-consistency.R` so a README
  or vignette that names a removed function fails.
- [ ] Documentation drift: the `performStudentizedBPTest()` help page describes
  `performKoenkerTest()` as "the absolute-residual variant", which predates the
  Breusch–Pagan/Koenker correction. Koenker is the studentized `n R^2` form.
- [ ] Check that the Zenodo record picked up 0.11.2, and cite the CRAN release
  in `inst/CITATION` and `CITATION.cff`.
- [ ] Close tracking issue #38 as On CRAN.
- [ ] Reinstate a release gate. `R CMD check --as-cran` can stay weekly rather
  than per merge, but it must pass in full on the tagged commit (vignettes and
  `--run-donttest` included) before any submission, alongside win-builder
  (R-devel).

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

- [ ] **Resampling on weighted fits.** `performWildBootstrapTest()`,
  `performWhiteTestBootstrap()`, `rbootstrap_test_statistic()` and
  `performQuantileRegressionTest()` refuse a weighted fit, because they refit
  without the weights. Resampling the Pearson residuals and refitting with the
  weights would lift the refusal for the first three.
- [ ] **`runSurveyHeteroTests()` drops the survey weights.** It calls
  `stats::lm(formula, data = data, weights = weights)`, where `weights` is a
  local variable. `lm()` looks the name up in `data` and then in the
  environment of the formula, finds `stats::weights`, fails, and the
  `tryCatch()` refits without weights. The documented survey-weighted fit
  does not happen. Fixing the scoping raises a second question, which
  residuals a test should read under sampling weights: they are not inverse
  error variances, so the Pearson residuals are not the answer there.
- [ ] **`.ht_fit_from_formula()` has the same scoping fault** and always fails
  with `invalid type (closure) for variable '(weights)'`, so the per-group
  refits built from an `lm`, `glm` or parsnip fit cannot run. A fix also has
  to subset the weights to each group.
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

### 0.13.0: review the public surface (deprecate, remove nothing)

Goal: every one of the 127 exports gets a verdict, which is one of **core API**,
**renamed**, **internal** or **moved out**. Nothing is removed in this release.
Everything that will go warns.

- [ ] **Duplicate exports.**
  - `performBreuschPaganTest()` is `performBPTest()`, assigned at
    `R/performBPTest.R:232`.
  - `performStudentizedBPTest()` is a separate implementation that matches
    `performKoenkerTest()` in every column of the full sweep. Confirm the
    statistics agree to `1e-8`, then keep one.
  - `performCookWeisbergTest()` delegates to `performNCVTest()` with the fitted
    values as the variance regressor. Keep one entry point for the
    Cook–Weisberg score test, with `var_formula` as an optional argument.
  - `*Enhanced` pairs: `checkModel()`/`checkModelEnhanced()`,
    `plotResidualsFitted()`/`plotResidualsFittedEnhanced()` and
    `plotDiagnosticSuite()`/`plotDiagnosticSuiteEnhanced()`. Merge each
    improvement into the base name.
- [ ] **Internal machinery that is exported:** `std_error()`, `std_warning()`,
  `validateTestInputs()`, `checkData()`, `TestFactory`/`test_factory()`, the
  cache functions (`cachedTest()`, `clearTestCache()`, `clearAnalysisCache()`)
  and the `ht_*` logging functions. Unexport them unless a user-facing reason
  exists; for logging, keep at most `ht_set_log_level()`.
- [ ] **The five `r`-prefixed exports** (`rbootstrap_test_statistic()`,
  `rcalculateEffectSize()`, `restimate_test_power()`,
  `rrunAdvancedDiagnostics()` and `rvalidate_against_reference()`) follow no
  naming scheme in the package. Rename each to the house convention or
  unexport it.
- [ ] **`test()` is a generic with a very common name.** Attached after
  `devtools`, it masks `devtools::test()`. Replace it with a specific name or
  fold it into `summary.HeteroDiagnostic()`.
- [ ] **Naming convention.** camelCase dominates (`performXTest()`, `fitWLS()`,
  `plotX()`). Either declare the simulation family (`simulate_hetero()`,
  `sigma_*()`) a deliberate snake_case family or move it to camelCase. The
  one-offs (`run_benchmark_suite()`, `generate_benchmark_report()`,
  `simulate_type_I_errors()`, `simulate_power_analysis()`) are renamed either
  way.
- [ ] **Scope.** Decide what stays in the core package (see
  [Decisions](#decisions-needed)). The dashboard (`R/dashboard.R`, 698 lines)
  and the benchmark suite (`R/benchmarking_system.R`, 798 lines) are the
  natural candidates for a companion package. Between them they account for
  most of the optional `Suggests` (`shiny`, `DT`, `plotly`, `htmlwidgets`,
  `bench`).
- [ ] **Related model checks** (`performRESETTest()`, `performVIFDiagnostic()`,
  `performInfluenceDiagnostics()`, `performScatterDiagnostic()`) are not
  heteroscedasticity tests. Keep them, grouped in the reference index as model
  checks, so they are not mistaken for tests of the variance.
- [ ] **Mechanism.** Use base `.Deprecated()`, so no new dependency is needed.
  Each call warns and names its replacement. `NEWS.md` carries an old → new
  mapping table.
- [ ] **Exit criterion.** An export inventory with a verdict for every one of
  the 127 exports, and `test-public-api.R` extended to assert the planned 1.0
  export list, so that an accidental new export fails CI.

### 0.14.0: make the interface match its description

These changes break existing calls. They have to happen before 1.0.0, not
after it.

- [ ] **The `(model, data, ...)` convention.** The README says every test
  follows it, but nine exported tests do not:
  - no `data` argument: `performArchLMTest()`, `performMcLeodLiTest()`,
    `performCookWeisbergTest()`, `performNCVTest()`, `performSpearmanTest()`,
    `performSpreadLevelTest()`, `performDavidianCarrollTest()` and
    `performHarveyTest()`;
  - no `model` argument: `performBoxMTest(data, group)`.

  Proposal: `data = NULL` in second position everywhere, recovered from the
  model frame when omitted, which `runHeteroTests()` already does through
  `.ht_prepare_model()`, and which `performVarianceFormTest()` and `fitWLS()`
  have followed since 0.12.0. This breaks positional calls such as
  `performArchLMTest(m, 3)`, which is the reason it cannot wait until after
  1.0. Where it is feasible, detect the old positional form and warn for one
  release. `performBoxMTest()` may stay the documented exception, since it
  tests the data rather than residuals.
- [ ] **Argument names.** Harvey uses `studentize` and `performBPTestRobust()`
  uses `studentized`. Standardise on `studentize`, which matches
  `lmtest::bptest()`.
- [ ] **Resampling defaults.** `B` is 499 for the wild bootstrap, 999 for rank
  permutation, and 1000 for the White bootstrap and
  `rbootstrap_test_statistic()`. Under the `(1 + #) / (B + 1)` convention, a
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
- [ ] **Return contract.** Document the fields every `perform*Test()` returns,
  both the `htest` core and the package's additions, on one help page. Check
  every exported test against it in a single parametrised test. This also
  retires the hand-maintained printer and metadata drift listed under the old
  technical debt.
- [ ] **Uniform input validation** across all exported tests. The README lists
  its absence as a known limitation.

### 0.15.0: statistics evidence, dependencies and internals (no API change)

- [ ] **Re-run the full sweep at `N_MC = 5000`,** so that the release gate in
  `inst/validation/README.md` (`[0.042, 0.058]`) applies to every exported
  test, not only Passes A and B. At 400 replications the Monte Carlo standard
  error is 1.1%, so the Gaussian sizes between 0.068 and 0.072 (Cook–Weisberg
  and NCV, Spearman, Levene) cannot be told apart from noise. Pass A, at 5000
  replications, put Cook–Weisberg at 0.044.
- [ ] **`performBoxMTest()`** is conservative, with size 0.012. It uses the
  chi-square approximation. Compare it against Box's F approximation, then
  either switch or document the conservatism on the help page.
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

- [ ] Remove everything deprecated in 0.13.0 and 0.14.0, or turn it into
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
- [ ] Update the R Journal manuscript in `paper/` to the 1.0 API and submit it.
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
- The companion package, if the scope decision goes that way.

## Candidate tests

None of these is scheduled. Each would be a new export, so each has to clear
the release gate in `inst/validation/README.md` before it ships: agreement to
`1e-8` with an established implementation or with a reconstruction of the
primary reference, simulated size inside the gate, and power against the
alternative it is built for. The first group covers questions the package
cannot answer yet. The second refines tests it already has.

Questions the package cannot answer yet:

- **Harrison–McCabe (1979).** The share of the residual sum of squares that
  falls in the first part of an ordered sample. Reference implementations:
  `lmtest::hmctest()` and `skedastic::harrison_mccabe()`.
- **A Glejser test that is valid under skewed errors** (Im 2000; Machado and
  Santos Silva 2000). The help page of `performGlejserTest()` already warns
  that the uncorrected statistic is not, and cites Im. No reference
  implementation has been identified, so the guard is simulated size under a
  skewed null.
- **Modified Wald test for groupwise heteroscedasticity** in a fixed-effects
  panel (Greene 2000; Baum 2001). The panel tests cover an individual effect
  and cross-sectional dependence, not unequal variances across units.
  Reference implementation: Stata's `xttest3`.
- **Sign and size bias tests** (Engle and Ng 1993): whether the squared
  residuals respond differently to negative and to positive lagged residuals.
  The time-series tests detect ARCH effects and say nothing about asymmetry.
  `rugarch::signbias()` computes the tests for a fitted GARCH model.
- **A break in the variance at an unknown date**: the CUSUM of squares test
  (Brown, Durbin and Evans 1975) or the statistic of Inclán and Tiao (1994).
  Goldfeld–Quandt and Szroeter need the ordering and the split to be chosen in
  advance.

Refinements of tests the package already has:

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
- **A consistent test of a variance function** (Dette, Neumeyer and Van
  Keilegom 2007). `performVarianceFormTest()` has power against the terms it
  is given; this one has it against any departure, at the cost of smoothing.

## Decisions needed

| Question | Proposal | Needed by |
| --- | --- | --- |
| Does the dashboard/benchmark/recommendation layer stay in core? | Move the dashboard and benchmark suite to a companion package; keep the recommendation engine, with its return format frozen | 0.13.0 |
| One name for the Cook–Weisberg score test | Keep `performCookWeisbergTest()` with optional `var_formula`; deprecate `performNCVTest()` | 0.13.0 |
| Replacement for the `test()` generic | Fold into `summary.HeteroDiagnostic()` | 0.13.0 |
| Naming of the simulation family | Keep `simulate_*()`/`sigma_*()` as a documented snake_case family; rename the one-offs | 0.13.0 |
| `data` argument on every test | `data = NULL` in second position; `performBoxMTest()` the documented exception | 0.14.0 |
| Default battery | White + Koenker | 0.14.0 |
| Wild bootstrap multiplier and `B` | Decide by simulation; `B = 999` | 0.14.0 |
| Supported R floor | Test the floor in CI, or raise it to oldrel-1 | 0.15.0 |
| `setup.sh` / `renv` / Docker | Keep Docker for reproducibility; drop `setup.sh` now CRAN is the install route | 0.15.0 |
