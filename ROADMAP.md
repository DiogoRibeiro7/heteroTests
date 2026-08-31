# heteroTests Roadmap

This document tracks the direction of the package: what is done, what is in
progress, and what is planned. It is updated alongside meaningful code changes.
Dates are omitted in favour of milestone ordering.

## Guiding principles

- **Statistical correctness first.** Every test must reproduce an established
  reference (or a defensible derivation) and be covered by a test that checks
  *behaviour* (size and power), not just object structure.
- **One consistent interface.** All diagnostics return base-R `htest` objects and
  follow the `perform*Test(model, data, ...)` convention.
- **Scale and robustness as first-class concerns**, not afterthoughts: streaming
  implementations for large data, resampling/robust variants for small or
  non-normal samples.

## Completed

### Statistical correctness
- [x] Corrected the Breusch–Pagan / Koenker swap: `performBPTest()` is the
  classical statistic (`lmtest::bptest(studentize = FALSE)`), `performKoenkerTest()`
  the studentized `n R^2` form (`studentize = TRUE`); validated to machine
  precision and in the reference-comparison tests.
- [x] Reimplemented `performWildBootstrapTest()` as a **null-imposed** bootstrap so
  it controls size and actually has power; added size/power regression tests.
- [x] Kept the streaming variants algebraically identical to their exact
  counterparts (covered by `test-streaming.R`).
- [x] Resolved a duplicate `%||%` operator collision that could abort
  `runHeteroTests()` on multi-column data.
- [x] **Pass A of the statistical validation matrix** (classical regression
  diagnostics). Each test was checked definition -> reference -> implementation ->
  numerical equivalence -> size/power. Reference equivalence is asserted in
  `tests/testthat/test-pass-a-reference.R`; size and power are measured by
  `inst/validation/pass-a-size-power.R` and recorded in
  `inst/validation/pass-a-size-power.csv`. Corrections: `performSzroeterTest()`
  (wrong standardisation, zero power), `performNCVTest()` (was a Glejser-type
  t-test, not the score test it documented), `performCookWeisbergTest()` (returned
  the Koenker statistic), `performHarveyTest()` (non-standard auxiliary design and
  statistic). `performArchLMTest()` and `performMcLeodLiTest()` passed unchanged.
- [x] Unified input validation across the Pass A tests; `performNCVTest()` and
  `performCookWeisbergTest()` no longer bypass the shared framework. This also
  removed a masking effect in which the unvalidated NCV test succeeded on
  degenerate models and acted as a universal fallback for other failing tests.
- [x] `compareModelDiagnostics()` no longer reports a substituted fallback
  diagnostic under the requested test's name.
- [x] Removed two shadowed duplicate definitions: the obsolete
  `performHCCovarianceTest()` and `performQuantileRegressionTest()` in
  `modern_diagnostics.R` were being overwritten at load time by the corrected
  versions. Correct behaviour depended on collation order.
- [x] Standardised bootstrap and permutation p-values on the finite-simulation
  convention `(1 + #) / (B_eff + 1)`, with a regression test covering every
  resampling entry point.

### Packaging / correctness hygiene
- [x] Declared dependencies correctly: `R6` and `parallel` in `Imports`, `digest`
  in `Suggests`.
- [x] Documented the previously undocumented `performBreuschPaganTest` export.
- [x] Consistent `df` labelling in `htest` output across the auxiliary-regression
  and ARCH tests.
- [x] Shipped the example datasets as `.rda` (with reproducible `data-raw/`
  scripts) and documented the `boston_housing` provenance.
- [x] `performWhiteTest()` drops collinear auxiliary columns and uses
  `df = rank`, so it works on factor models; `performVIFDiagnostic()` rewritten to
  work on the design matrix (factor-safe).
- [x] Consolidated the test suite onto one `test-*.R` convention, pinned
  `Config/testthat/edition: 3`, removed `context()` and three dead `skip()`ped
  files, and migrated edition-2 idioms; behavioural regression tests added.

### Documentation
- [x] Six-part executable tutorial series under `inst/tutorials/` (detection,
  remediation, modern & scalable diagnostics, group-wise variance, time series /
  ARCH, and a power-based test-selection study).
- [x] Refreshed the R Journal manuscript: new sections on the modern/robust and
  streaming diagnostics, reproducible figures, and verified usage examples.

## Current priorities (next)

- [ ] **Finish CRAN readiness.** Run a full `R CMD check --as-cran` with every
  Suggests installed and clear any remaining notes; decide whether `boston_housing`
  should remain a verbatim `MASS::Boston` copy or be replaced with a derived
  example. (Data are now `.rda`; `cran-comments.md` reflects the real state.)
- [ ] Restore `renv.lock` to match the declared `Imports`/`Suggests`.
- [ ] Clear the remaining `R CMD check` notes: reconcile the hand-written `.Rd`
  files with the function signatures (`\usage`/code mismatches) and replace
  non-ASCII characters in source with escapes. The `.Rd` files for the tests
  corrected in 0.7.0 are already reconciled; the rest are pre-existing and best
  done as a dedicated documentation-reconciliation pass.

## Short-term improvements

- [ ] Pass B of the validation matrix: the group-variance tests (Levene,
  Brown-Forsythe, Bartlett, Fligner-Killeen, Hartley F-max, O'Brien, modified
  Bartlett), several of which have closed-form definitions or reference
  implementations to reproduce.
- [ ] Pass C of the validation matrix: the methods with the least reference
  coverage (Cameron-Trivedi, ordered LM, Davidian-Carroll, Rice, Curry-Walsh,
  rank permutation, high-dimensional, spatial and panel diagnostics). These
  need either an established implementation to reproduce or the original
  paper's statistic reconstructed independently.
- [ ] Extend the validation matrix to the remaining `perform*Test` exports and
  publish the combined size/power table.
- [ ] Stop labelling percentile intervals of a test statistic as confidence
  intervals. (The p-value convention itself was standardised in 0.7.0.)
- [ ] Factor out repeated boilerplate (scalar validators, the model/data preparation
  block, intercept-stripping) into shared helpers.

## Sequencing

Statistical correctness comes before API reduction. Shrinking the public
surface is still wanted, but mixing API-breaking cleanup into the same release
as method-definition corrections makes both harder to review and harder to
explain in `NEWS.md`. The order is: finish the validation passes, green CI,
release, and only then reduce the exported surface.

## Medium-term

- [ ] Split and trim the oversized infrastructure modules (notably `validation.R`)
  into cache / result-type / assumption-checking / requirement-dispatch units.
- [ ] Extend streaming to more tests where it is meaningful, and expose a single
  `chunk_threshold_mb` policy consistently.
- [ ] Strengthen the feasible-WLS and auto-transform helpers (Box–Cox search,
  diagnostics on weighted residuals) used in the remediation workflow.
- [ ] Broaden reference-comparison coverage (skedastic, car, sandwich) into a single
  parametrised accuracy-validation test.

## Long-term

- [ ] First-class panel and spatial heteroscedasticity workflows.
- [ ] Helpers that bridge detection to conditional-variance modelling (ARCH/GARCH)
  for the time-series path.
- [ ] An experiment/report artefact (model card style) summarising a diagnostic run.

## Technical debt

- `renv.lock` is incomplete relative to `Imports`/`Suggests`; the Docker build leans
  on `install_local()` rather than the lockfile.
- The recommendation/benchmark/dashboard layers are large relative to the package's
  "simple implementations" remit; some carry optional dependencies that are only
  exercised conditionally.
- Several `htest` printers and metadata fields are hand-maintained and can drift
  from the roxygen sources; periodic reconciliation is needed.

## Open questions

- How much of the recommendation/automation layer belongs in the core package
  versus a companion package?
- What is the right default multiplier (`rademacher` vs `mammen`) and `B` for the
  null-imposed wild bootstrap across typical sample sizes?
- Should the caching layer depend on `digest` unconditionally (move to `Imports`)
  or remain an optional accelerator?
