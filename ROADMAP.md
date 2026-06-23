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
- [ ] Clear the remaining `R CMD check` warnings: reconcile the hand-written `.Rd`
  files with the function signatures (`\usage`/code mismatches), replace non-ASCII
  characters in source with escapes, and fix the vignette build metadata. These are
  pre-existing and best done as a dedicated documentation-reconciliation pass.

## Short-term improvements

- [ ] Unify input validation across the `perform*Test` family; `performNCVTest()`
  and `performCookWeisbergTest()` currently bypass the shared framework.
- [ ] Standardise bootstrap p-value conventions (always `(1 + #)/(B + 1)`) and stop
  labelling percentile intervals of a test statistic as confidence intervals.
- [ ] Factor out repeated boilerplate (scalar validators, the model/data preparation
  block, intercept-stripping) into shared helpers.

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
