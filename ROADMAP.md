# heteroTests Roadmap

This document tracks the direction of the package: what is done, what is
in progress, and what is planned. It is updated alongside meaningful
code changes. Dates are omitted in favour of milestone ordering.

## Guiding principles

- **Statistical correctness first.** Every test must reproduce an
  established reference (or a defensible derivation) and be covered by a
  test that checks *behaviour* (size and power), not just object
  structure.
- **One consistent interface.** All diagnostics return base-R `htest`
  objects and follow the `perform*Test(model, data, ...)` convention.
- **Scale and robustness as first-class concerns**, not afterthoughts:
  streaming implementations for large data, resampling/robust variants
  for small or non-normal samples.

## Completed

### Statistical correctness

Corrected the Breusch–Pagan / Koenker swap:
[`performBPTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md)
is the classical statistic (`lmtest::bptest(studentize = FALSE)`),
[`performKoenkerTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md)
the studentized `n R^2` form (`studentize = TRUE`); validated to machine
precision and in the reference-comparison tests.

Reimplemented
[`performWildBootstrapTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWildBootstrapTest.md)
as a **null-imposed** bootstrap so it controls size and actually has
power; added size/power regression tests.

Kept the streaming variants algebraically identical to their exact
counterparts (covered by `test-streaming.R`).

Resolved a duplicate `%||%` operator collision that could abort
[`runHeteroTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md)
on multi-column data.

**Pass A of the statistical validation matrix** (classical regression
diagnostics). Each test was checked definition -\> reference -\>
implementation -\> numerical equivalence -\> size/power. Reference
equivalence is asserted in `tests/testthat/test-pass-a-reference.R`;
size and power are measured by `inst/validation/pass-a-size-power.R` and
recorded in `inst/validation/pass-a-size-power.csv`. Corrections:
[`performSzroeterTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performSzroeterTest.md)
(wrong standardisation, zero power),
[`performNCVTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performNCVTest.md)
(was a Glejser-type t-test, not the score test it documented),
[`performCookWeisbergTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performCookWeisbergTest.md)
(returned the Koenker statistic),
[`performHarveyTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performHarveyTest.md)
(non-standard auxiliary design and statistic).
[`performArchLMTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performArchLMTest.md)
and
[`performMcLeodLiTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performMcLeodLiTest.md)
passed unchanged.

Unified input validation across the Pass A tests;
[`performNCVTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performNCVTest.md)
and
[`performCookWeisbergTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performCookWeisbergTest.md)
no longer bypass the shared framework. This also removed a masking
effect in which the unvalidated NCV test succeeded on degenerate models
and acted as a universal fallback for other failing tests.

[`compareModelDiagnostics()`](https://diogoribeiro7.github.io/heteroTests/reference/compareModelDiagnostics.md)
no longer reports a substituted fallback diagnostic under the requested
test’s name.

Removed two shadowed duplicate definitions: the obsolete
`performHCCovarianceTest()` and
[`performQuantileRegressionTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performQuantileRegressionTest.md)
in `modern_diagnostics.R` were being overwritten at load time by the
corrected versions. Correct behaviour depended on collation order.

Standardised bootstrap and permutation p-values on the finite-simulation
convention `(1 + #) / (B_eff + 1)`, with a regression test covering
every resampling entry point.

### Packaging / correctness hygiene

Declared dependencies correctly: `R6` and `parallel` in `Imports`,
`digest` in `Suggests`.

Documented the previously undocumented `performBreuschPaganTest` export.

Consistent `df` labelling in `htest` output across the
auxiliary-regression and ARCH tests.

Shipped the example datasets as `.rda` (with reproducible `data-raw/`
scripts) and documented the `boston_housing` provenance.

[`performWhiteTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTest.md)
drops collinear auxiliary columns and uses `df = rank`, so it works on
factor models;
[`performVIFDiagnostic()`](https://diogoribeiro7.github.io/heteroTests/reference/performVIFDiagnostic.md)
rewritten to work on the design matrix (factor-safe).

Consolidated the test suite onto one `test-*.R` convention, pinned
`Config/testthat/edition: 3`, removed `context()` and three dead
`skip()`ped files, and migrated edition-2 idioms; behavioural regression
tests added.

### Documentation

Six-part executable tutorial series under `inst/tutorials/` (detection,
remediation, modern & scalable diagnostics, group-wise variance, time
series / ARCH, and a power-based test-selection study).

Refreshed the R Journal manuscript: new sections on the modern/robust
and streaming diagnostics, reproducible figures, and verified usage
examples.

## Current priorities (next)

**Finish CRAN readiness.** Run `R CMD check --as-cran` with every
Suggests installed and clear what remains. The last local run left three
notes, two of them environmental (no network to verify the system clock,
`V8` absent for math rendering); the `\doi{}` note was cleared in 0.8.1.

Replace non-ASCII characters in source with escapes.

Decide whether R 4.1 remains the supported floor, or whether the minimum
should track R’s own support window.

### Validation matrix

The validation effort ran in four passes and is complete for the
exported surface. Evidence lives in `inst/validation/`, with scripts
that regenerate it.

Pass A, classical regression diagnostics (0.7.0).

Pass B, group-variance tests (0.7.1).

Pass C, the methods with least reference coverage (0.7.2).

Full sweep over all 32 exported `perform*Test()` functions (0.11.0),
each driven by the null and alternative appropriate to what it tests
rather than one process for all of them.

Six exported procedures were found to be broken, and all six shared a
single property: no size or power check.

| procedure | fault | outcome |
|----|----|----|
| `performRiceTest()` | insensitive by construction; rejection rate 0% under every variance pattern tried | withdrawn in 0.8.0 |
| `performCurryWalshTest()` | 0% rejection | withdrawn in 0.8.0 |
| [`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md) | weights were the inverse squared residuals of the same fit; nominal 95% intervals covered 10.4% | corrected to feasible GLS in 0.9.0 |
| [`rbootstrap_test_statistic()`](https://diogoribeiro7.github.io/heteroTests/reference/rbootstrap_test_statistic.md) | resampled rows rather than under the null, so the p-value sat near 0.5; 0% power | null-imposed resampling in 0.10.0 |
| [`performBPRandomEffectsTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPRandomEffectsTest.md) | statistic omitted the `- 1` and the square from Breusch-Pagan’s equation 5; size 32.5% | corrected in 0.11.0 |
| [`performPesaranTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performPesaranTest.md) | `T` divided where Pesaran’s CD multiplies, making the statistic `1/T` too small; size 0.0% | corrected in 0.11.0 |

Twenty-five of the twenty-six heteroscedasticity tests hold their
nominal level, at 400 replications and n = 150. The exception is
[`performBoxMTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBoxMTest.md),
conservative at 0.012; `inst/validation/README.md` carries the table and
the reasoning, and generates those counts from the CSV rather than
restating them, because an earlier draft of this paragraph quoted
figures from a different run and a z computed against the wrong standard
error.

Two conclusions worth keeping. A reference comparison is not a
substitute for a size check: four of the six faults above are in
procedures with no reference implementation to compare against, and the
two that had one agreed with it. And a stored-value regression test
would have frozen each fault rather than caught it, so the guards added
are simulated size, not recorded numbers.

## Short-term improvements

Factor out repeated boilerplate (scalar validators, the model/data
preparation block, intercept-stripping) into shared helpers.

Give the group-variance tests a heavy-tailed size column in the shipped
sweep. Bartlett and Hartley reject about 24% of the time against a `t5`
null, which the Pass B table records but the full sweep does not yet
cover.

Added a reference comparison for the panel statistics.
[`performBPRandomEffectsTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPRandomEffectsTest.md)
reproduces `plm::plmtest(type = "bp")` and
[`performPesaranTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performPesaranTest.md)
reproduces `plm::pcdtest(model = "pooling")`, both to 1e-8 across three
panel shapes. Neither had a reference among the packages the accuracy
table uses, which is why both were wrong until 0.11.0.

## Sequencing

Statistical correctness comes before API reduction. Shrinking the public
surface is still wanted, but mixing API-breaking cleanup into the same
release as method-definition corrections makes both harder to review and
harder to explain in `NEWS.md`. The order is: finish the validation
passes, green CI, release, and only then reduce the exported surface.

## Medium-term

Split and trim the oversized infrastructure modules (notably
`validation.R`) into cache / result-type / assumption-checking /
requirement-dispatch units.

Extend streaming to more tests where it is meaningful, and expose a
single `chunk_threshold_mb` policy consistently.

Strengthen the feasible-WLS and auto-transform helpers (Box–Cox search,
diagnostics on weighted residuals) used in the remediation workflow.

Broaden reference-comparison coverage (skedastic, car, sandwich) into a
single parametrised accuracy-validation test.

## Long-term

First-class panel and spatial heteroscedasticity workflows.

Helpers that bridge detection to conditional-variance modelling
(ARCH/GARCH) for the time-series path.

An experiment/report artefact (model card style) summarising a
diagnostic run.

## Technical debt

- `renv.lock` is incomplete relative to `Imports`/`Suggests`; the Docker
  build leans on `install_local()` rather than the lockfile.
- The recommendation/benchmark/dashboard layers are large relative to
  the package’s “simple implementations” remit; some carry optional
  dependencies that are only exercised conditionally.
- Several `htest` printers and metadata fields are hand-maintained and
  can drift from the roxygen sources; periodic reconciliation is needed.

## Open questions

- How much of the recommendation/automation layer belongs in the core
  package versus a companion package?
- What is the right default multiplier (`rademacher` vs `mammen`) and
  `B` for the null-imposed wild bootstrap across typical sample sizes?
- Should the caching layer depend on `digest` unconditionally (move to
  `Imports`) or remain an optional accelerator?
