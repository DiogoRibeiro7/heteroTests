# heteroTests News

## 0.7.0

This release is the first pass of a package-wide statistical validation effort.
Pass A covers the classical regression diagnostics. Every test in this group was
checked against an established implementation where one exists, and otherwise
against a reconstruction of the primary reference, followed by Monte Carlo size
and power measurement. Reference equivalence is asserted in
`tests/testthat/test-pass-a-reference.R`; the simulation evidence is reproducible
via `inst/validation/pass-a-size-power.R` and recorded in
`inst/validation/pass-a-size-power.csv`.

Several of the corrections below change the value a test returns. They are
breaking changes, and the minor version has been bumped accordingly.

### Statistical corrections

- **Bug fix (critical): `performSzroeterTest()` was standardised incorrectly and
  had no power.** Szroeter's (1978) rank-weighted statistic
  `h = sum(i * e_(i)^2) / sum(e_(i)^2)` has null mean `(n + 1) / 2` and null
  variance `(n^2 - 1) / (6n)`. The previous implementation rescaled `h` by the
  mean rank and then divided the centred statistic by a further `sqrt(n)`, using
  an ad-hoc variance `(n + 1)(2n + 1) / (12n)`. The net effect was to shrink the
  standardised statistic by a factor of roughly `2 / sqrt(n)` — a factor of 5 at
  `n = 100`. In 5,000 replications the test rejected in **0.0%** of samples under
  the null *and* under every alternative tried: it could not reject at any sample
  size or effect size. The statistic is now Szroeter's `Q`, which attains 4.5%
  size and 87–91% power in the same designs. The test also gained an
  `alternative` argument (`"greater"`, the default, for variance increasing with
  the ordering variable), and reports `h` in `estimate`.

- **Bug fix: `performNCVTest()` did not compute the NCV test.** It regressed the
  *absolute* residuals on the fitted values and reported a t statistic — a
  Glejser-type diagnostic — while documenting itself as equivalent to
  `car::ncvTest()`. It now computes the Cook–Weisberg score statistic: half the
  explained sum of squares from regressing `e^2 / (RSS / n)` on the variance
  model, referred to a chi-squared distribution. It reproduces `car::ncvTest()`
  to within `1e-8`. A new `var_formula` argument selects a variance model other
  than the fitted values, matching `car::ncvTest(var.formula = )`.

- **Bug fix: `performCookWeisbergTest()` returned the Koenker statistic.** It
  computed `n R^2` from regressing the raw squared residuals on the fitted
  values — that is, Stata's `estat hettest, iid fitted` — while documenting
  itself as Stata's `estat hettest, fitted`. It now computes the Cook–Weisberg
  score statistic and delegates to `performNCVTest()` so the two exports cannot
  drift apart. Note the trade-off this exposes: the score test assumes normal
  errors, and in simulation it over-rejects under heavy tails where the previous
  (mislabelled) statistic did not. `performKoenkerTest()` remains the
  kurtosis-robust choice and is now cross-referenced from both help pages.

- **Change: `performHarveyTest()` now follows Harvey (1976).** The variance
  regressors default to the model's own explanatory variables rather than the
  fitted values and their square, and the statistic is the classical
  `ESS / (pi^2 / 2)` referred to a chi-squared distribution, where `pi^2 / 2` is
  the null variance of `log(chi^2_1)`. The previous behaviour is available as
  `auxiliary = "fitted"`, and the studentized F form — which estimates that
  variance from the data instead of assuming normality, and holds size better
  under heavy tails — as `studentize = TRUE`.

- Harvey and NCV now take their degrees of freedom from the realised rank of the
  auxiliary design, so a rank-deficient variance model is not credited with
  degrees of freedom for columns that `lm()` aliased away.

- `performHarveyTest()` and `performParkTest()` now warn when a residual is
  numerically zero and has been floored before taking logarithms. A floored
  observation contributes `log(.Machine$double.eps) ~ -36`, roughly sixteen
  standard deviations below the mean of the null `log(chi^2_1)` distribution, and
  can dominate the auxiliary regression on its own.

- `performParkTest()`, `performGlejserTest()`, `performNCVTest()` and
  `performCookWeisbergTest()` now name their `parameter` element, so
  `print.htest()` labels the degrees of freedom instead of printing `= 118`.

### Validated without change

`performArchLMTest()` and `performMcLeodLiTest()` were audited and required no
change. ARCH LM reproduces Engle's `T R^2` auxiliary regression with `T = n - q`
(the `FinTS::ArchTest()` convention) and McLeod–Li reproduces the Ljung–Box
statistic on squared residuals with `df = m`, both to within `1e-10`, with
empirical size of 4.4% and 5.6% and power of 99% and 97% against an ARCH(1)
alternative.

### Diagnostic workflow

- **Bug fix: `compareModelDiagnostics()` could report one test's statistic under
  another test's name.** When a requested diagnostic fails, `runHeteroTests()`
  substitutes a fallback diagnostic and tags the result with the substitute's
  name. `compareModelDiagnostics()` read the statistic without checking that tag,
  so a comparison table column headed `white` could contain, for example, an NCV
  statistic. It now reports `NA` and warns when a substitution occurred, since a
  numeric table has nowhere to carry the caveat.

  This masking is also why two existing tests changed expectations. Both were
  passing because the previously unvalidated `performNCVTest()` succeeded on
  degenerate models — including a perfectly explained fit — and so served as a
  fallback that always worked. With NCV validating its input, the underlying
  failures are now surfaced rather than papered over.

### Source integrity

- **Removed two shadowed duplicate definitions.** `performHCCovarianceTest()` and
  `performQuantileRegressionTest()` each had two definitions in the source tree.
  The obsolete implementations in `R/modern_diagnostics.R` were loaded first and
  then silently overwritten by the corrected versions in
  `R/zz_deprecated_hc_covariance.R` and `R/quantile_regression_joint.R`. Runtime
  behaviour was correct only because of collation order; a partial sourcing, a
  file rename or a collation change would have resurrected the withdrawn HC
  pseudo-test and the superseded quantile comparison. The obsolete definitions
  and their roxygen blocks are gone, along with the now-unreachable
  `.hc_adjustment()` helper whose only caller was the deleted HC implementation.

### Resampling inference

- Bootstrap and permutation p-values now consistently use the finite-simulation
  convention `(1 + #{T_b >= T_obs}) / (B_eff + 1)`, where `B_eff` counts the
  replicates that converged. `performWhiteTestBootstrap()`,
  `performWhiteTestEnhanced()` and the shared `rbootstrap_test_statistic()`
  engine previously used the raw proportion `#/B`, which can return exactly
  zero — not an attainable p-value from a finite number of replicates — and is
  anti-conservative in the tail. `performWildBootstrapTest()` and
  `performRankPermutationTest()` already used the corrected form.
  `tests/testthat/test-bootstrap-pvalue-convention.R` pins all of them.

### Packaging and continuous integration

- **Fixed the vignette build, which was breaking `R CMD build` and therefore
  every platform's check job.** Three vignettes failed to build:
  `bibliography.Rmd` called `algorithms_bibliography()`, which has never existed
  as a function (the name belongs to a documentation-only help topic);
  `comprehensive_guide.Rmd` called `simulate_hetero(n = 50)` without the required
  `beta0`, `beta1` and `sigma_func` arguments; and `using_heteroTests.Rmd` passed
  `mtcars` as the data for a model fitted on `boston_housing`, referenced the
  non-existent `performGoldfeldQuandtTest()`, contained an illustrative chunk
  using an undefined `your_data` placeholder, and had a malformed chunk fence
  that left ten lines of code outside any chunk. `R CMD build` now completes and
  all eight vignettes render.
- `inst/CITATION` modernised: 25 `citEntry()` calls converted to `bibentry()`
  (deprecated since R 4.2), a duplicate `citHeader()` that silently replaced the
  first removed, and the package version now read from `meta$Version`. The
  previous `utils::packageDescription("heteroTests")$Version` raised
  "$ operator is invalid for atomic vectors" during CRAN incoming checks when the
  package was not installed.
- The CI R-version matrix was rebuilt around R-devel, R-release and R-oldrel-1,
  with macOS and Windows on R-release, replacing the fixed 4.0/4.1/4.2/4.3 set.
  The minimum version declared in `Depends:` now has its own Ubuntu 22.04 job
  that installs hard dependencies only, so it reports on the package's own R 4.0
  compatibility rather than on whether modern `Suggests` still support R 4.0.

- **Fixed a broken example that failed `R CMD check`.**
  `performQuantileRegressionTest()` documented itself with `mtcars`, which has 32
  observations, while the test requires at least 40. The example now uses
  `boston_housing`.
- Non-ASCII characters removed from `R/messages.R` and `R/validation.R`. The two
  occurrences inside string literals are now written as the escapes
  `\u00b2` and `\u2022`; the en dashes in roxygen comments became ASCII hyphens.
- Resolved 18 duplicated `\alias` entries. `man/simulate_hetero.Rd` is a complete
  hand-written family page for `simulate_hetero()`, `simulate_arch1()` and every
  `sigma_*()` helper, but roxygen2 also generated a thinner page per helper, so
  each name was documented twice. The generated stubs are removed — several were
  incomplete, for example `sigma_poly.Rd` omitted `a`, `b` and `c` — and the
  corresponding roxygen blocks are marked `@noRd` so they do not reappear. The
  same applied to `generate_benchmark_report`, which is documented on the
  `run_benchmark_suite` page.
- Reconciled `\usage` sections that had drifted from their function signatures:
  `performWhiteTest()` (`max_interactions`), `plot.HeteroDiagnostic()` (`plots`)
  and `summary.HeteroDiagnostic()` (`tests`).
- Documented previously undocumented arguments in
  `generateHeteroRecommendations.Rd`, `plot.power_analysis.Rd`,
  `print.htest_enhanced.Rd`, `test.HeteroDiagnostic.Rd` and
  `simulate_hetero.Rd`, and merged the duplicated `n` entry on the last of these.
- `.Rbuildignore` now excludes `.zenodo.json`, `CITATION.cff` and
  `CONTRIBUTING.md`, which are repository metadata rather than package content.
  `inst/validation/` is deliberately shipped so the size and power evidence
  travels with the package.

- **Fixed a latent runtime failure in the dashboard.**
  `launchDiagnosticDashboard()` used the magrittr pipe `%>%` in four places
  without importing it. `DT` and `plotly` are `Suggests` and are called with `::`
  rather than attached, so the pipe was only ever resolved if one of them
  happened to be attached by something else. The calls are now nested.
- A second example failed `R CMD check`: `performWhiteTestBootstrap()` used
  `mtcars` (32 observations) while the bootstrap requires 50. It now uses a
  simulated 120-observation dataset, with the heavier replication counts moved
  under `\donttest{}`.
- Fixed 15 broken `\link[stats:htest]{htest}` cross-references. There is no
  `htest` topic in `stats`, so every one of these was a dangling link; they are
  now plain `\code{htest}`.
- Completed the `NAMESPACE` imports. `AIC`, `cooks.distance`, `cov`, `lm.fit`,
  `model.frame`, `model.response`, `nobs`, `quantile`, `resid`, `terms` and
  `uniroot` from `stats`, and `head`, `object.size` and `tail` from `utils`,
  were used without being imported. Column names referenced inside `aes()` are
  declared through `utils::globalVariables()`.
- Removed the `Author:` and `Maintainer:` fields from `DESCRIPTION`, which had
  drifted from the values derived from `Authors@R` (the derived form carries the
  ORCID). Unescaped 15 `\&` sequences in `.Rd` files, and added `_pkgdown.yml`
  and `LICENSE` to `.Rbuildignore`.

- **Fixed three defects in the benchmarking system**, all of which surfaced once
  its example was actually reachable under `R CMD check --run-donttest`.
  `run_benchmark_suite()` aborted with "no rows to aggregate" whenever every
  measured difference was `NA`, because `aggregate()` drops `NA` responses and
  the guard only tested for zero rows. `generate_benchmark_report()` then failed
  with "argument 1 is not a vector" when called with `profile_memory = FALSE`,
  since it passed a `NULL` column to `order()`. And `ht_extract_metrics()` had no
  branch for `car::ncvTest()`, which returns `ChiSquare`/`Df`/`p` rather than the
  `htest` field names, so every accuracy row measured against `car` was silently
  `NA`. With that branch in place the benchmark now reports a p-value difference
  of about `1e-15` between `performNCVTest()` and `car::ncvTest()` -- an
  independent confirmation of the correction described above.

### Documentation

- `performGlejserTest()` documents that the test is not asymptotically valid
  under asymmetric errors (Godfrey 1996; Im 2000), and that scanning several
  transformations and reporting the smallest p-value invalidates the nominal
  level.
- `performParkTest()` documents that its t statistic is valid only
  asymptotically, the auxiliary error being a strongly skewed `log(chi^2_1)`
  variate, and that a non-significant result speaks only to the single variable
  tested.


## 0.6.5

- Bug fix (statistical correctness): `performBPTest()`/`performBreuschPaganTest()`
  now compute the classical Breusch-Pagan statistic (half the explained sum of
  squares from the scaled squared residuals), matching
  `lmtest::bptest(studentize = FALSE)`. It previously returned the studentized
  `n R^2` statistic despite being documented as the classical, normality-based test.
- Bug fix (statistical correctness): `performKoenkerTest()` now implements
  Koenker's (1981) studentized Breusch-Pagan statistic as `n R^2` from regressing
  the **squared** residuals on the regressors, matching
  `lmtest::bptest(studentize = TRUE)` and `performStudentizedBPTest()`. It
  previously regressed the absolute residuals, which is not a recognized test and
  produced p-values without a valid chi-squared justification.
- The streaming variants `performBPTestStreaming()` and
  `performKoenkerTestStreaming()` were updated to stay algebraically equivalent to
  their exact counterparts.
- Bug fix: resolved a duplicate `%||%` operator definition whose two copies had
  conflicting semantics. Depending on collation order this could cause
  `runHeteroTests()` to abort with a "length > 1" coercion error when given a
  multi-column data frame. Both copies now share a single vector-safe definition.
- Bug fix (statistical correctness): `performWildBootstrapTest()` now generates its
  bootstrap reference distribution **under the homoscedastic null** by resampling
  leverage-standardised, centred residuals i.i.d. before applying the wild
  multiplier. The previous implementation multiplied the original (heteroscedastic)
  residuals by the wild weights, so the bootstrap distribution inherited the
  heteroscedasticity being tested for and the test had essentially no power
  (returning p ~ 0.5 on strongly heteroscedastic data). It now controls size and
  detects heteroscedasticity reliably; regression tests cover both.
- Packaging: `R6` moved from `Suggests` to `Imports` (it is used at package load
  time), `parallel` added to `Imports` (used via `::`), and the optional `digest`
  dependency (used for cache hashing) is now declared in `Suggests`. These resolve
  `R CMD check` "undeclared dependency" and load-time failures.
- Bug fix: `performWhiteTest()` now drops perfectly collinear auxiliary columns
  (via QR pivoting) and reports `df` equal to the realised rank, instead of
  aborting. White's test consequently works on models containing factors, where a
  squared dummy column is collinear with the dummy itself.
- Bug fix: `performVIFDiagnostic()` now computes variance inflation factors from
  the inverse correlation matrix of the design, so it works for models with factor
  predictors (it previously built auxiliary formulas from contrast-expanded column
  names that do not exist in the model frame, e.g. `cyl6`, and errored).
- `ht_data_cleaning_suggestions()` now flags infinite values found in the supplied
  data, not only those mentioned in the error message.
- Bug fixes surfaced by `R CMD check`: corrected invalid namespace qualifications
  (`stats::all.vars`, `stats::summary`, `utils::close` are base functions), fixed a
  `suggestRemediation()` call in `generateDiagnosticReport()` that passed
  unsupported arguments, and declared the (guarded) optional `mgcv` dependency.
- Data sets `boston_housing`, `diagnostic_data` and `hetero_data` are now shipped
  as `.rda` files (generated by scripts under `data-raw/`) rather than R scripts,
  removing the `LazyData` note and making the bundled data reproducible.
- Testing: the suite is consolidated onto a single `test-*.R` naming convention,
  pins `Config/testthat/edition: 3`, drops the deprecated `context()` calls,
  removes three stale top-level-`skip()`ped files that provided no coverage, and
  adds behavioural regression tests for the fixes above.

## 0.6.4
- Added "Comprehensive Guide to Heteroscedasticity Testing" and "Troubleshooting Common Issues" vignettes.
- Implemented `performWhiteTestStreaming` for large datasets.
- Added wild bootstrap, HC covariance, quantile regression, permutation, high-dimensional, and spatial heteroscedasticity diagnostics.
- Integrated broom tidiers, ggplot2 theming/autoplot support, tidymodels workflows, survey designs, grouped data pipelines, and
  data.table/dtplyr compatibility for diagnostic workflows.
- Introduced an intelligent recommendation engine that profiles datasets, interprets diagnostics with confidence levels, and
  delivers remediation guidance plus decision-tree style reports for non-statisticians.
- Implemented robust error handling with adaptive fallbacks, actionable cleaning suggestions, and configurable logging for
  complex diagnostic workflows.

## 0.6.3
- Added automated report generation via `generateDiagnosticReport`.

## 0.6.2
- Added studentized Breusch-Pagan, bootstrap White and Szroeter tests.

## 0.6.1
- Introduced `TestFactory` R6 class with metadata support.
- Added exported `test_factory` instance registering core tests.
- DESCRIPTION now lists R6 in Suggests.
