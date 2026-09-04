# heteroTests News

## 0.10.0

`rbootstrap_test_statistic()` now resamples under the null. Its p-value
changes, and so does the `p.value_bootstrap` field of the two robust tests,
hence the minor version.

### The bootstrap p-value could not detect anything

The helper resampled rows of the data with replacement, refitted, and
recomputed the statistic. Those replicates follow the statistic's distribution
under whatever variance structure the data actually have, not under the null:
they are centred on the observed statistic, so comparing the two returns
roughly one half however strong the heteroscedasticity is.

Measured over 400 samples at n = 120, B = 199, testing Koenker's statistic:

| procedure | size, homoscedastic | power, sd = x^2 |
| --- | ---: | ---: |
| pairs resampling (before) | 0.0% | **0.0%** |
| null-imposed (now) | 5.0% | 100% |
| `performWildBootstrapTest()` | 2.5% | 100% |
| Koenker, asymptotic | 5.0% | 100% |

Zero rejections under a strong alternative. The p-value was exported directly
and also reached users as `p.value_bootstrap` from `performWhiteTestRobust()`
and `performBPTestRobust()` when called with `bootstrap = TRUE`.

`resample` now takes `"null"`, which is the default. The response is
regenerated as `fitted + e*`, with `e*` drawn with replacement from the
residuals after dividing by `sqrt(1 - h_i)` and centring, so the regenerated
data satisfy homoscedasticity by construction. Size is 5.0% (Monte Carlo
standard error 1.1%) and power is 99.2% against `sd = 0.5x` and 100% against
`sd = x^2`. Size holds at 5.0% under homoscedastic `t_5` errors as well, so
the calibration does not depend on Gaussian tails -- which matters here,
because Bartlett's and Hartley's tests reject about 24% of the time against
the same heavy-tailed null (see `inst/validation/README.md`).

The leverage correction is not decorative. OLS residuals have variance
`sigma^2 (1 - h_i)`, so resampling them unscaled under-disperses the
regenerated errors; a version without it rejected about 13% of the time under
the null. This is the construction `performWildBootstrapTest()` already used.

`resample = "pairs"` is still available and still returns the replicates and
the interval, which describe the statistic's variability perfectly well. It now
returns `NA` for `p_value` rather than a number that cannot be interpreted.

Null-imposed resampling is restricted to a Gaussian `lm` whose response is a
plain variable, and errors otherwise. Two cases previously ran and returned a
plausible p-value from replicates that meant nothing: a transformed response
such as `log(y) ~ x`, where `fitted()` is on the log scale and the data column
holds `y`, so refitting took the logarithm twice and returned p = 1; and a
`glm`, where the refit used `safe_lm()` and dropped the family and link,
returning p = 0.952 for a Poisson model. `resample = "pairs"` resamples rows
and is unaffected by the transformed-response case, so it remains available
there.

A `glm` is now refused by *both* strategies. Each replicate is refitted with
least squares whichever strategy is used, so on a Poisson fit of counts the
coefficients move from (0.457, 0.406) on the log link to (-0.931, 2.281) on the
identity scale: the replicates describe a different model from the one passed.
The helper documented `glm` support it never had.

### Percentile intervals are not confidence intervals

The `ci` component was documented as a "percentile confidence interval for the
statistic". It is a percentile interval of the bootstrap distribution: it
summarises where the resampled statistic falls, is not an interval for a
parameter, and carries no coverage guarantee. The documentation says so. This
closes the corresponding roadmap item.


## 0.9.0

`fitWLS()` now estimates its weights from a variance model. The numbers it
returns change, hence the minor version.

### fitWLS() produced unusable standard errors

The weights were the inverse squared residuals of the initial fit,
`w_i = 1 / e_i^2`. A squared residual is a one-degree-of-freedom estimate of
`sigma_i^2`, far too noisy to invert, and inverting it hands the weight to
whichever observations the first fit happened to reproduce most closely. On
`quakes` that meant five of a thousand points carrying 99.5% of the total
weight, with a largest-to-median weight ratio of about 1e+07.

Two consequences, measured over 2000 replications of
`y = 1 + 2x + e`, `sd(e) = 0.5x`, at n = 200:

| estimator | SD(beta1) | coverage of nominal 95% |
| --- | ---: | ---: |
| OLS | 0.1027 | 93.7% |
| `fitWLS()` before | 0.1022 | **10.4%** |
| `fitWLS()` now | 0.0837 | 96.1% |
| oracle weights | 0.0827 | 95.1% |

The point estimate was unbiased but no more efficient than OLS, so the function
delivered none of the efficiency that is the purpose of weighting; and its
intervals covered the truth about a tenth of the time, because the weighted
residual sum of squares collapses towards `n` and the reported sigma is
approximately `sqrt(n / (n - p))` whatever the data.

`fitWLS()` now regresses `log(e^2)` on the model's own design matrix and weights
by `1 / exp(fitted)`, the standard feasible-GLS recipe. That recovers the
efficiency (SD 19% below OLS, close to the oracle) and calibrates the
intervals. Zero residuals are floored before the logarithm by the same helper
the log-variance tests use, and an unusable variance model falls back to equal
weights, which degrades to the original OLS fit rather than failing.

The estimated variances are attached to the result as the `"variance_model"`
attribute.

### Knock-on corrections

- `compareModelDiagnostics()` can now run White's test on a WLS fit. It
  previously reported `NA` there, correctly, because the degenerate weighting
  made the weighted fit perfectly explained; that cell has now held three
  different things across three releases and the test records all three.
- `autoCompareRemediations()` compares AIC and RMSE across OLS, WLS and robust
  fits. The WLS row was computed from the degenerate fit and is now meaningful.
- The WLS section of `real_world_case_studies.Rmd` no longer needs its warning
  that the weighted variance profile was flat by arithmetic rather than by
  fit. On `quakes` the profile now falls from a 6.6-fold spread across thirds
  of the fitted range to 2.95-fold, which reflects the data.

### Metadata

- The maintainer affiliation is now recorded identically in `CITATION.cff`,
  `inst/CITATION`, `.zenodo.json`, `README.md` and the paper. Only
  `.zenodo.json` carried the current name; the rest still had the former one.


## 0.8.1

CRAN preparation. No change to any statistic or to the public API.

### The bundled Boston dataset is gone

`boston_housing` was a byte-identical copy of `MASS::Boston` -- all 506 rows and
all 14 columns. Shipping it under this package's `Apache License (>= 2.0)` put a
verbatim copy of GPL-2 | GPL-3 material under Apache terms, which is at best a
question a CRAN reviewer would ask, and `cran-comments.md` volunteered it.

The justification recorded there -- that the copy let examples run "without
attaching MASS" -- did not hold either: MASS is a hard `Imports` dependency, so
it is installed regardless.

Examples, vignettes and the tutorial notebooks now use R's built-in `quakes`
data, which lives in `datasets` and is attached by default. Nothing is
redistributed, so the licensing question disappears rather than being argued.
The two datasets the package still ships, `diagnostic_data` and `hetero_data`,
are simulated.

The replacement was chosen on evidence, not convenience. Against
`stations ~ mag + depth` (n = 1000, R-squared 0.74) White gives p = 2e-25,
Breusch-Pagan p = 2e-42 and Koenker p = 7e-25, and the HC3 standard error for
`mag` is 1.34 times the OLS one -- enough for the notebooks' robust-versus-
classical comparison to keep its point. The response is a count, so its variance
rises with its mean: the textbook mechanism, which makes it a better teaching
example than the original. Candidates with stronger p-values (ChickWeight,
Theoph, DNase, Loblolly, Orange) were rejected because they are repeated-measures
data, where residuals are correlated within subject and these tests' independence
assumption does not hold.

Dropping the dataset also drops the `black` column, the transformed
racial-composition variable that led scikit-learn to remove this data in 1.2.

### Packaging

- Three references used `\url{https://doi.org/...}` where CRAN asks for
  `\doi{}`, the form the rest of the package already used. Fixed in
  `performStudentizedBPTest`, `performSzroeterTest` and
  `performWhiteTestBootstrap`, and in the roxygen sources so a regeneration
  produces the same form.
- README no longer lists two limitations that were fixed in 0.8.0: the
  `renv.lock` drift and the bundled dataset.


## 0.8.0

The API review that the validation effort was sequenced towards. Six exports
are removed. This is a breaking change, hence the minor version.

Three returned nothing but a migration error, having been withdrawn once their
statistics were shown not to work:

| removed | use instead |
| --- | --- |
| `performRiceTest()` | `performSzroeterTest()`, `performGQTest()` |
| `performCurryWalshTest()` | `performSpatialHeteroTest()` |
| `performHCCovarianceTest()` | `performBPTest()`, `performKoenkerTest()`, `performWhiteTest()`; `sandwich::vcovHC()` for robust covariance |

Three computed a valid statistic under a name that promised a different method,
duplicating a test that remains:

| removed | identical to |
| --- | --- |
| `performOrderedLMTest()` | `performKoenkerTest()` -- sorting the rows and refitting returns the same residuals permuted, so its `order_by` argument could not affect the result |
| `performCameronTrivediTest()` | the `e^2 ~ yhat + yhat^2` auxiliary F test, covered by `performWhiteTest()` and `performNCVTest()`; it was never Cameron and Trivedi's information-matrix test |
| `performModifiedBartlettTest()` | `performBartlettTest()` -- the correction factor its documentation called a modification is part of the standard definition |

The package has not been released on CRAN, so no published contract is broken.
`tests/testthat/test-public-api.R` records the removals and checks that every
replacement named above is still exported.

Two dead fallback routes went with them: `hc_covariance` was registered as a
recovery path for `quantile_regression` and `high_dimensional`, and
`hc_covariance_hc0` as a custom fallback, both calling a function that only
ever raised an error.

### Packaging

- The pkgdown workflow now deploys. It called `build_site_github_pages()`,
  which writes `docs/`, and then stopped: there was no deploy step and no
  `gh-pages` branch, so the job reported success while nothing was published.
  That is why the URLs in `DESCRIPTION` and `inst/CITATION` returned 404.
- `renv.lock` now records all 38 declared non-base dependencies, up from 14.
  `renv::restore()` previously pinned about a third of them, and the Docker
  build only worked because `remotes::install_local()` resolves the remainder
  from CRAN at whatever version is current. The R version moves to 4.5.1, which
  is where the recorded versions come from.


## 0.7.2

Pass C of the statistical validation matrix: the methods with the least
reference coverage. Six were sound and one needed a fix. Two are withdrawn
because their statistics cannot detect heteroscedasticity at all, and two more
are valid statistics under names that promise something else.

### Withdrawn

- **`performRiceTest()`** compared Rice's (1984) difference-based variance
  estimator with the mean squared residual and referred the ratio to an F
  distribution. For independent residuals `E[(e_i - e_{i-1})^2]` is
  `s2_i + s2_{i-1}`, so the numerator estimates the *mean* of the variances and
  so does the denominator: the ratio sits at one under any variance pattern.
  Simulated means were 1.00 under homoscedasticity, under `sigma = 0.2 + 1.2x`,
  under `sigma = exp(x)` (a fiftyfold spread) and under a twentyfold step
  change, and empirical size was 0.000. Adding an ordering argument does not
  repair it. Use `performSzroeterTest()` or `performGQTest()` instead.

- **`performCurryWalshTest()`** passed an unstandardised Moran's I to
  `2 * (1 - pnorm(abs(I)))`. Moran's I has null expectation `-1/(n-1)` and a
  weights-dependent variance, and is bounded near one, so it essentially never
  reached 1.96: it rejected in 0.0% of samples under strong heteroscedasticity
  and returned `p = 0.875` on clustered variance. `performSpatialHeteroTest()`
  already does this correctly via `spdep::moran.mc()`.

Both retain their exports and signal a migration error, following the
withdrawn HC covariance precedent.

### Corrected

- `performDavidianCarrollTest()` failed with `NA/NaN/Inf in 'y'` when a residual
  was exactly zero. It now uses the shared log-residual floor, as Harvey and
  Park do.

### Documented rather than removed

- `performOrderedLMTest()` sorts the rows and refits, which returns the same
  residuals permuted, so `order_by` cannot change the statistic and the result
  is identical to `performKoenkerTest()`. It now warns that the argument has no
  effect.
- `performCameronTrivediTest()` is exactly the `e^2 ~ yhat + yhat^2` F test, not
  the information-matrix test of Cameron and Trivedi (1990), which concerns
  overdispersion in count models. The help page now says what it computes.

Removing either redundant export is left to the API review.

### Validated without change

`performRankPermutationTest()`, `performHighDimensionalTest()`,
`performWildBootstrapTest()`, `performQuantileRegressionTest()`,
`performSpatialHeteroTest()` and `performPesaranTest()` hold their nominal level
and have power.

## 0.7.1

Pass B of the statistical validation matrix: the group-variance tests. This
section was missing from the changelog when 0.7.1 shipped and is recorded here.

### Statistical corrections

- **Bug fix (critical): `performOBrienTest()` rejected on every input.**
  O'Brien's transformation produces one score per *observation*, whose group
  mean is that group's variance. The implementation computed a single value per
  *group* and repeated it across that group's observations, so the scores had no
  within-group variability: the one-way ANOVA had a residual sum of squares of
  zero, giving `F` on the order of `1e30` and `p = 0` for any data,
  homoscedastic or not. Empirical size is now 4.8%.

- **Bug fix (critical): `performHartleyFmaxTest()` used the wrong null
  distribution.** The statistic is the largest of `k` group variances over the
  smallest, but the p-value came from `pf(F, df, df)`, the distribution of a
  single variance ratio; the number of groups was computed and never used.
  Empirical size at a nominal 5%, groups of 30, was 0.0998 at two groups,
  0.2238 at three, 0.3497 at four and 0.5626 at six. It now uses the
  maximum-F-ratio distribution and holds 0.05 throughout.

- `performOBrienTest()` also called `safe_lm()` without a data argument, so
  every call errored once the transformation was corrected.

### Validated without change

`performLeveneTest()`, `performBrownForsytheTest()`, `performBartlettTest()` and
`performFlignerKilleenTest()` reproduce `car::leveneTest()` with mean and median
centring, `stats::bartlett.test()` and `stats::fligner.test()` to within `1e-8`.

### Documentation

- `performModifiedBartlettTest()` is Bartlett's test under another name: the
  correction factor its documentation described as a modification is part of the
  standard definition. It is now documented as a compatibility alias.
- Bartlett's per-group minimum was relaxed from three observations to two, which
  is what a sample variance requires.


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

- **Fixed the Breusch-Pagan benchmark baseline.** `performBPTest()` is the
  classical statistic, but the benchmark compared it against `lmtest::bptest()`
  with its default `studentize = TRUE`, i.e. the Koenker form. The pairing is now
  `studentize = FALSE`. With this and the `car::ncvTest()` extractor in place,
  all three baselines agree with our implementations to machine precision
  (`0`, `1.7e-16` and `1.2e-15`), so the benchmark now doubles as a live
  reference-equivalence check.
- **Fixed `simulate_power_analysis()`, whose effect size had no effect.** It
  built the variance function as `effect_size * sigma_func(x)`, but
  heteroscedasticity tests are invariant to multiplying every standard deviation
  by a constant, so every effect size produced statistically identical data and
  the reported power differed only by Monte Carlo noise. The effect size now
  interpolates between a constant variance and the supplied pattern, so `0` is
  homoscedastic and `1` is the pattern itself. Power against `sigma_linear` with
  White's test now runs 0.04 / 0.07 / 0.33 / 0.96 across effect sizes
  0 / 0.1 / 0.4 / 1.0, recovering the nominal level at zero effect.
- The Type I error test now sizes its tolerance from the Monte Carlo standard
  error rather than a fixed 0.03 margin, which at 200 replications was under two
  standard errors and failed by chance in roughly one run in seven.

- **Raised the declared minimum R version to 4.1.** `Depends: R (>= 4.0.0)` was
  unsatisfiable: `ggplot2` and `scales` are hard `Imports` and both now require
  R >= 4.1, so the package could not be installed on R 4.0 whatever its own code
  did. The minimum-version CI job checks 4.1 accordingly.
- Fixed two CI jobs that had never run successfully. The spell check called
  `spelling::spell_check_package(ignore = )`, an argument that does not exist,
  so the step errored every time; `inst/WORDLIST` is read via `use_wordlist`
  instead. The benchmark job gated on heteroTests being no more than 50% slower
  than bare `lmtest`/`car` calls, which it cannot satisfy -- it runs a validation
  layer those do not, and at n = 100 the baseline time rounds to zero, making the
  ratio infinite. The timing table is now reported as an artefact and the job
  gates on accuracy, which is the property that must not regress.

- Fixed the pkgdown deploy, which had been failing on main since before this
  release. `man/algorithms_bibliography.Rd` carried `\usage{NULL}`, which is
  what roxygen emits for a documentation-only topic and which pkgdown cannot
  parse. A topic that documents no function needs no usage section. This is
  why the package site was never published, and so why the URLs in
  `DESCRIPTION` and `inst/CITATION` returned 404.
- The automated release workflow now installs dependencies before
  `R CMD build`, which renders the vignettes and therefore needs the vignette
  builder. That step is gated on the version changing, so the omission had
  never fired until the 0.7.0 bump.

- **Fixed `generateDiagnosticReport()`, which could not generate a report.** The
  YAML front matter it writes put two mappings on one line
  (`output: html_document:`), so rmarkdown aborted with "mapping values are not
  allowed in this context". `html_document` is now nested under `output`. The
  example is wrapped in `\dontrun{}`, so only a check passing
  `--run-dontrun` executed it, which is why it went unnoticed.
- The CRAN submission job now installs a LaTeX toolchain, and the pkgdown job
  installs `rsconnect`. Both were failing on missing dependencies rather than on
  anything in the package: the former reported `pdflatex is not available`, which
  R CMD check surfaces as a probable Rd problem, and the latter needs rsconnect
  to inspect `inst/tutorials`.

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
