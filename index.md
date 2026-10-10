# heteroTests

![heteroTests project logo](assets/project-logo.png)

`heteroTests` implements a broad collection of heteroscedasticity
diagnostics for linear models in R. It includes classic tests such as
White, Breusch–Pagan and Goldfeld–Quandt along with helper functions to
visualise and mitigate heteroscedasticity.

Maintained by **Diogo Ribeiro** (<dfr@esmad.ipp.pt>, [ORCID
0009-0001-2022-7072](https://orcid.org/0009-0001-2022-7072)) at
**Faculty of Media Arts and Design, Technical University of Porto**.

## What it provides

Every test returns a base-R `htest` object, so results print, subset and
compose like those of
[`lmtest::bptest()`](https://rdrr.io/pkg/lmtest/man/bptest.html) and
slot directly into automated pipelines. Most tests take
`(model, data, ...)`; the
[roadmap](https://diogoribeiro7.github.io/heteroTests/ROADMAP.md) lists
the nine that do not yet. Where an established implementation exists,
the test reproduces it and `tests/testthat/` asserts the agreement:
`lmtest` for Breusch–Pagan, Koenker and Goldfeld–Quandt, `car` for the
Cook–Weisberg score test, Levene and Brown–Forsythe, `stats` for
Bartlett and Fligner–Killeen, `vartest` for Hartley and O’Brien, and
`plm` for the panel tests. Their simulated size and power are in
[`inst/validation/`](https://diogoribeiro7.github.io/heteroTests/inst/validation/README.md).

- **Does the variance depend on the regressors?** White
  ([`performWhiteTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTest.md)),
  the classical Breusch–Pagan test
  ([`performBPTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTest.md))
  and Koenker’s studentized form of it
  ([`performKoenkerTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTest.md)),
  Harvey
  ([`performHarveyTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performHarveyTest.md)),
  Park
  ([`performParkTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performParkTest.md)),
  Glejser
  ([`performGlejserTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performGlejserTest.md))
  and the Cook–Weisberg score test
  ([`performNCVTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performNCVTest.md),
  or
  [`performCookWeisbergTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performCookWeisbergTest.md)
  with the fitted values as the variance regressor).
  `performGlejserTest(robust = TRUE)` is the Glejser test of Im (2000)
  and of Machado and Santos Silva (2000), which keeps its level under
  skewed errors.
- **Does the variance change with the fitted values or along an
  ordering?** Goldfeld–Quandt
  ([`performGQTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performGQTest.md)),
  Szroeter
  ([`performSzroeterTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performSzroeterTest.md)),
  Spearman’s rank correlation
  ([`performSpearmanTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performSpearmanTest.md)),
  the spread–level test
  ([`performSpreadLevelTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performSpreadLevelTest.md))
  and Davidian–Carroll
  ([`performDavidianCarrollTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performDavidianCarrollTest.md)).
- **Do groups have equal variances?** Levene, Brown–Forsythe, Bartlett,
  Fligner–Killeen, Hartley’s F-max and O’Brien on the residuals of a
  model
  ([`performLeveneTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performLeveneTest.md),
  [`performBrownForsytheTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBrownForsytheTest.md),
  [`performBartlettTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBartlettTest.md),
  [`performFlignerKilleenTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performFlignerKilleenTest.md),
  [`performHartleyFmaxTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performHartleyFmaxTest.md),
  [`performOBrienTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performOBrienTest.md)),
  and Box’s M for equal covariance matrices
  ([`performBoxMTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBoxMTest.md)).
- **Does the variance of a time series depend on its past?** Engle’s
  ARCH LM test
  ([`performArchLMTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performArchLMTest.md))
  and McLeod–Li
  ([`performMcLeodLiTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performMcLeodLiTest.md)).
- **When the asymptotic reference distribution is in doubt** — a
  null-imposed wild bootstrap
  ([`performWildBootstrapTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performWildBootstrapTest.md)),
  a bootstrap White test
  ([`performWhiteTestBootstrap()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestBootstrap.md)),
  a rank-permutation test
  ([`performRankPermutationTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performRankPermutationTest.md))
  and a quantile-regression test
  ([`performQuantileRegressionTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performQuantileRegressionTest.md)).
  [`performWhiteTestRobust()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestRobust.md)
  and
  [`performBPTestRobust()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTestRobust.md)
  add optional bootstrap resampling and effect sizes, and
  [`performHighDimensionalTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performHighDimensionalTest.md)
  is a variant for designs with many regressors.
- **Panel and spatial data** —
  [`performSpatialHeteroTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performSpatialHeteroTest.md)
  asks whether the squared residuals cluster in space. For panels,
  [`runPanelTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runPanelTests.md)
  runs the Breusch–Pagan test for a random effect
  ([`performBPRandomEffectsTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPRandomEffectsTest.md))
  and Pesaran’s test of cross-sectional dependence
  ([`performPesaranTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performPesaranTest.md));
  they check the panel model, not its variance.
- **Scalability** — streaming implementations
  ([`performWhiteTestStreaming()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestStreaming.md),
  [`performBPTestStreaming()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTestStreaming.md),
  [`performKoenkerTestStreaming()`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTestStreaming.md))
  accumulate the auxiliary cross-products in chunks; results are exact
  and memory-bounded, and
  [`runHeteroTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md)
  adopts them automatically for large inputs.
- **The shape of the variance** —
  [`performVarianceFormTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performVarianceFormTest.md)
  tests whether an exponential or a power variance function describes
  the heteroscedasticity, where the other tests only establish that the
  variance is not constant.
- **Model checks that are not tests of the variance** — Ramsey’s RESET
  ([`performRESETTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performRESETTest.md)),
  variance inflation factors
  ([`performVIFDiagnostic()`](https://diogoribeiro7.github.io/heteroTests/reference/performVIFDiagnostic.md)),
  influential observations
  ([`performInfluenceDiagnostics()`](https://diogoribeiro7.github.io/heteroTests/reference/performInfluenceDiagnostics.md))
  and the correlations of the absolute residuals with chosen variables
  ([`performScatterDiagnostic()`](https://diogoribeiro7.github.io/heteroTests/reference/performScatterDiagnostic.md)).
- **Remediation and guidance** — weighted least squares
  ([`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md))
  with a choice of variance function, robust fits
  ([`fitRobust()`](https://diogoribeiro7.github.io/heteroTests/reference/fitRobust.md)),
  variance-stabilising transforms
  ([`autoTransform()`](https://diogoribeiro7.github.io/heteroTests/reference/autoTransform.md)),
  a model-comparison helper, and a recommendation engine
  ([`generateHeteroRecommendations()`](https://diogoribeiro7.github.io/heteroTests/reference/generateHeteroRecommendations.md))
  that interprets a diagnostic run.
- **Weighted fits** — on an `lm` fitted with weights the tests are
  computed from the standardized residuals, so they ask whether the
  weights are adequate. For weights estimated by
  [`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md),
  [`performVarianceFormTest()`](https://diogoribeiro7.github.io/heteroTests/reference/performVarianceFormTest.md)
  is the test that allows for the estimation.
- **Ecosystem integration** — `broom` tidiers, `ggplot2`
  theming/`autoplot`, and helpers for tidymodels and grouped pipelines.
  [`runSurveyHeteroTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runSurveyHeteroTests.md)
  runs the tests on the data of a survey design; it does not use the
  design, and its help page says when the results hold.

## Installation

Install the released version from
[CRAN](https://CRAN.R-project.org/package=heteroTests):

``` r

install.packages("heteroTests")
```

Or install the development version from GitHub:

``` r

install.packages("remotes")  # if needed
remotes::install_github("DiogoRibeiro7/heteroTests")
```

## Development setup

The package uses [`renv`](https://rstudio.github.io/renv/) to lock its
dependencies. On Debian-based systems a single command sets up the
environment. The helper verifies apt-get installs succeed and falls back
to CRAN only when network access is available:

``` bash
./setup.sh
```

This script installs R if it is missing, restores the locked package
library and fetches the development dependencies used by the test suite.
If you prefer a manual setup, install `renv` and run
[`renv::restore()`](https://rstudio.github.io/renv/reference/restore.html)
instead.

On Windows and macOS you can run a portable setup helper written in R:

``` bash
Rscript scripts/setup_helper.R
```

## Running the checks

After the environment is restored you can run all formatting, linting,
testing and coverage steps with:

``` bash
Rscript scripts/run_checks.R
```

The script prints the overall coverage percentage and writes a detailed
HTML report to `coverage/index.html`.

## Basic usage

``` r

library(heteroTests)

model <- lm(stations ~ mag + depth, quakes)

# Inspect heteroscedasticity
hd <- HeteroDiagnostic(model, quakes)
test(hd)
plot(hd)

# Choose the tests by name; listDiagnostics() returns the names
runHeteroTests(model, quakes, tests = c("white", "koenker"))

# Fit a weighted least squares model
wls <- fitWLS(model)
compareModelDiagnostics(list(model, wls))

# Was the variance function the right shape?
performVarianceFormTest(wls)
```

See `vignettes/tutorial.Rmd` and `browseVignettes("heteroTests")` for a
full walkthrough.

## Tutorials

A six-part executable course lives in
[`inst/tutorials/`](https://diogoribeiro7.github.io/heteroTests/inst/tutorials)
(Jupyter notebooks with an R kernel; see its
[README](https://diogoribeiro7.github.io/heteroTests/inst/tutorials/README.md)):

1.  **Detecting heteroscedasticity** — the cost of ignoring it, visual
    diagnosis, and the core tests.
2.  **Remediation** — robust standard errors, weighted least squares,
    transforms.
3.  **Modern & scalable diagnostics** — size control under heavy tails,
    resampling tests, streaming.
4.  **Group-wise variance tests** — and the normality trap that breaks
    Bartlett.
5.  **Time series & ARCH effects** — conditional heteroscedasticity and
    volatility clustering.
6.  **Choosing a test** — a power study distilled into a decision guide.

## Testing

The suite uses [`testthat`](https://testthat.r-lib.org/). Run it
directly with

``` r

devtools::test()        # or testthat::test_dir("tests/testthat")
```

or run the full formatting/linting/testing/coverage pipeline via
`Rscript scripts/run_checks.R`. Statistical tests assert *behaviour*
(size, power, and agreement with reference implementations such as
`lmtest` and `car`), not just object structure.

## Project structure

``` text
R/                 test implementations, remediation, streaming, recommendation engine
man/               roxygen-generated documentation
tests/testthat/    unit, property-based, and reference-comparison tests
inst/tutorials/    six-part Jupyter notebook course
vignettes/         long-form guides
paper/             R Journal manuscript and reproducible figures
scripts/           setup, checks, benchmarks, and notebook/figure builders
```

## Roadmap and limitations

The development direction, completed work and known technical debt are
tracked in
[ROADMAP.md](https://diogoribeiro7.github.io/heteroTests/ROADMAP.md).
Current known limitations include: input validation is not yet uniform
across every test.

## Contributing

Contributions are welcome! Please read
[CONTRIBUTING.md](https://diogoribeiro7.github.io/heteroTests/CONTRIBUTING.md)
for coding guidelines. Pull requests run the full check suite via GitHub
Actions, so ensure `Rscript scripts/run_checks.R` completes successfully
before submitting.

## Citation

If you use this package in your research, please cite the CRAN release
(<https://doi.org/10.32614/CRAN.package.heteroTests>), as
`citation("heteroTests")` and
[CITATION.cff](https://diogoribeiro7.github.io/heteroTests/CITATION.cff)
do. Releases are also archived on
[Zenodo](https://doi.org/10.5281/zenodo.22226790); that DOI resolves to
the latest archived version.

## License

`heteroTests` is released under the [Apache
2.0](https://diogoribeiro7.github.io/heteroTests/LICENSE) license.

## Docker

For a fully reproducible setup you can build the included `Dockerfile`:

``` bash
docker build -t hetero-tests .
docker run -it hetero-tests R
```

Scripts in `scripts/` also provide helpers such as `build-docker.sh`.
