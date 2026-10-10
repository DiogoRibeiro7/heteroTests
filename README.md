<p align="center">
  <img src="assets/project-logo.png" alt="heteroTests project logo" width="160" height="160">
</p>

# heteroTests

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/heteroTests)](https://CRAN.R-project.org/package=heteroTests)
[![R-CMD-check](https://github.com/DiogoRibeiro7/heteroTests/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/DiogoRibeiro7/heteroTests/actions/workflows/R-CMD-check.yml)
<!-- badges: end -->

`heteroTests` implements a broad collection of heteroscedasticity diagnostics for linear models in R. It includes classic tests such as White, Breusch--Pagan and Goldfeld--Quandt along with helper functions to visualise and mitigate heteroscedasticity.

Maintained by **Diogo Ribeiro** (<dfr@esmad.ipp.pt>, [ORCID 0009-0001-2022-7072](https://orcid.org/0009-0001-2022-7072)) at **Faculty of Media Arts and Design, Technical University of Porto**.

## What it provides

Every test returns a base-R `htest` object, so results print, subset and
compose like those of `lmtest::bptest()` and slot directly into automated
pipelines. Most tests take `(model, data, ...)`; the [roadmap](ROADMAP.md)
lists the nine that do not yet. Where an established implementation exists, the
test reproduces it and `tests/testthat/` asserts the agreement: `lmtest` for
Breusch--Pagan, Koenker and Goldfeld--Quandt, `car` for the Cook--Weisberg
score test, Levene and Brown--Forsythe, `stats` for Bartlett and
Fligner--Killeen, `vartest` for Hartley and O'Brien, and `plm` for the panel
tests. Their simulated size and power are in
[`inst/validation/`](inst/validation/README.md).

- **Does the variance depend on the regressors?** White
  (`performWhiteTest()`), the classical Breusch--Pagan test (`performBPTest()`)
  and Koenker's studentized form of it (`performKoenkerTest()`), Harvey
  (`performHarveyTest()`), Park (`performParkTest()`), Glejser
  (`performGlejserTest()`) and the Cook--Weisberg score test
  (`performNCVTest()`, or `performCookWeisbergTest()` with the fitted values as
  the variance regressor). `performGlejserTest(robust = TRUE)` is the Glejser
  test of Im (2000) and of Machado and Santos Silva (2000), which keeps its
  level under skewed errors.
- **Does the variance change with the fitted values or along an ordering?**
  Goldfeld--Quandt (`performGQTest()`), Szroeter (`performSzroeterTest()`),
  Spearman's rank correlation (`performSpearmanTest()`), the spread--level test
  (`performSpreadLevelTest()`) and Davidian--Carroll
  (`performDavidianCarrollTest()`).
- **Do groups have equal variances?** Levene, Brown--Forsythe, Bartlett,
  Fligner--Killeen, Hartley's F-max and O'Brien on the residuals of a model
  (`performLeveneTest()`, `performBrownForsytheTest()`, `performBartlettTest()`,
  `performFlignerKilleenTest()`, `performHartleyFmaxTest()`,
  `performOBrienTest()`), and Box's M for equal covariance matrices
  (`performBoxMTest()`).
- **Does the variance of a time series depend on its past?** Engle's ARCH LM
  test (`performArchLMTest()`) and McLeod--Li (`performMcLeodLiTest()`).
- **When the asymptotic reference distribution is in doubt** — a null-imposed
  wild bootstrap (`performWildBootstrapTest()`), a bootstrap White test
  (`performWhiteTestBootstrap()`), a rank-permutation test
  (`performRankPermutationTest()`) and a quantile-regression test
  (`performQuantileRegressionTest()`). `performWhiteTestRobust()` and
  `performBPTestRobust()` add optional bootstrap resampling and effect sizes,
  and `performHighDimensionalTest()` is a variant for designs with many
  regressors.
- **Panel and spatial data** — `performSpatialHeteroTest()` asks whether the
  squared residuals cluster in space. For panels, `runPanelTests()` runs the
  Breusch--Pagan test for a random effect (`performBPRandomEffectsTest()`) and
  Pesaran's test of cross-sectional dependence (`performPesaranTest()`); they
  check the panel model, not its variance.
- **Scalability** — streaming implementations (`performWhiteTestStreaming()`,
  `performBPTestStreaming()`, `performKoenkerTestStreaming()`) accumulate the
  auxiliary cross-products in chunks; results are exact and memory-bounded, and
  `runHeteroTests()` adopts them automatically for large inputs.
- **The shape of the variance** — `performVarianceFormTest()` tests whether an
  exponential or a power variance function describes the heteroscedasticity,
  where the other tests only establish that the variance is not constant.
- **Model checks that are not tests of the variance** — Ramsey's RESET
  (`performRESETTest()`), variance inflation factors (`performVIFDiagnostic()`),
  influential observations (`performInfluenceDiagnostics()`) and the
  correlations of the absolute residuals with chosen variables
  (`performScatterDiagnostic()`).
- **Remediation and guidance** — weighted least squares (`fitWLS()`) with a
  choice of variance function, robust fits (`fitRobust()`),
  variance-stabilising transforms (`autoTransform()`), a model-comparison
  helper, and a recommendation engine (`generateHeteroRecommendations()`) that
  interprets a diagnostic run.
- **Weighted fits** — on an `lm` fitted with weights the tests are computed
  from the standardized residuals, so they ask whether the weights are
  adequate. For weights estimated by `fitWLS()`, `performVarianceFormTest()`
  is the test that allows for the estimation.
- **Ecosystem integration** — `broom` tidiers, `ggplot2` theming/`autoplot`, and
  helpers for tidymodels and grouped pipelines. `runSurveyHeteroTests()` runs
  the tests on the data of a survey design; it does not use the design, and its
  help page says when the results hold.

## Installation

Install the released version from [CRAN](https://CRAN.R-project.org/package=heteroTests):

```r
install.packages("heteroTests")
```

Or install the development version from GitHub:

```r
install.packages("remotes")  # if needed
remotes::install_github("DiogoRibeiro7/heteroTests")
```

## Development setup

The package uses [`renv`](https://rstudio.github.io/renv/) to lock its dependencies. On Debian-based systems a single command sets up the environment. The helper verifies apt-get installs succeed and falls back to CRAN only when network access is available:

```bash
./setup.sh
```

This script installs R if it is missing, restores the locked package library and fetches the development dependencies used by the test suite. If you prefer a manual setup, install `renv` and run `renv::restore()` instead.

On Windows and macOS you can run a portable setup helper written in R:

```bash
Rscript scripts/setup_helper.R
```

## Running the checks

After the environment is restored you can run all formatting, linting, testing and coverage steps with:

```bash
Rscript scripts/run_checks.R
```

The script prints the overall coverage percentage and writes a detailed HTML report to `coverage/index.html`.

## Basic usage

```r
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

See `vignettes/tutorial.Rmd` and `browseVignettes("heteroTests")` for a full walkthrough.

## Tutorials

A six-part executable course lives in [`inst/tutorials/`](inst/tutorials) (Jupyter
notebooks with an R kernel; see its [README](inst/tutorials/README.md)):

1. **Detecting heteroscedasticity** — the cost of ignoring it, visual diagnosis, and the core tests.
2. **Remediation** — robust standard errors, weighted least squares, transforms.
3. **Modern & scalable diagnostics** — size control under heavy tails, resampling tests, streaming.
4. **Group-wise variance tests** — and the normality trap that breaks Bartlett.
5. **Time series & ARCH effects** — conditional heteroscedasticity and volatility clustering.
6. **Choosing a test** — a power study distilled into a decision guide.

## Testing

The suite uses [`testthat`](https://testthat.r-lib.org/). Run it directly with

```r
devtools::test()        # or testthat::test_dir("tests/testthat")
```

or run the full formatting/linting/testing/coverage pipeline via
`Rscript scripts/run_checks.R`. Statistical tests assert *behaviour* (size, power,
and agreement with reference implementations such as `lmtest` and `car`), not just
object structure.

## Project structure

```text
R/                 test implementations, remediation, streaming, recommendation engine
man/               roxygen-generated documentation
tests/testthat/    unit, property-based, and reference-comparison tests
inst/tutorials/    six-part Jupyter notebook course
vignettes/         long-form guides
paper/             R Journal manuscript and reproducible figures
scripts/           setup, checks, benchmarks, and notebook/figure builders
```

## Roadmap and limitations

The development direction, completed work and known technical debt are tracked in
[ROADMAP.md](ROADMAP.md). Current known limitations include: input validation is
not yet uniform across every test.

## Contributing

Contributions are welcome! Please read [CONTRIBUTING.md](CONTRIBUTING.md) for coding guidelines. Pull requests run the full check suite via GitHub Actions, so ensure `Rscript scripts/run_checks.R` completes successfully before submitting.

## Citation

If you use this package in your research, please cite the CRAN release
(<https://doi.org/10.32614/CRAN.package.heteroTests>), as `citation("heteroTests")`
and [CITATION.cff](CITATION.cff) do. Releases are also archived on
[Zenodo](https://doi.org/10.5281/zenodo.22226790); that DOI resolves to the
latest archived version.

## License

`heteroTests` is released under the [Apache 2.0](LICENSE) license.

## Docker

For a fully reproducible setup you can build the included `Dockerfile`:

```bash
docker build -t hetero-tests .
docker run -it hetero-tests R
```

Scripts in `scripts/` also provide helpers such as `build-docker.sh`.
