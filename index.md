# heteroTests

[![R-CMD-check](https://github.com/DiogoRibeiro7/heteroTests/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/DiogoRibeiro7/heteroTests/actions/workflows/R-CMD-check.yml)

`heteroTests` implements a broad collection of heteroscedasticity
diagnostics for linear models in R. It includes classic tests such as
White, Breusch–Pagan and Goldfeld–Quandt along with helper functions to
visualise and mitigate heteroscedasticity.

Maintained by **Diogo Ribeiro** (<dfr@esmad.ipp.pt>, [ORCID
0009-0001-2022-7072](https://orcid.org/0009-0001-2022-7072)) at
**Faculty of Media Arts and Design, Technical University of Porto**.

## What it provides

Every test returns a base-R `htest` object and follows the same
`perform*Test(model, data, ...)` convention, so results print, subset
and compose like `stats::bptest()` and slot directly into automated
pipelines.

- **Auxiliary-regression tests** — White, classical Breusch–Pagan,
  Koenker (studentized), Harvey, Park, Glejser. The classical and
  studentized Breusch–Pagan statistics are validated against
  [`lmtest::bptest()`](https://rdrr.io/pkg/lmtest/man/bptest.html) to
  machine precision.
- **Group-wise variance tests** — Levene, Brown–Forsythe, Bartlett,
  Fligner–Killeen, Hartley’s F-max (validated against `car`).
- **Rank-based and non-constant-variance diagnostics** — Spearman,
  Cameron–Trivedi, Cook–Weisberg NCV, spread–level.
- **ARCH-type tests** for time series — Engle’s ARCH LM and McLeod–Li.
- **Modern resampling and robust diagnostics** — a null-imposed wild
  bootstrap, HC0–HC4 covariance test, quantile-regression test,
  rank-permutation test, and high-dimensional and spatial variants for
  settings where the classical asymptotics are unreliable.
- **Scalability** — streaming implementations
  ([`performWhiteTestStreaming()`](https://diogoribeiro7.github.io/heteroTests/reference/performWhiteTestStreaming.md),
  [`performBPTestStreaming()`](https://diogoribeiro7.github.io/heteroTests/reference/performBPTestStreaming.md),
  [`performKoenkerTestStreaming()`](https://diogoribeiro7.github.io/heteroTests/reference/performKoenkerTestStreaming.md))
  accumulate the auxiliary cross-products in chunks; results are exact
  and memory-bounded, and
  [`runHeteroTests()`](https://diogoribeiro7.github.io/heteroTests/reference/runHeteroTests.md)
  adopts them automatically for large inputs.
- **Remediation and guidance** — weighted least squares
  ([`fitWLS()`](https://diogoribeiro7.github.io/heteroTests/reference/fitWLS.md)),
  robust fits
  ([`fitRobust()`](https://diogoribeiro7.github.io/heteroTests/reference/fitRobust.md)),
  variance-stabilising transforms
  ([`autoTransform()`](https://diogoribeiro7.github.io/heteroTests/reference/autoTransform.md)),
  a model-comparison helper, and a recommendation engine
  ([`generateHeteroRecommendations()`](https://diogoribeiro7.github.io/heteroTests/reference/generateHeteroRecommendations.md))
  that interprets a diagnostic run.
- **Ecosystem integration** — `broom` tidiers, `ggplot2`
  theming/`autoplot`, and helpers for tidymodels, survey designs and
  grouped pipelines.

## Installation

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

# Fit a weighted least squares model
wls <- fitWLS(model)
compareModelDiagnostics(list(model, wls))
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

If you use this package in your research, please cite it as described in
[CITATION.cff](https://diogoribeiro7.github.io/heteroTests/CITATION.cff).

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
