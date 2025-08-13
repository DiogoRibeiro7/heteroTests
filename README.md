# heteroTests
[![R-CMD-check](https://github.com/DiogoRibeiro7/heteroTests/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/DiogoRibeiro7/heteroTests/actions/workflows/R-CMD-check.yml)

`heteroTests` implements a broad collection of heteroscedasticity diagnostics for linear models in R. It includes classic tests such as White, Breusch--Pagan and Goldfeld--Quandt along with helper functions to visualise and mitigate heteroscedasticity.

Maintained by **Diogo Ribeiro** (<dfr@esmad.ipp.pt>, [ORCID 0009-0001-2022-7072](https://orcid.org/0009-0001-2022-7072)) at **ESMAD - Instituto Politécnico do Porto**.

## Installation

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

data(boston_housing, package = "heteroTests")
model <- lm(medv ~ lstat + rm + crim, boston_housing)

# Inspect heteroscedasticity
hd <- HeteroDiagnostic(model, boston_housing)
test(hd)
plot(hd)

# Fit a weighted least squares model
wls <- fitWLS(model)
compareModelDiagnostics(list(model, wls))
```

See `vignettes/tutorial.Rmd` and `browseVignettes("heteroTests")` for a full walkthrough.

## Contributing

Contributions are welcome! Please read [CONTRIBUTING.md](CONTRIBUTING.md) for coding guidelines. Pull requests run the full check suite via GitHub Actions, so ensure `Rscript scripts/run_checks.R` completes successfully before submitting.

## Citation

If you use this package in your research, please cite it as described in [CITATION.cff](CITATION.cff).

## License

`heteroTests` is released under the [Apache 2.0](LICENSE) license.

## Docker

For a fully reproducible setup you can build the included `Dockerfile`:

```bash
docker build -t hetero-tests .
docker run -it hetero-tests R
```

Scripts in `scripts/` also provide helpers such as `build-docker.sh`.
