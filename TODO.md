# Planned Enhancements

Most items from the original TODO list are complete, but some remain in progress:

* Expand examples in the README and vignette with real datasets. **(done)**
* Unit test coverage now includes all `perform*` functions. Continue
  adding tests as new helpers are introduced.
* Investigate further diagnostics or comparisons between tests. **(done: added `compareTestResults()` helper and WLS comparison test)**
* Provide a helper script to install system dependencies and restore the package library. **(done)**
* Document code style expectations with a `.lintr` config so contributors can
  run `lintr::lint_package()` **(done)**.

Future work:
* Publish a prebuilt Docker image to speed up trial and CI setups.
* Expand the tutorial vignette into a full case study with an end-to-end
  workflow on a larger dataset.
* Provide cross-platform setup scripts for Windows and macOS users. **(done)**
* Explore a command-line interface for running the basic diagnostics.
* Detect a missing `renv` directory in `setup.sh` and automatically
  initialize the project before restoring packages. **(open)**

* Fallback in `setup.sh` to run `renv::init()` when the environment was not
  created during bootstrap. **(open)**

* Ensure `setup.sh` creates a minimal renv library when package installation
  fails so subsequent commands can run. **(open)**

* Verify `setup.sh` writes `renv/activate.R` after initialization so future
  sessions load the environment correctly. **(open)**

* Verify `setup.sh` bootstraps renv without network connectivity. **(open)**

* Resolve failing tests caused by missing dependencies in CI environment. **(open)**
* Provide an apt-get fallback in the bootstrap script so required packages are installed when compilation fails. **(open)**
* Ensure setup.sh verifies apt-get success and warns on failure. **(open)**
* Add apt-get helper that checks network connectivity before installing packages. **(open)**
* Diagnose `renv::restore` failures related to data.table and ggplot2 so the lockfile packages install cleanly. **(open)**
* Set up a GitHub Actions workflow to run `scripts/run_checks.R` and collect coverage. **(open)**
