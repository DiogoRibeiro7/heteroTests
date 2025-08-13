# AGENTS.md

## 1. Environment

* Ensure R is installed and available as `Rscript`. On Debian systems you can
  install it with:

  ```bash
  sudo apt-get update && sudo apt-get install -y r-base
  ```

* Ensure the setup script is executable (only once):

  ```bash
  chmod +x setup.sh
  ```
* Run the project bootstrap:

  ```bash
  ./setup.sh
  ```
    This will automatically install R if missing, load **renv**, and restore your locked package library without prompting.
* (Optional) Check status:

  ```bash
  Rscript -e "renv::status()"
  ```

## 2. Code Style

* Format R code with **styler**:

  ```bash
  Rscript -e "styler::style_dir('R/')"
  ```
* Lint with **lintr**:

  ```bash
  Rscript -e "lintr::lint_dir('R/')"
  ```

## 3. Testing

* Run all tests via **testthat**:

  ```bash
  Rscript -e "testthat::test_dir('tests/')"
  ```
* Generate coverage report (requires **covr**):

  ```bash
  Rscript -e "covr::report()"
  ```

* Or run all checks in one step (requires the `quickcheck` package
  used by some tests; `./setup.sh` installs it automatically):

```bash
Rscript scripts/run_checks.R
```

## 4. PR Guidelines

* Title template:

  ```
  [pkg-name] Brief description
  ```
* In the body include:

  1. **Summary** – one-line overview of the change
  2. **Testing Done** – list of examples or test outputs
  3. **Package Version** – bump in DESCRIPTION if API changes

## 5. Project Notes

* R code lives under `R/`; documentation under `man/`
* Raw data and fixtures in `data-raw/`; processed data in `data/`
* Use `devtools::document()` to rebuild NAMESPACE and Rd files
* Deployment scripts in `scripts/` (e.g. CI, reports)
* Keep this file updated with any new project guidelines.
