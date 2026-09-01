# Contributing to heteroTests

We welcome contributions in the form of pull requests or issue reports.

## Workflow

1. Fork the repository and create feature branches from `main`.
2. Restore development dependencies by running `./setup.sh` and then execute `R CMD check --no-manual`.
3. Build the docs with `pkgdown::build_site()` to verify examples render.
4. Add unit tests for any new functionality and update relevant docs.

To run style checks, unit tests and coverage in one step, first run
`./setup.sh` to install the development packages (including
`quickcheck`) and then execute:

```bash
Rscript scripts/run_checks.R
```

Run `lintr::lint_package()` to ensure the code matches the `.lintr` style
configuration before submitting.

Code should follow the tidyverse style guide and respect CRAN policies
(no unchecked warnings or notes from `R CMD check`).

All R source lives directly in `R/`, one file per diagnostic, named after the
function it defines (for example `R/performBPTest.R`). There are no
subdirectories.

### Documentation, and a warning about roxygen2

`man/` is **mixed**: roxygen2 generated some pages and the rest were written
by hand. Running `roxygen2::roxygenise()` would overwrite the hand-written
pages with stubs built from the roxygen comments and silently lose their
content, so do not run it across the package.

When you change a function signature, update its `.Rd` file in the same commit.
Nothing regenerates it for you. `tests/testthat/test-documentation-consistency.R`
runs `tools::codoc()`, `tools::checkDocFiles()` and `tools::undoc()` so the
local test suite catches the mismatch before CI does.

If ``R CMD check`` fails with ``command not found`` you need to
install R and ensure the ``R`` executable is available. Consult your
package manager or visit <https://cran.r-project.org> for
installation instructions.

Continuous integration runs `R CMD check` automatically for every
pull request via GitHub Actions, so make sure the checks pass locally
before submitting.

Please open an issue to discuss substantial changes or cross-language
API additions. Pull requests should include a concise description in the
body and reference any related issues.
