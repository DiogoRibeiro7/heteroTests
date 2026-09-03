## Test environments

- Local: Windows 11, R 4.5.1
- GitHub Actions: Ubuntu (R-devel, R-release, R-oldrel-1), macOS (R-release)
  and Windows (R-release)
- GitHub Actions: a separate Ubuntu 22.04 job on R 4.1 checks the minimum
  version declared in `Depends:`

## R CMD check results

`R CMD check` is run on every push via GitHub Actions. Locally the package builds
and installs cleanly and the `testthat` suite passes under edition 3.

A full `R CMD check --as-cran` requires all Suggested packages to be installed;
where some optional Suggests are unavailable locally, the check is run with
`_R_CHECK_FORCE_SUGGESTS_=false` and all code paths that use them are guarded by
`requireNamespace()`.

Known notes we are aware of and addressing:

- Examples, vignettes and tutorials use R's built-in `quakes` dataset, so the
  package redistributes no third-party data. The two datasets it does ship,
  `diagnostic_data` and `hetero_data`, are simulated; their generating scripts
  are under `data-raw/`.
- The package intentionally ships a large optional surface (visualisation,
  reporting and ecosystem-integration helpers) behind `Suggests`; each is exercised
  only when the corresponding package is installed.

## Downstream dependencies

There are currently no reverse dependencies on CRAN.
