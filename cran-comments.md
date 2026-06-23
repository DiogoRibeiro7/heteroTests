## Test environments

- Local: Windows 11, R 4.5.1
- GitHub Actions: Ubuntu, macOS and Windows across R 4.0–4.4 (release and oldrel)

## R CMD check results

`R CMD check` is run on every push via GitHub Actions. Locally the package builds
and installs cleanly and the `testthat` suite passes under edition 3.

A full `R CMD check --as-cran` requires all Suggested packages to be installed;
where some optional Suggests are unavailable locally, the check is run with
`_R_CHECK_FORCE_SUGGESTS_=false` and all code paths that use them are guarded by
`requireNamespace()`.

Known notes we are aware of and addressing:

- The bundled example dataset `boston_housing` is a verbatim copy of
  `MASS::Boston`, shipped so the examples, tests and tutorials run without
  attaching MASS. Its provenance is documented in `?boston_housing`. The datasets
  are stored as `.rda` files under `data/` (generation scripts live in
  `data-raw/`).
- The package intentionally ships a large optional surface (visualisation,
  reporting and ecosystem-integration helpers) behind `Suggests`; each is exercised
  only when the corresponding package is installed.

## Downstream dependencies

There are currently no reverse dependencies on CRAN.
