## Resubmission

This is a resubmission. The version is increased from 0.11.1 to 0.11.2. In
response to the review:

* Removed the redundant "in R" from the Description field, together with a
  second instance of the same redundancy ("common R modelling tools").

* Added references for the methods the package implements to the Description
  field, in the form `authors (year) <doi:...>`.

* The `:::` calls flagged in `rhandleMissingValues.Rd`,
  `rTEST_REQUIREMENTS.Rd`, `rvalidateDataInputs.Rd`,
  `rvalidateDistributionalAssumptions.Rd`, `rvalidateGroupingVariable.Rd` and
  `rvalidateModelInputs.Rd` all refer to unexported objects documented with
  `\keyword{internal}`. Replacing `:::` with `::` would make those examples
  fail, so the examples have been removed from these internal help pages
  instead. The objects remain unexported, and no example in the package now
  uses `:::`.

* Replaced `\dontrun{}` in all three examples that used it:
  * `generateHeteroRecommendations()` runs in well under a second, so its
    example is now unwrapped.
  * `generateDiagnosticReport()` takes about ten seconds and requires pandoc,
    so its example is wrapped in `\donttest{}`, guarded by
    `rmarkdown::pandoc_available()`, and writes its output to `tempdir()`.
  * `launchDiagnosticDashboard()` returns a Shiny application that starts an
    interactive server when printed, so its example is guarded by
    `if (interactive())` rather than wrapped in `\dontrun{}`.

* Not raised in the review, but the same concern: with its `\donttest{}` part
  included, the `performWhiteTestBootstrap()` example ran for just over five
  seconds, so its bootstrap replications there are reduced from 999 to 499.
  It now runs in under two seconds.

## Test environments

- Local: Windows 11, R 4.5.1
- GitHub Actions: Ubuntu (R-devel, R-release, R-oldrel-1), macOS (R-release)
  and Windows (R-release)
- GitHub Actions: a separate Ubuntu 22.04 job on R 4.1 checks the minimum
  version declared in `Depends:`

## R CMD check results

0 errors | 0 warnings | 0 notes

From `R CMD check --as-cran --run-donttest` on the 0.11.2 source tarball, locally on Windows 11 with . The run set `_R_CHECK_CRAN_INCOMING_REMOTE_=false`, so the remote incoming checks were not part of it; the DOIs added to the Description were resolved separately against doi.org. The incoming check is expected to add the usual note for a new submission.

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
