## Submission

This is an update of heteroTests from 0.11.2, the version on CRAN, to 0.16.0.
The versions in between were released on GitHub only.

The main changes:

* The tests of a model now read the response, the design, the residuals and
  the weights from one validated object built from the stored fit. They match
  the rows of `data` to the observations of the fit, so they no longer depend
  on the order of `data`.
* Several statistics are corrected: the degrees of freedom of White's test
  and the Fligner-Killeen, Box's M, Szroeter, Pesaran and quantile regression
  tests.
* Every test takes `(model, data, ...)`. The old call forms still work,
  warn with the call to use instead, and will be removed in 1.0.0.
* The resampling tests draw 999 replicates by default, and the default
  battery of `runHeteroTests()` is White and Koenker.

NEWS.md lists every change, release by release.

## Test environments

* GitHub Actions, on the commit tagged v0.16.0: Ubuntu (R-devel, R-release,
  R-oldrel-1), macOS (R-release), Windows (R-release), and Ubuntu 22.04 on
  R 4.1, the minimum version declared in `Depends:`. R-devel was
  R Under development (unstable) (2026-10-25 r90763): Status OK, with no
  notes.
* GitHub Actions, on the source tarball submitted here:
  `R CMD check --as-cran` on Ubuntu (R-release) with all Suggested packages
  installed. The release workflow refuses to tag a version that gives a
  warning or an error in this check.
* win-builder, R-release (R 4.6.1, Windows Server 2022): Status OK, with no
  notes. The check took 565 s, of which the tests took 283 s.
* win-builder, R-devel: no result. The tarball was uploaded three times on
  28 and 29 October; each upload waited in the R-devel queue and no e-mail
  came back, while the R-release queue answered within the hour. R-devel
  is covered by the GitHub Actions check above.
* macOS builder (mac.r-project.org): R 4.6.1 Patched, macOS 26.6 on arm64.
  Status OK, with no notes. The check took 3 min 27 s, of which the tests
  took 107 s.

## R CMD check results

0 errors | 0 warnings | 0 notes

The GitHub Actions `--as-cran` check gave a NOTE on the HTML manual.
It came from the runner, which has neither HTML Tidy nor the V8 package, so
that check was skipped rather than failed.

None of these checks noted the installed size, but in case CRAN's do: it is
5.3 MB. The sub-directories over 1 MB are `R`
(1.5 MB), `doc` (1.2 MB, the vignettes) and `validation` (1.0 MB). The
`validation` directory holds the simulation scripts behind the package's
statistical claims, with their results: the size and power of each test.
Users can rerun them on their own installation.

The test suite takes about four minutes on GitHub Actions (238 s on Ubuntu,
R-release). The examples take 26 s, or 33 s with `--run-donttest`.

## Downstream dependencies

There are currently no reverse dependencies on CRAN.
