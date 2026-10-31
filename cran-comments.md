## Submission

This is an update of heteroTests from 0.16.0, published on CRAN on
29 October, to 0.17.0. It fixes the failures that CRAN's additional check
with Intel MKL as BLAS reported on 0.16.0
(<https://www.stats.ox.ac.uk/pub/bdr/Rblas/MKL/heteroTests.out>), which is
why it comes two days after 0.16.0.

The MKL check failed 179 expectations of the test suite. Nearly all of them
compared two computations of the same numbers bit for bit, which under MKL
differ by 1e-14 to 1e-12 relative. On CRAN (`NOT_CRAN` unset), the tests now
compare such numbers to a tolerance: 1e-10 for most of them, and 1e-8 or on
another scale for a few whose computation amplifies the last bits. The few
expectations whose outcome the last bits decide (a tie, a rank, a cache hit,
the count behind a permutation p-value) run off CRAN only. One of them is
the Fligner-Killeen comparison that differed more under MKL (8.133 against
8.149): with tied absolute residuals, the ranking that test uses can depend
on the last bits of the residuals. That fragility of the test itself is
recorded, to be fixed in the next release.

The package's CI now runs `R CMD check --as-cran` on R-devel with
`NOT_CRAN` unset, as CRAN's additional checks do, under MKL 2026.2,
OpenBLAS, BLIS and R without long doubles, in the R-hub containers, on every
pull request that changes the package. MKL on GitHub's runners does not
reproduce the bits of CRAN's Intel machine, so the same check also runs
under a BLAS that moves each result by up to one unit in the last place,
with three seeds.

The release also carries the rest of the development since 0.16.0: the
resampling and robust tests read the stored fit as the other tests do, fits
with `na.action = na.exclude` are accepted, and `MASS::rlm()` fits and, in
the tests that need least squares, penalised `mgcv::gam()` fits are refused.
There are no export, argument or default changes. NEWS.md lists every
change.

## Test environments

* GitHub Actions, on the tree tagged v0.17.0: Ubuntu (R-devel, R-release,
  R-oldrel-1), macOS (R-release) and Windows (R-release), Status OK on each.
  R-devel was R Under development (unstable) (2026-10-29 r90794).
* GitHub Actions, Ubuntu on R 4.1.3, the minimum version declared in
  `Depends:`, with the hard dependencies only: Status 2 NOTEs, both from the
  Suggested packages that job does not install (they are not available for
  checking, and neither is `lmtest` for the Rd cross-references).
* GitHub Actions, CRAN's additional checks as above: `R CMD check --as-cran`
  on R-devel (2026-10-29 r90794) under MKL, OpenBLAS, BLIS, noLD and the
  perturbing BLAS with three seeds. Status OK on each, with no failed test.
* GitHub Actions, on the source tarball submitted here:
  `R CMD check --as-cran` on Ubuntu (R-release) with all Suggested packages
  installed. The release workflow refuses to tag a version that gives a
  warning or an error in this check.
* win-builder, R-release (R 4.6.2, Windows Server 2022): Status 1 NOTE, the
  days since the last update. The check took 403 s, of which the tests took
  205 s.
* win-builder, R-devel (R Under development (unstable) (2026-10-30 r90805
  ucrt)): Status 1 NOTE, the same. The check took 393 s, of which the tests
  took 211 s.
* macOS builder (mac.r-project.org): not run. Its upload service answered
  "unavailable" to each of eight attempts over two hours on 31 October.
  macOS is covered by the GitHub Actions check on macOS (R-release) above,
  Status OK.

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Days since last update: 2

  This update fixes the MKL failures of 0.16.0, as explained above.

The GitHub Actions `--as-cran` check also gave a NOTE on the HTML manual.
It came from the runner, which has neither HTML Tidy nor the V8 package, so
that check was skipped rather than failed; win-builder checked the HTML
manual without a note.

The installed size is 5.5 MB, which R-devel reports as information. The
sub-directories over 1 MB are `R` (1.6 MB), `doc` (1.2 MB, the vignettes) and
`validation` (1.0 MB). The `validation` directory holds the simulation
scripts behind the package's statistical claims, with their results: the
size and power of each test. Users can rerun them on their own installation.

The tests take about three and a half minutes on win-builder (205 s and
211 s). The examples take 27 s and 32 s.

## Downstream dependencies

There are currently no reverse dependencies on CRAN.
