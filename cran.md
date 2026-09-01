# How to Publish an R Package on CRAN

This guide provides a step-by-step workflow to prepare and submit your R
package to CRAN. It follows best practices and common requirements.

## 1. Prepare your package structure

1.  Use a standard layout:

    ``` bash
    yourpkg/
    ├── DESCRIPTION
    ├── NAMESPACE
    ├── R/
    ├── man/
    ├── tests/
    ├── vignettes/
    └── README.md
    ```

2.  Fill out **DESCRIPTION** with all required fields:

    - `Package`, `Version`, `Title`, `Description`
    - `Authors@R` (use [`person()`](https://rdrr.io/r/utils/person.html)
      with roles)
    - `License`, `Depends`/`Imports`/`Suggests`, `URL`, `BugReports`

3.  Document your functions with **roxygen2** tags (`@param`, `@return`,
    `@examples`, etc.).

    - Run `roxygen2::roxygenise()` to generate `.Rd` files in **man/**.

------------------------------------------------------------------------

## 2. Add tests and vignettes

- **Unit tests** with **testthat** under **tests/testthat/**.
- A **vignette** (R Markdown) under **vignettes/** showing key usage.
- Ensure `Suggests: testthat, knitr, rmarkdown` in DESCRIPTION.

These help CRAN reviewers verify that your package works and is
documented.

------------------------------------------------------------------------

## 3. Run local checks

1.  Install development helpers:

    ``` r

    install.packages(c("roxygen2", "testthat", "rcmdcheck"))
    ```

2.  From the package root, run:

    ``` r

    rcmdcheck::rcmdcheck(args = "--as-cran")
    ```

    or at the shell:

    ``` bash
    R CMD check yourpkg_0.1.0.tar.gz --as-cran
    ```

3.  Fix **all** notes, warnings, and errors. CRAN is strict about NOTES
    and WARNINGS.

------------------------------------------------------------------------

## 4. Build the source tarball

Once `R CMD check` is clean:

``` bash
R CMD build yourpkg
```

This produces `yourpkg_0.1.0.tar.gz`.

------------------------------------------------------------------------

## 5. Submit to CRAN

1.  Log in or create a CRAN account at <https://cran.r-project.org/>.
2.  Go to the “Submit” page: <https://cran.r-project.org/submit.html>.
3.  Provide your tarball, a brief submission message, and explain any
    deliberate NOTE or WARNING you couldn’t avoid.
4.  Upload and confirm.

CRAN will send you an email acknowledging receipt.

------------------------------------------------------------------------

## 6. Respond to review

- Expect questions or requests for small fixes.
- Reply promptly and courteously.
- Update your package, increment the `Version` field (e.g., `0.1.0.9000`
  → `0.1.1`), rebuild, and resubmit.

A clear and polite response speeds approval.

------------------------------------------------------------------------

## 7. After acceptance

- Your package will appear on CRAN within a day or two.
- CRAN runs daily checks and reports.
- Monitor incoming check results at
  <https://cran.r-project.org/web/checks/check_results_yourpkg.html>.

------------------------------------------------------------------------

## Tips & Best Practices

- Keep your `Imports:` minimal.
- Avoid external system dependencies unless essential.
- Use continuous integration (e.g., GitHub Actions) with style checks.
- Maintain a `NEWS.md` and bump version for each change.
