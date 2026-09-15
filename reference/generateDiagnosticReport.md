# Automated diagnostic report generation

Creates a standalone report summarizing heteroscedasticity tests and
diagnostic plots for a fitted model.

## Usage

``` r
generateDiagnosticReport(
  model,
  data = NULL,
  output_format = "html",
  output_file = NULL,
  include_remediation = TRUE,
  include_theory = FALSE
)
```

## Arguments

- model:

  Fitted `lm` model or formula.

- data:

  Optional data frame if `model` is a formula.

- output_format:

  One of `"html"`, `"pdf"`, or `"word"`.

- output_file:

  Path to write the report to. If `NULL`, a name is generated
  automatically.

- include_remediation:

  Logical; include remediation suggestions if `TRUE`.

- include_theory:

  Logical; include background theory section.

## Value

Invisibly returns the path to the generated report.

## Examples

``` r
# \donttest{
if (requireNamespace("rmarkdown", quietly = TRUE) &&
    rmarkdown::pandoc_available()) {
  model <- lm(mpg ~ wt + hp, data = mtcars)
  generateDiagnosticReport(
    model, mtcars,
    output_file = file.path(tempdir(), "diagnostic_report.html")
  )
}
#> [INFO] Running White test
#> [INFO] White test completed: statistic = 6.5431 df = 5 p = 0.2569
#> [INFO] Running Breusch-Pagan test
#> Report generated: /tmp/RtmpQiQgGc/diagnostic_report.html
# }
```
