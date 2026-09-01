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
if (FALSE) { # \dontrun{
data(mtcars)
model <- lm(mpg ~ wt + hp, data = mtcars)
generateDiagnosticReport(model, mtcars)
} # }
```
