#' Automated diagnostic report generation
#'
#' Creates a standalone report summarizing heteroscedasticity tests and
#' diagnostic plots for a fitted model.
#'
#' @param model Fitted \code{lm} model or formula.
#' @param data Optional data frame if \code{model} is a formula.
#' @param output_format One of \code{"html"}, \code{"pdf"}, or \code{"word"}.
#' @param output_file Path to write the report to. If \code{NULL}, a name is
#'   generated automatically.
#' @param include_remediation Logical; include remediation suggestions if \code{TRUE}.
#' @param include_theory Logical; include background theory section.
#' @return Invisibly returns the path to the generated report.
#' @examples
#' \dontrun{
#' data(mtcars)
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' generateDiagnosticReport(model, mtcars)
#' }
#' @export
generateDiagnosticReport <- function(model, data = NULL,
                                     output_format = "html",
                                     output_file = NULL,
                                     include_remediation = TRUE,
                                     include_theory = FALSE) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("rmarkdown package required for report generation")
  }

  if (inherits(model, "formula")) {
    if (is.null(data)) stop("`data` must be supplied when `model` is a formula")
    checkData(data)
    model <- lm(model, data = data)
  } else {
    checkModel(model)
    if (is.null(data)) {
      data <- model.frame(model)
    } else {
      checkData(data)
    }
  }

  report_template <- create_report_template(include_remediation, include_theory)
  temp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(report_template, temp_rmd)

  test_results <- runHeteroTests(model, data)
  plots <- plotDiagnosticSuiteEnhanced(model)

  if (include_remediation) {
    remediation <- suggestRemediation(test_results)
  } else {
    remediation <- NULL
  }

  if (is.null(output_file)) {
    output_file <- paste0(
      "diagnostic_report_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".",
      output_format
    )
  }

  rmarkdown::render(
    input = temp_rmd,
    output_format = switch(output_format,
      html = rmarkdown::html_document(toc = TRUE, toc_float = TRUE),
      pdf = rmarkdown::pdf_document(toc = TRUE),
      word = rmarkdown::word_document(toc = TRUE)
    ),
    output_file = output_file,
    params = list(
      model = model,
      tests = test_results,
      plots = plots,
      remediation = remediation,
      timestamp = Sys.time()
    ),
    quiet = TRUE
  )

  message("Report generated: ", output_file)
  invisible(output_file)
}

create_report_template <- function(include_remediation = TRUE,
                                   include_theory = FALSE) {
  template <- c(
    "---",
    "title: 'Heteroscedasticity Diagnostic Report'",
    "output: html_document:\n  toc: true\n  toc_float: true\n  theme: flatly",
    "params:\n  model: NULL\n  tests: NULL\n  plots: NULL\n  remediation: NULL\n  timestamp: NULL",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
    "library(ggplot2)",
    "library(heteroTests)",
    "```",
    "",
    "## Summary",
    "",
    "Model formula: `r deparse(formula(params$model))`  ",
    "Sample size: `r nobs(params$model)`  ",
    "Generated on: `r params$timestamp`",
    "",
    "```{r summary-table}",
    "test_summary <- data.frame(",
    "  Test = names(params$tests),",
    "  Statistic = sapply(params$tests, function(x) round(x$statistic, 4)),",
    "  P_Value = sapply(params$tests, function(x) round(x$p.value, 4))",
    ")",
    "knitr::kable(test_summary, caption = 'Heteroscedasticity Test Results')",
    "```",
    "",
    "```{r plots, fig.width=12, fig.height=8}",
    "gridExtra::grid.arrange(grobs = params$plots[1:4], ncol = 2)",
    "```"
  )

  if (include_remediation) {
    template <- c(
      template,
      "",
      "## Remediation",
      "",
      "```{r remediation}",
      "if (!is.null(params$remediation)) {",
      "  print(params$remediation)",
      "} else {",
      "  cat('No remediation suggestions available.')",
      "}",
      "```"
    )
  }

  if (include_theory) {
    template <- c(
      template,
      "",
      "## Background",
      "Heteroscedasticity occurs when the variance of the residuals is not constant.",
      "This violates the assumptions of ordinary least squares regression.",
      ""
    )
  }

  paste(template, collapse = "\n")
}
