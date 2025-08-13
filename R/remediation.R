#' Suggest remediation actions for heteroscedasticity
#'
#' Given diagnostic test results from \code{runHeteroTests()}, this helper
#' provides a basic summary of recommended follow-up steps. It evaluates the
#' number of significant tests and proposes variance stabilising
#' transformations or modelling approaches.
#'
#' @param diagnostic_results Named list of \code{htest} objects as returned by
#'   \code{runHeteroTests()}.
#'
#' @details
#' The function counts how many diagnostic tests yield a p-value below 0.05.
#' If none are significant it returns a brief conclusion that no action is
#' needed. Otherwise a severity level is assigned and appropriate
#' transformations or variance modelling approaches are suggested.
#'
#' @return An object of class \code{remediation_suggestions} containing a summary
#'   of potential actions.
#'
#' @examples
#' data(mtcars)
#' mod <- lm(mpg ~ wt + qsec, data = mtcars)
#' res <- runHeteroTests(mod, mtcars)
#' suggestRemediation(res)
#' @export
suggestRemediation <- function(diagnostic_results) {
  stopifnot(is.list(diagnostic_results))
  p_values <- vapply(diagnostic_results, function(x) {
    if (inherits(x, "htest")) x$p.value else NA_real_
  }, numeric(1))
  sig_tests <- p_values < 0.05
  n_sig <- sum(sig_tests, na.rm = TRUE)

  suggestions <- list()
  if (n_sig == 0) {
    suggestions$conclusion <- "No evidence of heteroscedasticity detected"
    suggestions$action <- "No remediation needed"
    return(structure(suggestions, class = "remediation_suggestions"))
  }

  suggestions$severity <- if (n_sig >= 3) {
    "High"
  } else if (n_sig == 2) {
    "Medium"
  } else {
    "Low"
  }

  if (isTRUE(sig_tests["white"])) {
    suggestions$transformations <- c("log", "sqrt")
  }
  if (isTRUE(sig_tests["breusch_pagan"])) {
    suggestions$variance_modeling <- c(
      "Weighted Least Squares",
      "Robust Standard Errors"
    )
  }

  structure(suggestions, class = "remediation_suggestions")
}

#' Print method for remediation suggestions
#'
#' @param x Object of class \code{remediation_suggestions}.
#' @param ... Not used.
#' @rdname suggestRemediation
#' @export
print.remediation_suggestions <- function(x, ...) {
  cat("Heteroscedasticity Remediation Suggestions\n")
  cat(rep("-", 45), "\n", sep = "")

  if (!is.null(x$conclusion)) {
    cat(x$conclusion, "\n")
    return(invisible(x))
  }

  cat("Severity:", x$severity, "\n")
  if (!is.null(x$transformations)) {
    cat(
      "Recommended transformations:",
      paste(x$transformations, collapse = ", "), "\n"
    )
  }
  if (!is.null(x$variance_modeling)) {
    cat(
      "Variance modelling options:",
      paste(x$variance_modeling, collapse = ", "), "\n"
    )
  }
  invisible(x)
}
