% Generated manually
\name{runSurveyHeteroTests}
\alias{runSurveyHeteroTests}
\title{Run heteroscedasticity diagnostics on survey designs}
\description{
Fits a survey-weighted linear model with \code{survey::svyglm()} and forwards the
result to \code{\link{runHeteroTests}} so standard diagnostics can be reused
with complex survey data.
}
\usage{
runSurveyHeteroTests(formula, design, tests = c("white", "breusch_pagan"), ...)
}
\arguments{
  \item{formula}{Model formula specifying the survey-weighted mean structure.}
  \item{design}{A \code{survey::survey.design} object describing weights (and
  optionally strata or clusters).}
  \item{tests}{Diagnostic names passed on to \code{runHeteroTests}.}
  \item{...}{Additional arguments forwarded to \code{runHeteroTests}.}
}
\value{
An object of class \code{hetero_test_suite} (or \code{hetero_grouped_suite} when
grouped survey data are analysed).
}
\seealso{\code{\link{runHeteroTests}}, \code{survey::svyglm}}
\examples{
\donttest{
if (requireNamespace("survey", quietly = TRUE)) {
  data(api, package = "survey")
  design <- survey::svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat)
  res <- runSurveyHeteroTests(api00 ~ api99 + ell, design)
  generics::tidy(res)
}
}
}
