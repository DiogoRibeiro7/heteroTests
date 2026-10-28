% Generated manually
\name{tidy.hetero_test}
\alias{tidy.hetero_test}
\alias{glance.hetero_test}
\alias{augment.hetero_test}
\alias{tidy.hetero_test_suite}
\alias{glance.hetero_test_suite}
\alias{augment.hetero_test_suite}
\alias{tidy.hetero_grouped_suite}
\title{broom methods for heteroscedasticity diagnostics}
\description{
Tidy, glance and augment methods integrating heteroscedasticity diagnostics with
the broom generics from the \pkg{generics} package.
}
\usage{
\method{tidy}{hetero_test}(x, ...)
\method{glance}{hetero_test}(x, ...)
\method{augment}{hetero_test}(x, data = NULL, ...)
\method{tidy}{hetero_test_suite}(x, ...)
\method{glance}{hetero_test_suite}(x, ...)
\method{augment}{hetero_test_suite}(x, data = NULL, ...)
\method{tidy}{hetero_grouped_suite}(x, ...)
}
\arguments{
  \item{x}{Heteroscedasticity diagnostic result(s) returned by
  \code{runHeteroTests}.}
  \item{data}{Optional data frame used to augment diagnostics; defaults to the
  data embedded within the fitted model when available. Data with one row per
  observation of the fit are matched to the observations as the tests match
  them, by row name in any order, or for data that only number their rows in
  the order of the fit, and the result holds the rows of the fit in its order;
  data whose rows cannot be shown to hold the observations are refused (see
  \code{\link{test_results}}). Before 0.17.0 they were bound row by row, so
  named data in another order than the fit had the fitted values of other rows
  bound to them. On a fit with \code{na.action = na.exclude} the rows of the
  observations are padded with a row of \code{NA} at each row the fit dropped;
  the full data of such a fit are bound row by row, and those of a fit with
  \code{na.omit} are refused.}
  \item{...}{Passed through for compatibility.}
}
\value{
A data frame summarising the diagnostic(s), or an augmented data set containing
fitted values and residuals alongside the original predictors.

\code{tidy} returns one row per test. \code{parameter} is the first parameter
of the test and \code{parameter2} the second, for a test that has two (the
denominator degrees of freedom of an F statistic, or the number of bootstrap
replications); it is \code{NA} otherwise. \code{estimate} is given when the
test reports exactly one estimate. Up to 0.12.0 a test with two parameters was
returned as two rows.
}
\seealso{\code{generics::tidy}, \code{generics::glance}, \code{generics::augment}}
\examples{
if (requireNamespace("generics", quietly = TRUE)) {
  data(mtcars)
  fit <- lm(mpg ~ wt + qsec, data = mtcars)
  suite <- runHeteroTests(fit, mtcars, tests = c("white", "breusch_pagan"))
  generics::tidy(suite)
}
}
