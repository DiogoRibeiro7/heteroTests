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
  data embedded within the fitted model when available. The rows of \code{data}
  are matched to the observations of the fit as the tests match them, by row
  name in any order, or for data that only number their rows by the row
  numbers of the fit or in its order, and bound as broom binds them, in the
  order of \code{data}, each row with its own fitted value and residual: on a
  fit without \code{na.action = na.exclude} the rows the fit used, so the full
  data of a fit that dropped rows give those rows; on a fit with
  \code{na.exclude} those rows and the rows it dropped, with \code{NA} there,
  every row of its full data. Given the rows of its observations only, as its
  model frame without \code{data}, a fit with \code{na.exclude} binds them in
  the order of the fit with a row of \code{NA} at each row it dropped. Data
  whose rows cannot be shown to hold the observations are refused (see
  \code{\link{test_results}}). The data of a tidymodels workflow, or of a
  parsnip fit of \code{fit_xy()}, are mapped to the variables of its engine as
  \code{\link{runHeteroTests}()} maps them, and each mapped row is traced to
  the row of \code{data} it was made from: the data of a preprocessing that
  drops rows of new data, such as \code{step_filter()} with
  \code{skip = FALSE}, or reorders them, such as \code{step_arrange()}, are
  refused, since it does not say which row of \code{data} each observation
  comes from. Before 0.17.0 the rows were bound
  row by row: named data in another order than the fit had the values of
  other observations bound to them, and the full data of a fit with
  \code{na.omit} could not be bound.}
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
