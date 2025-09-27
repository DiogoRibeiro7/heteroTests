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
  data embedded within the fitted model when available.}
  \item{...}{Passed through for compatibility.}
}
\value{
A data frame summarising the diagnostic(s), or an augmented data set containing
fitted values and residuals alongside the original predictors.
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
