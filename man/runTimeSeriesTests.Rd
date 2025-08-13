\name{runTimeSeriesTests}
\alias{runTimeSeriesTests}
\title{Run time-series heteroscedasticity tests}
\description{Convenience wrapper for Engle's ARCH LM and McLeod-Li tests.}
\usage{runTimeSeriesTests(model, lags = 1, tests = c("arch_lm", "mcleod_li"))}
\arguments{
  \item{model}{A fitted \code{lm} object.}
  \item{lags}{Number of lags for both tests.}
  \item{tests}{Character vector of test names.}
}
\value{A named list of \code{htest} objects.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, mtcars)
 runTimeSeriesTests(m, lags = 2)
}
\seealso{\code{\link{runPanelTests}}}
