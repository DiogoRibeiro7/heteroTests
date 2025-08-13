\name{runMultivariateTests}
\alias{runMultivariateTests}
\title{Run multivariate heteroscedasticity tests}
\description{Convenience wrapper for multivariate tests such as Box's M.}
\usage{runMultivariateTests(data, group, tests = c("box_m"))}
\arguments{
  \item{data}{Numeric data frame or matrix with multiple variables.}
  \item{group}{Factor defining groups.}
  \item{tests}{Character vector of test names.}
}
\value{A named list of \code{htest} objects.}
\examples{
runMultivariateTests(iris[,1:4], iris$Species)
}

