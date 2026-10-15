\name{performBoxMTest}
\alias{performBoxMTest}
\title{Box's M Test for Equality of Covariance Matrices}
\description{Test whether multiple groups have equal covariance matrices.}
\details{\code{performBoxMTest(data, group)} is the one test that does not take \code{(model, data, ...)}: it tests the covariance matrices of the data themselves, not the residuals of a fitted model.}
\usage{performBoxMTest(data, group)}
\arguments{
  \item{data}{A numeric data frame or matrix.}
  \item{group}{A factor indicating group membership.}
}
\value{An object of class \code{htest}.}
\examples{
data(iris)
performBoxMTest(iris[,1:4], iris$Species)
}
