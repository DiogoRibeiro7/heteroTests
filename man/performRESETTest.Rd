\name{performRESETTest}
\alias{performRESETTest}
\title{Ramsey's RESET test for nonlinearity}
\description{Adds powers of the fitted values and performs an F-test to check for neglected nonlinearity.}
\usage{performRESETTest(model, power = 2:3)}
\arguments{\item{model}{A fitted \code{lm} model.}\item{power}{Numeric vector of powers to include.}}
\value{An object of class \code{htest}.}
\examples{data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
performRESETTest(m)}
