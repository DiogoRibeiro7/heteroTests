\name{performDavidianCarrollTest}
\alias{performDavidianCarrollTest}
\title{Perform Davidian-Carroll test}
\description{Regresses \code{log(residual^2)} on fitted values using a polynomial.}
\details{A polynomial in the fitted values is fitted to \eqn{\log(e^2)}. Joint significance of the polynomial terms is evaluated with an \eqn{F} statistic.}
\usage{performDavidianCarrollTest(model, degree = 2)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{degree}{polynomial degree.}
}
\value{An object of class \code{htest}.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performDavidianCarrollTest(m)
}
\references{Davidian, M., & Carroll, R. J. (1987). Variance function estimation. \emph{Journal of the American Statistical Association}, 82(400), 1079--1091.}
