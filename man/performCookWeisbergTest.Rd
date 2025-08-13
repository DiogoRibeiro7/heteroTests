\name{performCookWeisbergTest}
\alias{performCookWeisbergTest}
\title{Perform Cook-Weisberg test for heteroscedasticity}
\description{Implementation of the Breusch-Pagan test using fitted values.}
\details{Squared residuals are regressed on the model's fitted values. The statistic \eqn{n R^2} follows a chi-square distribution with one degree of freedom under homoscedasticity.}
\usage{performCookWeisbergTest(model)}
\arguments{
  \item{model}{an object of class \code{lm}.}
}
\value{An object of class \code{htest}.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performCookWeisbergTest(m)
}
\references{Cook, R. D., & Weisberg, S. (1983). Diagnostics for heteroscedasticity in regression. \emph{Biometrika}, 70(1), 1--10.}
