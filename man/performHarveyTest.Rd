\name{performHarveyTest}
\alias{performHarveyTest}
\title{Perform Harvey test for heteroscedasticity}
\description{Regresses \code{log(residuals^2)} on fitted values and their squares.}
\details{Under multiplicative heteroscedasticity the log variance is linear in the fitted values. The \eqn{F} statistic from this auxiliary regression tests the joint significance of the terms.}
\usage{performHarveyTest(model)}
\arguments{
  \item{model}{an object of class \code{lm}.}
}
\value{An object of class \code{htest} containing the F statistic, p-value and degrees of freedom.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performHarveyTest(m)
}
\references{
Harvey, A. C. (1976). Estimating regression models with multiplicative heteroscedasticity. \emph{Econometrica}, 44(3), 461--465. \doi{10.2307/1913974}

Cook, R. D., \& Weisberg, S. (1983). Diagnostics for heteroscedasticity in regression. \emph{Biometrika}, 70(1), 1--10. \doi{10.1093/biomet/70.1.1}
}
