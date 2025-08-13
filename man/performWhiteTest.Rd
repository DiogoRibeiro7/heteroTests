\name{performWhiteTest}
\alias{performWhiteTest}
\title{Perform White's test for heteroscedasticity}
\description{Implements White's test on a fitted linear model.}
\details{An auxiliary regression of \eqn{e^2} on all regressors, their squares and cross-products produces \eqn{R^2}. The statistic \eqn{n R^2} follows a chi-square distribution with degrees of freedom equal to the number of regressors in the auxiliary model.}
\usage{performWhiteTest(model, data, cross_products = TRUE)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{Data frame used to fit \code{model}.}
  \item{cross_products}{Logical. Include cross-product terms in the auxiliary regression?}
}
\value{An object of class \code{htest} containing the test statistic, p-value and degrees of freedom.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performWhiteTest(m, mtcars)
}
\references{White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator and a direct test for heteroskedasticity. \emph{Econometrica}, 48(4), 817--838.}
