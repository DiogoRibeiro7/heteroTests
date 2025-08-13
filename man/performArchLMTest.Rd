\name{performArchLMTest}
\alias{performArchLMTest}
\title{Perform Engle's ARCH LM test}
\description{Regresses squared residuals on their lags to detect ARCH effects.}
\details{The statistic is \eqn{n R^2} from an auxiliary regression of \eqn{e_t^2} on its lagged values, where \eqn{R^2} is the coefficient of determination. Under the null of no ARCH effects it follows a chi-square distribution with degrees of freedom equal to the number of lags.}
\usage{performArchLMTest(model, lags = 1)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{lags}{number of lags to include.}
}
\value{An object of class \code{htest} containing the test statistic, p-value and degrees of freedom.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performArchLMTest(m, lags = 2)
}
\references{Engle, R. F. (1982). Autoregressive Conditional Heteroskedasticity with Estimates of the Variance of United Kingdom Inflation. \emph{Econometrica}, 50, 987--1007.}
