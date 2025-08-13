\name{performMcLeodLiTest}
\alias{performMcLeodLiTest}
\title{Perform McLeod-Li test}
\description{Applies the Ljung-Box test to squared residuals.}
\details{Serial correlation in \eqn{e_t^2} indicates conditional heteroscedasticity. The Ljung-Box statistic is compared to a chi-square distribution with the chosen number of lags.}
\usage{performMcLeodLiTest(model, lags = 10)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{lags}{number of lags for the Ljung-Box test.}
}
\value{An object of class \code{htest} containing the test statistic, p-value and degrees of freedom.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performMcLeodLiTest(m, lags = 10)
}
\references{McLeod, A. I., & Li, W. K. (1983). Diagnostic checking ARMA time series models using squared-residual autocorrelations. \emph{Journal of Time Series Analysis}, 4(4), 269--273.}
