\name{performMcLeodLiTest}
\alias{performMcLeodLiTest}
\title{Perform McLeod-Li test}
\description{Applies the Ljung-Box test to squared residuals.}
\details{Serial correlation in \eqn{e_t^2} indicates conditional heteroscedasticity. The Ljung-Box statistic is compared to a chi-square distribution with the chosen number of lags.}
\usage{performMcLeodLiTest(model, data = NULL, lags = 10)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{optional data frame the model was fitted to. When \code{NULL}, the default, the model frame of \code{model} is used. The statistic is computed from the residuals of \code{model}, so \code{data} does not change the result; it is accepted so that every test takes \code{(model, data, ...)}. Before 0.15.0 the second argument was \code{lags}: a number passed there by position is still read as \code{lags}, with a warning that shows the new call.}
  \item{lags}{number of lags for the Ljung-Box test.}
}
\value{An object of class \code{htest} containing the test statistic, p-value and degrees of freedom.}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performMcLeodLiTest(m, lags = 10)
}
\references{McLeod, A. I., & Li, W. K. (1983). Diagnostic checking ARMA time series models using squared-residual autocorrelations. \emph{Journal of Time Series Analysis}, 4(4), 269--273.}
