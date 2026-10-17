\name{performArchLMTest}
\alias{performArchLMTest}
\title{Perform Engle's ARCH LM test}
\description{Regresses squared residuals on their lags to detect ARCH effects.}
\details{The statistic is \eqn{n R^2} from an auxiliary regression of \eqn{e_t^2} on its lagged values, where \eqn{R^2} is the coefficient of determination. Under the null of no ARCH effects it follows a chi-square distribution with degrees of freedom equal to the number of lags.}
\usage{performArchLMTest(model, data = NULL, lags = 1)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{optional data frame the model was fitted to. When \code{NULL}, the default, the model frame of \code{model} is used. The statistic is computed from the residuals of \code{model}, so \code{data} does not change the result; it is accepted so that every test takes \code{(model, data, ...)}. Before 0.15.0 the second argument was \code{lags}: a number passed there by position is still read as \code{lags}, with a warning that shows the new call; that form stops working in 1.0.0.}
  \item{lags}{number of lags to include.}
}
\value{An object of class \code{htest} containing the test statistic, p-value and degrees of freedom.}
\section{Supported models}{
It accepts an \code{lm} and a \code{glm} with the Gaussian family and the identity link, unweighted
or with positive weights, a \code{fitWLS()} fit and, since 0.16.0, a rank-deficient fit, which it
tests on the estimable part of its design: it gives the value of the same model fitted without the
aliased columns. It refuses a \code{glm} of another family or link, a \code{survey::svyglm()} fit, a
fit with a zero weight, a coefficient that is not finite for another reason, and \code{data} that
lack rows or variables of the fit. Which model each test accepts, and why, is set out in
\code{\link{test_results}}, under \emph{Supported models}.
}
\examples{
 data(mtcars)
 m <- lm(mpg ~ wt + qsec, data = mtcars)
 performArchLMTest(m, lags = 2)
}
\references{
Engle, R. F. (1982). Autoregressive conditional heteroskedasticity with estimates of the variance of United Kingdom inflation. \emph{Econometrica}, 50(4), 987--1007. \doi{10.2307/1912773}

Hamilton, J. D. (1994). \emph{Time Series Analysis}. Princeton University Press.
}
