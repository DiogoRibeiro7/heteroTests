\name{performRESETTest}
\alias{performRESETTest}
\title{Ramsey's RESET test for nonlinearity}
\description{Adds powers of the fitted values and performs an F-test to check for neglected nonlinearity.}
\details{On a fit with weights the augmented model is fitted by weighted least squares and both residual sums of squares are weighted. Releases before 0.12.0 refitted the augmented model without the weights, so the two models were not nested.}
\usage{performRESETTest(model, data = NULL, power = 2:3)}
\arguments{\item{model}{A fitted \code{lm} model.}\item{data}{optional data frame the model was fitted to. When \code{NULL}, the default, the model frame of \code{model} is used. The statistic is computed from the response, fitted values and model matrix of \code{model}, so \code{data} does not change the result; it is accepted so that every test takes \code{(model, data, ...)}. Before 0.15.0 the second argument was \code{power}: a number passed there by position is still read as \code{power} in 0.15.x, with a warning that shows the new call, and stops working in 1.0.0.}\item{power}{Numeric vector of powers to include.}}
\value{An object of class \code{htest}.}
\examples{data(mtcars)
m <- lm(mpg ~ wt + qsec, data = mtcars)
performRESETTest(m)}
