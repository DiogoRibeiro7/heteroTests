\name{performWhiteTest}
\alias{performWhiteTest}
\title{Perform White's test for heteroscedasticity}
\description{Implements White's test on a fitted linear model.}
\details{An auxiliary regression of \eqn{e^2} on all regressors, their squares and cross-products produces \eqn{R^2}. The statistic \eqn{n R^2} follows a chi-square distribution with degrees of freedom equal to the number of regressors in the auxiliary model.}
\section{Weighted fits}{On a fit with weights the test is computed from the Pearson residuals \eqn{\sqrt{w_i}\, e_i}, so it tests whether the weights are adequate; see the section on weighted fits in \code{\link{performKoenkerTest}()}, which also covers weights estimated by \code{\link{fitWLS}()}.}
\usage{performWhiteTest(model, data = NULL, cross_products = TRUE, max_interactions = 10)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{Data frame used to fit \code{model}. When \code{NULL}, the default, the model frame of \code{model} is used. It holds the terms of the model rather than its variables (\code{log(x)} rather than \code{x}), so a model that transforms a variable needs \code{data}, as does a model with an \code{offset()} term, and so may a fit whose \code{weights} or \code{offset} argument is a column of the data, which the model frame holds as \code{(weights)} or \code{(offset)}.}
  \item{cross_products}{Logical. Include cross-product terms in the auxiliary regression?}
  \item{max_interactions}{Maximum number of cross-product terms admitted to the auxiliary
    regression. Guards the auxiliary design against growing quadratically with the number
    of regressors.}
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
 performWhiteTest(m, mtcars)
}
\references{
White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator and a direct test for heteroskedasticity. \emph{Econometrica}, 48(4), 817--838. \doi{10.2307/1912934}

Greene, W. H. (2018). \emph{Econometric Analysis} (8th ed.). Pearson.
}
