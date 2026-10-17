\name{performBPTest}
\alias{performBPTest}
\alias{performBreuschPaganTest}
\title{Perform Breusch-Pagan test for heteroscedasticity}
\description{Implements the classical Breusch-Pagan (1979) test on a fitted linear model, in which the scaled squared residuals are regressed on the original regressors.}
\details{The test statistic is half the explained sum of squares from regressing the scaled squared residuals \eqn{e_i^2/\hat{\sigma}^2 - 1} (with \eqn{\hat{\sigma}^2 = \sum e_i^2 / n}) on the explanatory variables. Under the null hypothesis of homoscedasticity \emph{and normal disturbances} it follows a chi-square distribution with degrees of freedom equal to the number of regressors, matching \code{lmtest::bptest(..., studentize = FALSE)}. For the studentized \eqn{n R^2} form that drops the normality assumption use \code{\link{performKoenkerTest}}.}
\section{Deprecated name}{\code{performBreuschPaganTest()} is deprecated since 0.14.0. It was the same function as \code{performBPTest()} under a second name; it still works, returns what \code{performBPTest()} returns and warns. Use \code{performBPTest()}.}
\section{Weighted fits}{On a fit with weights the test is computed from the Pearson residuals \eqn{\sqrt{w_i}\, e_i}, so it tests whether the weights are adequate; see the section on weighted fits in \code{\link{performKoenkerTest}()}, which also covers weights estimated by \code{\link{fitWLS}()}.}
\usage{performBPTest(model, data = NULL)
performBreuschPaganTest(model, data = NULL)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}. When \code{NULL}, the default, the model frame of \code{model} is used. It holds the terms of the model rather than its variables (\code{log(x)} rather than \code{x}), so a model that transforms a variable needs \code{data}, as does a model with an \code{offset()} term, and so may a fit whose \code{weights} or \code{offset} argument is a column of the data, which the model frame holds as \code{(weights)} or \code{(offset)}.}
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
 performBPTest(m, mtcars)
}
\references{
Breusch, T. S., & Pagan, A. R. (1979). A simple test for heteroscedasticity and random coefficient variation. \emph{Econometrica}, 47(5), 1287--1294. \doi{10.2307/1911963}

Koenker, R. (1981). A note on studentizing a test for heteroscedasticity. \emph{Journal of Econometrics}, 17(1), 107--112. \doi{10.1016/0304-4076(81)90062-2}
}
