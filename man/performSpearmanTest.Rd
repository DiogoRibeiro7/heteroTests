\name{performSpearmanTest}
\alias{performSpearmanTest}
\title{Perform Spearman rank correlation test}
\description{Computes Spearman's rho between absolute residuals and fitted values.}
\details{The squared rank correlation coefficient times the sample size minus 1 approximates a chi-square distribution with one degree of freedom.}
\usage{performSpearmanTest(model, data = NULL)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{optional data frame the model was fitted to. When \code{NULL}, the default, the model frame of \code{model} is used. The statistic is computed from the residuals and fitted values of \code{model}, so \code{data} does not change the result; it is accepted so that every test takes \code{(model, data, ...)}.}
}
\value{An object of class \code{htest} containing the test statistic, p-value, degrees of freedom and rho.}
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
 performSpearmanTest(m)
}
\references{Spearman, C. (1904). The proof and measurement of association between two things. \emph{American Journal of Psychology}, 15(1), 72--101.}
