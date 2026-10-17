\name{performLeveneTest}
\alias{performLeveneTest}
\title{Perform Levene's test for equality of variances}
\description{Tests equality of variances across groups using residuals from a linear model.}
\details{Absolute deviations from group means are analyzed via a one-way ANOVA. The resulting \eqn{F} statistic has \eqn{k-1} and \eqn{N-k} degrees of freedom under the null of equal variances.}
\usage{performLeveneTest(model, data = NULL, group)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}. When \code{NULL}, the default, the model frame of \code{model} is used, so \code{group} must then be a variable of the model. The model frame holds the terms of the model rather than its variables (\code{log(x)} rather than \code{x}), so a model that transforms a variable needs \code{data}, as does a model with an \code{offset()} term.}
  \item{group}{name of the grouping variable.}
}
\value{An object of class \code{htest} containing the F statistic, p-value and degrees of freedom.}
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
 mtcars$cyl <- factor(mtcars$cyl)
 m <- lm(mpg ~ wt, data = mtcars)
 performLeveneTest(m, mtcars, "cyl")
}
\references{
Levene, H. (1960). Robust tests for equality of variances. In \emph{Contributions to Probability and Statistics} (pp. 278--292). Stanford University Press.

Brown, M. B., & Forsythe, A. B. (1974). Robust tests for the equality of variances. \emph{Journal of the American Statistical Association}, 69(346), 364--367. \doi{10.1080/01621459.1974.10482955}
}
