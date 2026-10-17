\name{performBartlettTest}
\alias{performBartlettTest}
\title{Perform Bartlett's test for equality of variances}
\description{Uses \code{bartlett.test} on model residuals grouped by a factor.}
\details{The test statistic compares the pooled variance to the individual group variances. It is computed as \eqn{\chi^2 = (N - k) \ln S_p^2 - \sum (n_i - 1) \ln s_i^2}, where \eqn{S_p^2} is the pooled variance and \eqn{s_i^2} the group variances. Under the null it approximates a chi-square distribution with \eqn{k-1} degrees of freedom.}
\usage{performBartlettTest(model, data = NULL, group)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}. When \code{NULL}, the default, the model frame of \code{model} is used, so \code{group} must then be a variable of the model. The model frame holds the terms of the model rather than its variables (\code{log(x)} rather than \code{x}), so a model that transforms a variable needs \code{data}, as does a model with an \code{offset()} term.}
  \item{group}{name of the grouping variable.}
}
\value{An object of class \code{htest} containing the chi-squared statistic, p-value and degrees of freedom.}
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
 performBartlettTest(m, mtcars, "cyl")
}
\references{
Bartlett, M. S. (1937). Properties of sufficiency and statistical tests. \emph{Proceedings of the Royal Society of London}, 160(901), 268--282. \doi{10.1098/rspa.1937.0109}

Hartley, H. O. (1950). The maximum F-ratio as a short-cut test for heterogeneity of variance. \emph{Biometrika}, 37(3/4), 308--312. \doi{10.2307/2332383}
}
