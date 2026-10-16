\name{performHartleyFmaxTest}
\alias{performHartleyFmaxTest}
\title{Perform Hartley's Fmax test}
\description{Compares the maximum and minimum group residual variances using Hartley's maximum F-ratio distribution.}
\usage{performHartleyFmaxTest(model, data = NULL, group)}
\arguments{
  \item{model}{A fitted \code{lm} object.}
  \item{data}{Data frame used to fit \code{model}. When \code{NULL}, the default, the model frame of \code{model} is used, so \code{group} must then be a variable of the model. The model frame holds the terms of the model rather than its variables (\code{log(x)} rather than \code{x}), so a model that transforms a variable needs \code{data}, as does a model with an \code{offset()} term.}
  \item{group}{Name of the grouping variable.}
}
\details{The statistic is \eqn{F_{max}=\max_j s_j^2/\min_j s_j^2}. Under normality and equal group sizes its null distribution is the maximum F-ratio distribution for \eqn{k} independent mean squares with common degrees of freedom, evaluated with \code{SuppDists::pmaxFratio()}. If group sizes differ, the function warns and rounds the mean group size, then subtracts one, to obtain an approximate common integer degrees of freedom.}
\value{An object of class \code{htest} containing the Fmax statistic, number of groups, reference degrees of freedom and p-value.}
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
 set.seed(1701)
 n <- 20
 d <- data.frame(g = factor(rep(letters[1:3], each = n)), x = rnorm(3 * n))
 d$y <- 1 + d$x + rnorm(3 * n)
 m <- lm(y ~ x, data = d)
 performHartleyFmaxTest(m, d, "g")
}
\references{Hartley, H. O. (1950). The maximum F-ratio as a short-cut test for heterogeneity of variance. \emph{Biometrika}, 37(3/4), 308--312.}
