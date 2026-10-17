\name{performBPRandomEffectsTest}
\alias{performBPRandomEffectsTest}
\title{Breusch-Pagan LM test for random effects}
\description{Lagrange Multiplier test for the presence of a random individual effect in panel data. This is not a test for heteroscedasticity and does not respond to one; use the auxiliary-regression diagnostics for that.}
\details{The statistic is Breusch and Pagan (1980) equation 5, \eqn{LM = nT / (2(T-1)) [ \sum_i (\sum_t e_{it})^2 / \sum_{it} e_{it}^2 - 1 ]^2}, which follows a chi-square distribution with one degree of freedom under the null of no individual effect. The bracketed ratio is close to one under the null and the statistic measures its squared departure from one. Before 0.11.0 the "- 1" and the square were absent and the scaling used T^2 rather than nT, which left the statistic sitting at the critical value: it rejected about a third of the time when no individual effect was present.}
\usage{performBPRandomEffectsTest(model, data = NULL, id)}
\arguments{
  \item{model}{an object of class \code{lm}.}
  \item{data}{data frame used to fit \code{model}. When \code{NULL}, the default, the model frame of \code{model} is used, so \code{id} must then be a variable of the model. The model frame holds the terms of the model rather than its variables (\code{log(x)} rather than \code{x}), so a model that transforms a variable needs \code{data}, as does a model with an \code{offset()} term.}
  \item{id}{individual identifier column.}
}
\value{An object of class \code{htest}.}
\section{Supported models}{
It accepts an \code{lm} and a \code{glm} with the Gaussian family and the identity link, unweighted
or with positive weights, a \code{fitWLS()} fit and, since 0.16.0, a rank-deficient fit, which it
tests on the estimable part of its design: it gives the value of the same model fitted without the
aliased columns. It refuses a \code{glm} of another family or link, a \code{survey::svyglm()} fit, a
fit with a zero weight, a coefficient that is not finite for another reason, and \code{data} that
lack rows or variables of the fit. The model is a pooled \code{lm} of the panel, with the units and
the periods as columns of \code{data}; a \code{plm::plm()} fit is not an \code{lm} and is refused.
Which model each test accepts, and why, is set out in \code{\link{test_results}}, under
\emph{Supported models}.
}
\examples{
 df <- data.frame(id = rep(1:5, each = 4), time = rep(1:4, 5), x = runif(20), y = rnorm(20))
 m <- lm(y ~ x, data = df)
 performBPRandomEffectsTest(m, df, "id")
}
\references{Breusch, T. S., & Pagan, A. R. (1980). The Lagrange Multiplier Test and Its Applications to Model Specification in Econometrics. \emph{Review of Economic Studies}, 47(1), 239--253.}
